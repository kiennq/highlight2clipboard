;;; multiclip.el --- Copy text to clipboard with highlighting.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Anders Lindgren

;; Author: Anders Lindgren
;; Maintainer: Kien Nguyen
;; Version: 0.5
;; Created: 2015-06-17
;; Package-Requires: ((emacs "26.1") (htmlize "1.47") (jsonrpc "1.0.7"))
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for copying text with formatting information, like color,
;; to the system clipboard.  Concretely, this allows you to paste
;; syntax highlighted source code into word processors and mail
;; editors.
;; Also, this package acts as clipboard provider, completely short-circut
;; `Emacs' system clipboard call.

;;; Code:


(require 'htmlize)
(require 'jsonrpc)
(require 'pcase)
(require 'url)

(defgroup multiclip nil
  "Support for exporting formatted text to the clipboard."
  :group 'faces)

(defcustom multiclip-log-size 0
  "Maximum size for logging jsonrpc event.  0 disables, nil means infinite."
  :type 'integer)

(defcustom multiclip-host "127.0.0.1"
  "Host of csclip server."
  :type 'string)

(defcustom multiclip-port "9123"
  "Port of csclip server."
  :type 'string)

(defcustom multiclip-enable-clipboard-history t
  "Enable clipboard history."
  :type 'boolean)

(defcustom multiclip-bin (expand-file-name ".cache/multiclip/csclip.exe"
                                           user-emacs-directory)
  "Path to `multiclip' compatible server binary."
  :type 'file)

(defcustom multiclip-assets-subpath "assets"
  "Directory subpath of where blob assets will be saved.
The subpath is from the current folder."
  :type 'string)

(defvar multiclip--original-interprogram-cut-function
  interprogram-cut-function)

(defvar multiclip--original-interprogram-paste-function
  interprogram-paste-function)

(defvar multiclip--content-len 0 "Content length of a message.")

(defvar multiclip-debug nil "Enable debug mode.")

;; Supported jsonrpc method:
;; Sent:
;; - copy: [<text data>] -> nil
;; - get: [<data format>] -> <requested data>
;; Received:
;; - paste: <text data> -> nil
;; - get: <data format> -> <requested data>

(defconst multiclip--format-html "html" "HTML format string.")
(defconst multiclip--format-text "text" "Text format.")
(defconst multiclip--format-bitmap "bitmap" "Bitmap format.")

(defvar multiclip--set-data-to-clipboard-function nil)
(defvar multiclip--get-data-from-clipboard-function nil)

(defconst multiclip--system-type
  (or (and (executable-find "wslpath") 'gnu/wsl) system-type)
  "Extended `system-type' that recognize wsl.")

;; ------------------------------------------------------------
;; Global minor mode
;;


;;;###autoload
(define-minor-mode multiclip-mode
  "When active, cuts and copies are exported with formatting to the clipboard."
  :global t
  :lighter " clipboard"
  :group 'multiclip
  ;; This will issue an error on unsupported systems, preventing our
  ;; hooks to be installed.
  (if multiclip-mode (multiclip--init) (multiclip--exit)))

(defvar multiclip--conn nil "Clipboard jsonrpc connection.")

(defun multiclip--init ()
  "Init multiclip."
  (interactive)
  ;; Set up multiclip, or issue an error if system not supported.
  (cond ((memq multiclip--system-type '(windows-nt cygwin gnu/wsl))
         (setq multiclip--set-data-to-clipboard-function
               #'multiclip--set-data-to-clipboard-w32
               multiclip--get-data-from-clipboard-function
               #'multiclip--get-data-from-clipboard-w32))
        (t (error "Unsupported system: %s" multiclip--system-type)))

  (multiclip-ensure-binary)
  (setq interprogram-cut-function 'multiclip-copy-to-clipboard)
  (setq interprogram-paste-function 'multiclip-paste-from-clipboard)
  (when (memq multiclip--system-type '(windows-nt cygwin gnu/wsl))
    (let ((default-directory (expand-file-name "~")))
      (make-process :name "csclip"
                    :command `(,(file-truename multiclip-bin) "server"
                               "-p" ,multiclip-port)))
    (setq multiclip--conn (jsonrpc-process-connection
                           :process (make-network-process
                                     :name "clipboard"
                                     :buffer "*clipboard*"
                                     :host multiclip-host
                                     :service multiclip-port
                                     :type nil
                                     :coding '(utf-8-unix . no-conversion)
                                     :noquery t
                                     :nowait t)
                           :events-buffer-config `(:size ,multiclip-log-size)
                           :request-dispatcher #'multiclip--handle-request
                           :notification-dispatcher #'multiclip--handle-request))
    (jsonrpc-async-request multiclip--conn
                           'get `[,multiclip--format-text]
                           :success-fn
                           (lambda (result)
                             (multiclip--handle-paste result))
                           :error-fn
                           (lambda (err)
                             (message "Got error: %s" err))
                           :deferred t)))

(defun multiclip--exit ()
  "Kill clipboard connection and clean up."
  (interactive)
  (setq interprogram-cut-function multiclip--original-interprogram-cut-function)
  (setq interprogram-paste-function multiclip--original-interprogram-paste-function)
  (if multiclip--conn (jsonrpc-shutdown multiclip--conn)))

;; ------------------------------------------------------------
;; Core functions.
;;

(defvar multiclip--external-copy nil "External clipboard text, pushed to Emacs.")

(defun multiclip--handle-request (_ method params)
  "Handling request from CONN with METHOD and PARAMS.
This is used for both jsonrpc `notify' and `request'."
  ;; Received:
  ;; - paste: [(:cf :data)] -> nil
  ;; - get: [<data format>] -> <requested data>
  (pcase method
    ('paste
     (multiclip--handle-paste (elt params 0)))
    ('get
     (multiclip--handle-get-data (elt params 0))))
  )

(defvar multiclip--last-paste-is-blob nil "Non-nil if the last paste is a blob.")

(defun multiclip--handle-paste (payload)
  "Paste PAYLOAD into `Emacs' clipboard."
  (cond
   ((equal (plist-get payload :cf) multiclip--format-text)
    (setq multiclip--last-paste-is-blob nil)
    (let* ((text (plist-get payload :data))
           (txt (replace-regexp-in-string "\r" "" text))
           interprogram-cut-function)
      (when (and multiclip-enable-clipboard-history
                 (not (equal txt (car kill-ring))))
        (kill-new txt))
      (setq multiclip--external-copy txt)))
   ((equal (plist-get payload :cf) multiclip--format-bitmap)
    (setq multiclip--last-paste-is-blob t))))

(defun multiclip--handle-get-data (cf)
  "Render format CF to put into clipboard."
  (pcase cf
    ("html" (multiclip--htmlize (car kill-ring)))
    ("text" (car kill-ring))
    (_ "")))

(defun multiclip--request-save-blob (cf)
  "Render format CF to file and save it to subpath of `multiclip-assets-subpath'."
  (when-let ((_ buffer-file-name)
             (resp (jsonrpc-request multiclip--conn
                                    'get-to-file
                                    `[( :cf ,cf
                                        :path ,(expand-file-name multiclip-assets-subpath
                                                                 (file-name-directory buffer-file-name))
                                        :prefix ,(concat (file-name-nondirectory buffer-file-name) "_"))]
                                    :timeout 2
                                    :cancel-on-input t
                                    :cancel-on-input-retval nil)))
    (setq multiclip--external-copy
          (cond
           ((derived-mode-p '(org-mode))
            (format "[[%s]]" (string-join `("." ,multiclip-assets-subpath ,resp) "/")))
           (t
            (format "![%s](%s)" (substring resp 0 8)
                    (string-join `("." ,multiclip-assets-subpath ,resp) "/")))))))

;;;###autoload
(defun multiclip-ensure-buffer-is-fontified ()
  "Ensure that the buffer is fontified."
  (interactive)
  (when (and font-lock-mode
             ;; Prevent clearing out face attributes explicitly
             ;; inserted by functions like `list-faces-display'.
             ;; (Font-lock mode is enabled, for some reason, in those
             ;; buffers.)
             (not (and (eq major-mode 'help-mode)
                       (not font-lock-defaults))))
    (font-lock-ensure)))


;;;###autoload
(defun multiclip-copy-region-to-clipboard (beg end)
  "Copy region (BEG END) with formatting to system clipboard.

Unlike using multiclip mode, this ensure that buffers
are fully fontified."
  (interactive "r")
  (multiclip-ensure-buffer-is-fontified)
  (kill-new (buffer-substring beg end)))


;;;###autoload
(defun multiclip-copy-buffer-to-clipboard ()
  "Copy buffer with formatting to system clipboard.

Unlike using multiclip mode, this ensure that buffers
are fully fontified."
  (interactive)
  (multiclip-copy-region-to-clipboard (point-min) (point-max)))

(defun multiclip--htmlize (text)
  "Htmlize TEXT."
  (save-excursion
      (with-temp-buffer
        (goto-char (point-min))
        (insert text)
        (let* ((htmlize-output-type 'inline-css))
          (with-current-buffer (htmlize-buffer)
            (goto-char (point-min))
            ;; changing <body> tag to <div> and trim region around
            (let ((p (if (re-search-forward "<body")
                         (prog1
                             (match-beginning 0)
                           (replace-match "<div"))
                       (point-min))))
              (delete-region (point-min) p))
            (goto-char (point-max))
            (let ((p (if (re-search-backward "</body>")
                         (prog1
                             (match-end 0)
                           (replace-match "</div>"))
                       (point-max))))
              (delete-region p (point-max)))
            (goto-char (point-min))
            (let ((p (if (re-search-forward "<pre>" nil t)
                         (prog1
                             (match-beginning 0)
                           ;; Remove extra newline.
                           (delete-char 1))
                       (point-min))))
              (goto-char p)
              (insert "<meta charset='utf-8'>"))
            (let ((text (buffer-string))) (kill-buffer) text))))))

(defun multiclip-copy-to-clipboard (text)
  "Copy TEXT with formatting to the system clipboard."
  (condition-case-unless-debug nil
      (when multiclip--set-data-to-clipboard-function
        (funcall multiclip--set-data-to-clipboard-function
                 `[(:cf ,multiclip--format-text :data ,text)
                   (:cf ,multiclip--format-html)]))
    (error
     ;; fall back to original function
     (funcall multiclip--original-interprogram-cut-function text)))
  ;; Set the external clipboard string(s).
  (setq multiclip--external-copy text))

(defun multiclip-paste-from-clipboard ()
  "Paste from system clipboard."
  (when multiclip--last-paste-is-blob
    (multiclip--request-save-blob multiclip--format-bitmap)
    (setq multiclip--last-paste-is-blob nil))
  (funcall multiclip--get-data-from-clipboard-function))

;; ------------------------------------------------------------
;; System-specific support.
;;

(defun multiclip--normalize-data (data)
  "DATA is a list of (format . text).  Convert to [{cf:format, data:text}] json."
  (apply #'vector (mapcar (lambda (x) `(:cf ,(car x) :data ,(cdr x))) data)))

(defun multiclip--set-data-to-clipboard-w32 (data)
  "DATA is a list of (format . text)."
  (jsonrpc-notify multiclip--conn 'copy `[,data]))

(defun multiclip--get-data-from-clipboard-w32 ()
  "Get data from clipboard, need to check internal state before set."
  (unless (equal multiclip--external-copy (car kill-ring))
    multiclip--external-copy))

;; Utils
(declare-function tar-untar-buffer "ext:tar-mode")

(defun multiclip-ensure-binary (&optional forced)
  "Ensure the server binary is installed.
Will redownload if FORCED."
  (interactive "P")
  (unless (and (not forced)
               (executable-find multiclip-bin))
    (let* ((url "https://github.com/kiennq/csclip/releases/latest/download/csclip.tar.zst")
           (exec-path (append exec-path `(,(expand-file-name
                                            (concat exec-directory "../../../../bin")))))
           (default-directory (file-name-directory multiclip-bin))
           (bin-file "csclip.tar.zst"))
      (unless (file-directory-p default-directory)
        (make-directory default-directory 'parents))
      (url-copy-file url bin-file 'ok-if-already-exists)
      (require 'tar-mode)
      (with-temp-buffer
        (insert-file-contents bin-file)
        (tar-mode)
        (tar-untar-buffer)))))

(provide 'multiclip)

;;; multiclip.el ends here
