;;; multiclip.el --- Copy text to clipboard with highlighting.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.2
;; Created: 2015-06-17
;; Package-Requires: ((emacs "26.1") (htmlize "1.47") (jsonrpc "1.0.7") (s))
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
;; `Emacs'' system clipboard call.
;;
;; Usage:
;;
;; * `M-x multiclip-copy-region-to-clipboard RET' -- Copy
;;   the region, with formatting, to the clipboard.
;;
;; * `M-x multiclip-copy-buffer-to-clipboard RET' -- Copy
;;   the buffer, with formatting, to the clipboard.
;;
;; * multiclip mode -- Global minor mode, when enabled, all
;;   copies and cuts are exported, with formatting information, to the
;;   clipboard.
;;
;; Supported systems:
;;
;; Copying formatted text to the clipboard is highly system specific.
;; Currently, MS-Windows are supported.  Contributions for
;; other systems are most welcome.
;;
;; Known problems:
;;
;; Font Lock mode, the system providing syntax highlighting in Emacs,
;; use "lazy highlighting".  Effectively, this mean that only the
;; visible parts of a buffer are highlighted.  The problem with this is
;; that when copying text to the clipboard, only the highlighted parts
;; gets formatting information.  To get around this, walk through the
;; buffer, use `multiclip-ensure-buffer-is-fontified', or
;; use one of the `multiclip-copy-' functions.
;;
;; Implementation:
;;
;; This package use the package `htmlize' to create an HTML version of
;; a highlighted text.  This is added as a new flavor to the clipboard,
;; allowing an application to pick the most suited version.
;; Additional to that, clipboard's changes are now monitored and will be
;; reflect to `Emacs' before hand, clipboard delay rendering also supported.
;; Copy & Paste and be pleasingly fast.

;;; Code:


(require 'htmlize)
(require 'jsonrpc)
(require 'pcase)
(require 's)

(defgroup multiclip nil
  "Support for exporting formatted text to the clipboard."
  :group 'faces)

(defcustom multiclip-log-size 0
  "Maximum size for logging jsonrpc event. 0 disables, nil means infinite."
  :group 'multiclip
  :type 'integer)

(defvar multiclip--original-interprocess-cut-function
  interprogram-cut-function)

(defvar multiclip--original-interprocess-paste-function
  interprogram-paste-function)

(defvar multiclip--content-len 0 "Content length of a message.")

(defvar multiclip-debug nil "Enable debug mode.")

(defconst multiclip--directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

;; Supported jsonrpc method:
;; Sent:
;; - copy: [<text data>] -> nil
;; - get: [<data format>] -> <requested data>
;; Received:
;; - paste: <text data> -> nil
;; - get: <data format> -> <requested data>

(defconst multiclip--format-html "html" "HTML format string.")
(defconst multiclip--format-text "text" "Text format.")

;; ------------------------------------------------------------
;; Global minor mode
;;

;;;###autoload
(define-minor-mode multiclip-mode
  "When active, cuts and copies are exported with formatting to the clipboard."
  nil
  nil
  nil
  :global t
  :lighter " clipboard"
  :group 'multiclip
  ;; This will issue an error on unsupported systems, preventing our
  ;; hooks to be installed.
  (multiclip-set-defaults)
  (if multiclip-mode (multiclip-init) (multiclip-exit)))

(defvar multiclip--proc nil "Server clipboard process.")
(defvar multiclip--conn nil "Clipboard jsonrpc connection.")

(defun multiclip-init ()
  "Init multiclip."
  (interactive)
  (setq interprogram-cut-function 'multiclip-copy-to-clipboard)
  (setq interprogram-paste-function 'multiclip-paste-from-clipboard)
  (setq multiclip--proc (make-process
                         :name "clipboard"
                         :buffer "*clipboard*"
                         :command
                         `(,(file-truename (concat multiclip--directory "bin/csclip.exe"))
                           "server")
                         :connection-type 'pipe
                         :coding '(utf-8-unix . no-conversion)
                         :noquery t))
  (setq multiclip--conn (jsonrpc-process-connection
                         :process multiclip--proc
                         :events-buffer-scrollback-size multiclip-log-size
                         :request-dispatcher #'multiclip--handle-request
                         :notification-dispatcher #'multiclip--handle-request))
  (jsonrpc-async-request multiclip--conn
                         'get `(,multiclip--format-text)
                         :success-fn
                         (lambda (result)
                           (multiclip--handle-paste result))
                         :error-fn
                         (lambda (err)
                           (message "Got error: %s" err)))
  )

(defun multiclip-exit ()
  "Kill clipboard process and clean up."
  (interactive)
  (setq interprogram-cut-function multiclip--original-interprocess-cut-function)
  (setq interprogram-paste-function multiclip--original-interprocess-paste-function)
  (if multiclip--conn (jsonrpc-shutdown multiclip--conn))
  )

;; ------------------------------------------------------------
;; Core functions.
;;

(defvar multiclip--last-copy nil "Last copied text.")
(defvar multiclip--external-copy nil "External clipboard text, pushed to Emacs.")

(defun multiclip--handle-request (conn method params)
  "Handling request from CONN with METHOD and PARAMS.
This is used for both jsonrpc `notify' and `request'."
  ;; Received:
  ;; - paste: [<text data>] -> nil
  ;; - get: [<data format>] -> <requested data>
  (pcase method
    ('paste
     (multiclip--handle-paste (elt params 0)))
    ('get
     (multiclip--handle-get-data (elt params 0))))
  )

(defun multiclip--handle-paste (text)
  "Paste text into `Emacs' clipboard."
  (setq multiclip--external-copy
        (if (s-contains-p "-dos" (symbol-name buffer-file-coding-system))
            text
          (replace-regexp-in-string
           "" ""
           text))))

(defun multiclip--handle-get-data (cf)
  "Render format CF to put into clipboard."
  (pcase cf
    ("html" (multiclip--htmlize multiclip--last-copy))
    ("text" multiclip--last-copy)
    (_ ""))
  )


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
    (font-lock-fontify-region (point-min) (point-max))))


;;;###autoload
(defun multiclip-copy-region-to-clipboard (beg end)
  "Copy region (BEG END) with formatting to system clipboard.

Unlike using multiclip mode, this ensure that buffers
are fully fontified."
  (interactive "r")
  (multiclip-ensure-buffer-is-fontified)
  (multiclip-copy-to-clipboard (buffer-substring beg end)))


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
  (prog1
      ;; Set the normal clipboard string(s).
      ;; (funcall multiclip--original-interprocess-cut-function text)
      (setq multiclip--last-copy text)
      (setq multiclip--external-copy text)
    ;; Add addition flavor(s)
      (when multiclip--set-data-to-clipboard-function
        (funcall multiclip--set-data-to-clipboard-function
                 `((,multiclip--format-text . ,text)
                   (,multiclip--format-html))))
))

(defun multiclip-paste-from-clipboard ()
  "Paste from system clipboard."
  (funcall multiclip--get-data-from-clipboard-function))
;; ------------------------------------------------------------
;; System-specific support.
;;

(defvar multiclip--set-data-to-clipboard-function nil)
(defvar multiclip--get-data-from-clipboard-function nil)

(if (and (executable-find "uname")
           (string-match
            "Microsoft"
            (shell-command-to-string "uname -r")))
      (setq system-type 'wsl))

(defun multiclip-set-defaults ()
  "Set up multiclip, or issue an error if system not supported."

  (cond ((eq system-type 'darwin)
         (setq multiclip--set-data-to-clipboard-function
               #'multiclip--set-data-to-clipboard-osx)
         (setq multiclip--get-data-from-clipboard-function
                 #'multiclip--get-data-from-clipboard-osx))
        ((memq system-type '(windows-nt cygwin wsl))
         (setq multiclip--set-data-to-clipboard-function
               #'multiclip--set-data-to-clipboard-w32)
         (setq multiclip--get-data-from-clipboard-function
                 #'multiclip--get-data-from-clipboard-w32))
        (t (error "Unsupported system: %s" system-type))))

(defun multiclip--set-data-to-clipboard-osx (data)
  ;; (call-process
  ;;  "python"
  ;;  nil
  ;;  0                                  ; <- Discard and don't wait
  ;;  nil
  ;;  (concat multiclip--directory
  ;;          "bin/multiclip-osx.py")
  ;;  file-name)
  )

(defun multiclip--get-data-from-clipboard-osx ()
  )

(defun multiclip--normalize-data (data)
  "DATA is a list of (format . text).  Convert to [{cf:format, data:text}] json."
  (mapcar (lambda (x) `((cf . ,(car x)) (data . ,(cdr x)))) data)
  )

(defun multiclip--set-data-to-clipboard-w32 (data)
  "DATA is a list of (format . text)."
  (jsonrpc-notify multiclip--conn
                  'copy `(,(multiclip--normalize-data data))))

(defun multiclip--get-data-from-clipboard-w32 ()
  (unless (string= multiclip--external-copy multiclip--last-copy)
    multiclip--external-copy))

(provide 'multiclip)

;;; multiclip.el ends here
