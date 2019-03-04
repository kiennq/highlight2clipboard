;;; multiclip.el --- Copy text to clipboard with highlighting.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.2
;; Created: 2015-06-17
;; Package-Requires: ((htmlize "1.47"))
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
;; to the system clipboard. Concretely, this allows you to paste
;; syntax highlighted source code into word processors and mail
;; editors.
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
;; Currently, Mac OS X and MS-Windows are supported. Contributions for
;; other systems are most welcome.
;;
;; Known problems:
;;
;; Font Lock mode, the system providing syntax highlighting in Emacs,
;; use "lazy highlighting". Effectively, this mean that only the
;; visible parts of a buffer are highlighted. The problem with this is
;; that when copying text to the clipboard, only the highlighted parts
;; gets formatting information. To get around this, walk through the
;; buffer, use `multiclip-ensure-buffer-is-fontified', or
;; use one of the `multiclip-copy-' functions.
;;
;; Implementation:
;;
;; This package use the package `htmlize' to create an HTML version of
;; a highlighted text. This is added as a new flavor to the clipboard,
;; allowing an application to pick the most suited version.

;;; Code:


(require 'htmlize)
(require 'json)

(defgroup multiclip nil
  "Support for exporting formatted text to the clipboard."
  :group 'faces)

(defvar multiclip--original-interprocess-cut-function
  interprogram-cut-function)

(defvar multiclip--original-interprocess-paste-function
  interprogram-paste-function)

(defconst multiclip--directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(defconst html-format "html" "HTML format string.")
(defconst text-format "text" "Text format.")

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
  (multiclip-init))

(defun multiclip-init ()
  (interactive)
  (setq interprogram-cut-function
        (if multiclip-mode 'multiclip-copy-to-clipboard
          multiclip--original-interprocess-cut-function))
  (setq interprogram-paste-function
        (if multiclip-mode 'multiclip-paste-from-clipboard
          multiclip--original-interprocess-paste-function))
  )

;; ------------------------------------------------------------
;; Core functions.
;;

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

(defvar multiclip--last-copy nil)

(defun multiclip-copy-to-clipboard (text)
  "Copy TEXT with formatting to the system clipboard."
  (prog1
      ;; Set the normal clipboard string(s).
      ;; (funcall multiclip--original-interprocess-cut-function text)
      (setq multiclip--last-copy text)
    ;; Add addition flavor(s)
    (save-excursion
      (with-temp-buffer
        (goto-char (point-min))
        (insert text)
        (let* ((htmlize-output-type 'inline-css)
               (html-text (with-current-buffer (htmlize-buffer)
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
                            (let ((text (buffer-string))) (kill-buffer) text))))
          (when multiclip--set-data-to-clipboard-function
            (funcall multiclip--set-data-to-clipboard-function
                     `((text-format . ,text) (html-format . ,html-text)))))
        ))))

(defun multiclip-paste-from-clipboard ()
  "Paste from system clipboard"
  (funcall multiclip--get-data-from-clipboard-function))
;; ------------------------------------------------------------
;; System-specific support.
;;

(defvar multiclip--set-data-to-clipboard-function nil)
(defvar multiclip--get-data-from-clipboard-function nil)

(defun multiclip-set-defaults ()
  "Set up multiclip, or issue an error if system not supported."
  (cond ((eq system-type 'darwin)
         (setq multiclip--set-data-to-clipboard-function
               #'multiclip--set-data-to-clipboard-osx)
         (setq multiclip--get-data-from-clipboard-function
                 #'multiclip--get-data-from-clipboard-osx))
        ((memq system-type '(windows-nt cygwin))
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

(defun multiclip--set-data-to-clipboard-w32 (data)
  "DATA is a list of (format . text)."
  (let ((clipboard (start-process "clipboard"
                                  nil
                                  (concat multiclip--directory "bin/csclip.exe")
                                  "copy"))
          (data (json-encode (mapcar (lambda (x) `((cf . ,(eval (car x))) (data . ,(cdr x)))) data))))
    (process-send-string clipboard data)
    (process-send-eof clipboard)))

(defun multiclip--get-data-from-clipboard-w32 ()
  "."
  (let ((text (shell-command-to-string (concat multiclip--directory "bin/csclip.exe paste"))))
    (unless (string= text multiclip--last-copy) text)))


(provide 'multiclip)

;;; multiclip.el ends here
