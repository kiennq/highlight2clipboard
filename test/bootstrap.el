;;; bootstrap.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  Kien Nguyen <kien.n.quang <at> gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'package)

(setq user-emacs-directory (expand-file-name (make-temp-name ".emacs.d") "~")
      package-user-dir (expand-file-name (make-temp-name "tmp-elpa")
                                         user-emacs-directory))

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
       (no-byte-compile t)
       (deps '(htmlize f dash
               (jsonrpc :fetcher url
                        :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/jsonrpc.el"))))
  (package-initialize)

  ;; bootstrap quelpa
  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

  (mapc #'quelpa deps)
  (add-hook 'kill-emacs-hook `(lambda ()
                                (delete-directory ,user-emacs-directory t))))
