;;; translate-shell.el --- an unofficial Emacs front-end for translate-shell (a command-line translator powered by Google Translate)

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/translate-shell.el
;; Created: Sat Jun 20 13:35:27 CST 2015
;; Version: 0.0.1
;; Keywords: google, translate

;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `translate-shell.el' is an unofficial Emacs front-end for translate-shell
;; <https://github.com/soimort/translate-shell>. translate-shell is a
;; command-line translator powered by Google Translate.
;;
;;
;; Installations
;; =============
;;
;; To install manually, make sure this file is saved in a directory in your
;; `load-path', and add the line:
;;
;;   (require 'translate-shell)
;;
;; to your Emacs initialization file.
;;
;;
;; Usage
;; =====
;;
;; You can call `translate-shell-brief' and `translate-shell' to get the
;; definition your query word. The result will display in the echo area and
;; the "*Translate Shell*" buffer. You might want to bind them to some keys.
;;
;;
;; Cache
;; =====
;;
;; The result returned from `trans' (the translate-shell command-line program)
;; are stored in the `translate-shell-brief-cache' and `translate-shell-cache'
;; variable.
;;
;; If you want to use cache even after restarting Emacs, you need tt save these
;; two variable yourself. An easy solution is using `desktop.el', see below.
;;
;;
;; Customization
;; =============
;;
;; Below is my customization around this package:
;;
;;   (use-package desktop                    ; Save buffers, windows and frames
;;     :init (desktop-save-mode)
;;     :config
;;     (add-to-list 'desktop-globals-to-save 'translate-shell-cache)
;;     (add-to-list 'desktop-globals-to-save 'translate-shell-brief-cache))
;;
;;   (use-package translate-shell
;;     :load-path "~/wip/translate-shell.el"
;;     :bind (("C-c s"   . translate-shell-brief)
;;            ("C-c S"   . translate-shell))
;;     :config
;;     ;; <https://translate.google.com> is blocked in China for no apparent
;;     ;; reason. No one ever asked my option.
;;     (setq translate-shell-command "proxychains4 -q trans -t en %s"
;;           translate-shell-brief-command "proxychains4 -q trans -brief -t zh %s"))
;;
;;
;; TODO
;; ====
;;
;; - [ ] Add more functions to the result buffer, then if needs, add a header line to indicate usage

;;; Code:

(defgroup translate-shell nil
  "An unofficial Emacs front-end for translate-shell."
  :group 'tools
  :prefix "translate-shell-"
  :link '(emacs-commentary-link :tag "commentary" "translate-shell.el")
  :link '(emacs-library-link :tag "lisp file" "translate-shell.el")
  :link '(url-link :tag "Github" "https://github.com/xuchunyang/translate-shell.el"))

(defcustom translate-shell-target-language nil
  "Target language for translate-shell.
Must be a string with the full English name of the language. `M-x
translate-shell-available-languages' lists all available
languages. To set language for current session only use `M-x
translate-shell-set-target-language.'"
  :type 'string)

(defcustom translate-shell-command "trans"
  "The translate-shell command for the `translate-shell' command."
  :type 'string)

(defvar translate-shell-history nil
  "History list for `translate-shell' and `translate-shell-brief'.")

(defvar translate-shell-cache nil
  "Cache alist for `translate-shell'.")
(defvar translate-shell-brief-cache nil
  "Cache alist for `translate-shell-brief'.")

(defun translate-shell-lang-alist ()
  (let ((bin (shell-command-to-string
              (format "echo -n $(which %s)" translate-shell-command)))
        lang-alist)
    (with-temp-buffer
      (insert-file-contents-literally bin)
      (while (re-search-forward 
              "^Locale\\[\"\\([a-z]*\\)\"\\]\\[\"name\"\\] *= \"\\(.*\\)\"" nil t)
        (setq lang-alist (cons
                          (cons (match-string 2) (match-string 1))
                          lang-alist))))
    lang-alist))

(defun translate-shell-available-languages ()
  (interactive)
  (shell-command "trans -R"))

(defun translate-shell-set-target-language ()
  (interactive)
  (setq translate-shell-target-language (completing-read "Select target language: "
                                                 (translate-shell-lang-alist))))

(defun translate-shell-command (verbose play source target text)
  (shell-command-to-string
   (concat translate-shell-command
           (if verbose "" " -b")
           (when play " -p")
           (if source (concat " -s " source) "")
           (if target (concat " -t " target) "")
           " " text)))

(defun translate-shell--read-string ()
  "A `read-string' wrapper for translate-shell."
  (let* ((default (cond ((string-equal major-mode "pdf-view-mode")
                         (car (pdf-view-active-region-text)))
                        ((use-region-p)
                         (buffer-substring-no-properties (region-beginning)
                                                         (region-end)))
                        (t (let ((word (thing-at-point 'word)))
                             (when word (substring-no-properties word))))))
         (prompt (if (stringp default)
                     (format "Google Translate (default \"%s\"): " default)
                   "Google Translate: ")))
    (read-string prompt nil 'translate-shell-history default)))

;;;###autoload
(defun translate-shell-brief (text &optional arg)
  "Show the explanation of WORD in the echo area.
When prefixed with a
`universal-argument' (\\[universal-argument]), also play
pronunciation example."
  (interactive "i\nP")
  (let* ((query (or text (translate-shell--read-string)))
         (lang-alist (translate-shell-lang-alist))
         (target-lang (unless translate-shell-target-language
                        (completing-read "Select target language: "
                                         lang-alist)))
         (target-lang-code (alist-get (or translate-shell-target-language
                                          target-lang)
                                      lang-alist nil nil #'string=))
         (word-sym (intern query))
         result)
    (if (and (not arg) (assq word-sym translate-shell-brief-cache))
        (message (assoc-default word-sym translate-shell-brief-cache))
      (setq result (ansi-color-apply
                    (translate-shell-command nil arg nil target-lang-code
                                             (shell-quote-argument query))))
        (message result)
        (add-to-list 'translate-shell-brief-cache (cons word-sym result)))))

;;;###autoload
(defun translate-shell (text &optional arg)
  "Show the explanation of WORD in buffer.
When prefixed with a
`universal-argument' (\\[universal-argument]), also play
pronunciation example."
  (interactive "i\nP")
  (let* ((query (or text (translate-shell--read-string)))
         (lang-alist (translate-shell-lang-alist))
         (target-lang (unless translate-shell-target-language
                        (completing-read "Select target language: "
                                         lang-alist)))
         (target-lang-code (alist-get (or translate-shell-target-language
                                  target-lang)
                              lang-alist nil nil #'string=))
        (word-sym (intern query))
        result)
    (if (and (not arg) (assq word-sym translate-shell-cache))
        (setq result (assoc-default word-sym translate-shell-cache))
      (setq result (ansi-color-apply
                     (translate-shell-command t arg nil target-lang-code
                             (shell-quote-argument query))))
      (add-to-list 'translate-shell-cache (cons word-sym result)))
    (with-current-buffer (get-buffer-create "*Translate Shell*")
      (erase-buffer)
      (insert result)
      (goto-char (point-min))
      (display-buffer (current-buffer))
      (text-mode)
      ;; Set up `imenu'
      (setq imenu-generic-expression
            `(("Sections"
               ,(rx (and line-start (or "adjective" "adverb" "noun" "verb" "Synonyms" "Examples" "See also") line-end))
               0))))))

(provide 'translate-shell)
;;; translate-shell.el ends here
