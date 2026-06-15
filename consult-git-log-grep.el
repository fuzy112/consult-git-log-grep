;;; consult-git-log-grep.el --- Consult integration for git log grep  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ghosty

;; Author: Ghosty
;; Homepage: https://github.com/Ghosty141/consult-git-log-grep
;; Keywords: git convenience
;; Version: 1.3.0
;; Package-Requires: ((emacs "28.1") (consult "1.9"))

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

;; `consult-git-log-grep' makes git log --grep accessible via consult

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'consult)

(defcustom consult-git-log-grep-args
  '("git" "--no-pager" "log"
    ;; use git log's formattings padding/truncating for
    ;; better performance (less lisp string processing)
    "--pretty=format:%H@@@%<(76,mtrunc)%s@@@%aN@@@%ad"
    "--date=format:%Y-%m-%d\ %H:%M:%S"
    "--all-match"
    "--regexp-ignore-case" "--extended-regexp")
  "Command line arguments for git-log.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-git-log-grep-open-function #'consult-git-log-grep-show-commit
  "The function used to open the selected candidate."
  :package-version '(consult-git-log-grep . "1.0.0")
  :group 'consult-git-log-grep
  :type '(function :tag "Function"))

(defcustom consult-git-log-grep-preview nil
  "Whether to show a preview of the selected candidate"
  :package-version '(consult-git-log-grep . "1.3.0")
  :group 'consult-git-log-grep
  :type 'boolean)

(defface consult-git-log-grep-sha
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight the commit sha in 'consult-git-log-grep'."
  :group 'consult-git-log-grep)

(defface consult-git-log-grep-author
  '((t :inherit completions-annotations))
  "Face used to highlight the author in 'consult-git-log-grep'."
  :group 'consult-git-log-grep)

(defface consult-git-log-grep-datetime
  '((t :inherit completions-annotations))
  "Face used to highlight the datetime in 'consult-git-log-grep'."
  :group 'consult-git-log-grep)

(defvar consult-git-log-grep--history nil)

(defun consult-git-log-grep-show-commit (sha)
  "Displays the result of 'git show SHA' in a new buffer."
  (let* ((short-sha (truncate-string-to-width sha 8))
         (buf (get-buffer-create (format "consult-git-log-grep-commit-%s" short-sha))))
    (shell-command (format "git --no-pager show %s" sha) buf)))

(defun consult-git-log-grep--format (line)
  "Format git log grep candidates from LINES."
  (let ((str (car line)))
    (when (string-match "\\([a-z0-9].*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)" str)
      (let* ((sha (match-string 1 str))
             (suffix (match-string 1 str))
             (msg (match-string 2 str))
             (author (match-string 3 str))
             (datetime (match-string 4 str)))
        (put-text-property 0
                           1
                           'consult-log-grep--metadata
                           `((sha . ,sha)
                             (author . ,author)
                             (datetime . ,datetime))
                           msg)
        (add-text-properties 0 (length suffix) '(invisible t consult-strip t) suffix)
        (list (cons (concat msg suffix) sha))))))

(defun consult-git-log-grep---make-builder (paths)
  "Create git log grep command line builder given PATHS."
  (let ((cmd (consult--build-args consult-git-log-grep-args)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (flags (append cmd opts))
                   (ignore-case (or (member "-i" flags) (member "--regexp-ignore-case" flags)))
                   (type (cond ((or (member "-E" flags) (member "--extended-regexp" flags)) 'extended)
                               ((or (member "-P" flags) (member "--perl-regexp" flags)) 'pcre)
                               (t 'basic))))
        (if (or (member "-F" flags) (member "--fixed-strings" flags))
            (cons (append cmd (list "--grep" arg) opts '("--") paths)
                  (apply-partially #'consult--highlight-literals arg ignore-case))
          (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
            (when re
              (cons (append cmd
                            (mapcan (apply-partially #'list "--grep") re)
                            opts '("--") paths)
                    hl))))))))

(defun consult-git-log-grep-result-annotator (cand)
  "Annotate the current candidate CAND using its text-properties."
  (when-let (metadata (get-text-property 0 'consult-log-grep--metadata cand))
    (let ((shortsha (truncate-string-to-width (cdr (assoc 'sha metadata)) 8))
          (datetime (cdr (assoc 'datetime metadata)))
          (author (cdr (assoc 'author metadata))))
      (format " %s  %s  %s"
              (propertize shortsha 'face 'consult-git-log-grep-sha)
              (propertize datetime 'face 'consult-git-log-grep-datetime)
              (propertize author 'face 'consult-git-log-grep-author)))))


;;;###autoload
(defun consult-git-log-grep (&optional dir initial)
  "Search the git log using 'git log --grep' in DIR starting with INITIAL input."
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Commit Subject" dir))
               (default-directory dir)
               (builder (consult-git-log-grep---make-builder paths))
               (result (consult--read
                        (consult--process-collection builder
                          :transform (consult--async-transform #'consult-git-log-grep--format)
                          :highlight t)
                        :prompt prompt
                        :require-match t
                        :sort nil
                        :lookup #'consult--lookup-cdr
                        :category 'consult-git-log-grep-result
                        :annotate 'consult-git-log-grep-result-annotator
                        :initial initial
                        :add-history (thing-at-point 'symbol)
                        :history '(:input consult-git-log-grep--history)
                        :state #'consult-git-log-grep--preview)))
    (funcall consult-git-log-grep-open-function result)))

(defun consult-git-log-grep--preview (action cand)
  (and cand
       consult-git-log-grep-preview
       (eq action 'preview)
       (funcall consult-git-log-grep-open-function cand)))

(provide 'consult-git-log-grep)
;;; consult-git-log-grep.el ends here
