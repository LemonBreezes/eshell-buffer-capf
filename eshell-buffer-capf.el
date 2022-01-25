;;; packages/eshell-buffer-capf/eshell-buffer-capf.el -*- lexical-binding: t; -*-

;; Author: Antonio Ruiz <antonioruiz.math@gmail.com>
;; URL: https://github.com/LemonBreezes/eshell-buffer-capf
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (anaphora "1.0.4"))
;; Keywords: extensions

;; eshell-buffer-capf.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; eshell-buffer-capf.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Complete buffer expressions in Eshell.

(eval-when-compile
  (require 'subr-x))

(defun eshell-buffer-capf--bounds ()
  "Return a cons cell containing (beg . end).

This represents the boundaries of the topmost expression
enclosed by `<', `>'."
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (modify-syntax-entry ?\\ "\\" table)
    (ignore-error 'scan-error
      (with-syntax-table table
        (save-excursion
          (save-restriction
            (narrow-to-region
             (save-excursion (eshell-bol) (point))
             (point-at-eol))
            (when-let ((beg (car-safe (nth 9 (syntax-ppss)))))
              (cons (1+ (goto-char beg))
                    (1- (progn (forward-sexp)
                               (point)))))))))))

(defun eshell-buffer-capf--candidates ()
  "Return the list of candidates for `eshell-buffer-capf'."
  (mapcar #'buffer-name (buffer-list)))

(defun eshell-buffer-capf--predicate (buffer)
  "Return nil if BUFFER is read only."
  (not (buffer-local-value 'buffer-read-only
                           (get-buffer buffer))))

;;;###autoload
(defun eshell-buffer-capf ()
  "Return the list of buffer completions if inside of buffer delimiters."
  (when-let* ((bounds (eshell-buffer-capf--bounds))
              (complete-p (eq (char-before (1- (car bounds))) ?#)))
    (list (car bounds)
          (cdr bounds)
          (completion-table-dynamic
           (lambda (_)
             (eshell-buffer-capf--candidates)))
          :predicate #'eshell-buffer-capf--predicate
          :exclusive 'no)))

;;;###autoload
(defun eshell-buffer-capf-setup ()
  "Install `eshell-buffer-capf' to `completion-at-point-functions'."
  (add-hook 'completion-at-point-functions
            #'eshell-buffer-capf
            nil t))

(provide 'eshell-buffer-capf)
