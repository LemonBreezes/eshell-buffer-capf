;;; packages/eshell-buffer-capf/eshell-buffer-capf.el -*- lexical-binding: t; -*-

;; Author: Antonio Ruiz <antonioruiz.math@gmail.com>
;; URL: https://github.com/LemonBreezes/eshell-buffer-capf
;; Version: 1.0
;; Package-Requires: ()
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

(defun eshell-buffer-capf--ppss ()
  "Parse the state of angle brackets at point."
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (with-syntax-table table
      (syntax-ppss))))

(defun eshell-buffer-capf ()
  "Return the list of buffer completions if inside of buffer delimiters."
  (when-let* ((op (car (nth 9 (buffer-capf--ppss))))
              (complete-p (eq (char-before op) ?#))
              (prefix (buffer-substring-no-properties
                       (1+ op)
                       (point))))
    (cl-loop for buffer in (buffer-list)
             when (string-prefix-p prefix (buffer-name buffer))
             collect (buffer-name buffer))))
