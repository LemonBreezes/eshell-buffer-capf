;;; packages/eshell-buffer-capf/eshell-buffer-capf.el -*- lexical-binding: t; -*-

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
