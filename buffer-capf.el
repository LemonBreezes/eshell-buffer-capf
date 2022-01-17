;;; packages/buffer-capf/buffer-capf.el -*- lexical-binding: t; -*-

(defun buffer-capf--ppss ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (with-syntax-table table
      (syntax-ppss))))

(defun buffer-capf ()
  (when-let* ((op (car (nth 9 (buffer-capf--ppss))))
              (complete-p (eq (char-before op) ?#))
              (prefix (buffer-substring-no-properties
                       (1+ op)
                       (point))))
    (cl-loop for buffer in (buffer-list)
             when (string-prefix-p prefix (buffer-name buffer))
             collect (buffer-name buffer))))
