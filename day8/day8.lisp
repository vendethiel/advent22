(in-package #:day8)

(defun parse-file (filename)
  (mapcar (lambda (line)
            (mapcar (lambda (c) (parse-integer (string c)))
                    (coerce line 'list)))
          (uiop:read-file-lines filename)))

(defun line (board i)
  (nth i board))

(defun column (board j)
  (mapcar (lambda (line) (nth j line)) board))
