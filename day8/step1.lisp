(in-package #:step1)

(defun index (board i j)
  (nth j (nth i board)))

(defun is-visible (board i j)
  (let* ((height (length board))
         (width (length (car board)))
         (line (nth i board))
         (column (alexandria:map-iota (lambda (c) (index board c j))
                                      (1- height))))
    (or (= i 0)
        (= i (1- height))
        (= j 0)
        (= j (1- width))
        (concatenate 'list))))

(defun step1 (filename)
  (let* ((board (parse-file filename))
         (height (length board))
         (width (length (car board))))
    (loop :for i :from 0 :to height
          :for j :from 0 :to width
          :counting (is-visible board i j))))
