; Libs
(ql:quickload "alexandria")

; Helpers
(defun split-array-at (ary point)
  "Split array at point `point', excluding that element"
  (cons (subseq ary 0 point)
        (subseq ary (1+ point))))

; Step 8
(defun parse-file (filename)
  (mapcar (lambda (line)
            (mapcar (lambda (c) (parse-integer (string c)))
                    (coerce line 'list)))
          (uiop:read-file-lines filename)))

(defun line (board i)
  (nth i board))

(defun column (board j)
  (mapcar (lambda (line) (nth j line)) board))

(defun index (board i j)
  (nth j (nth i board)))

(defun is-visible (board i j)
  (let* ((height (length board))
         (width (length (car board)))
         (line (line board i))
         (column (column board j))
         (el (index board i j)))
    (or (= i 0)
        (= i (1- height))
        (= j 0)
        (= j (1- width))
        (destructuring-bind (before-line . after-line) (split-array-at line j)
          (or (every (lambda (x) (< x el)) before-line)
              (every (lambda (x) (< x el)) after-line)))
        (destructuring-bind (before-column . after-column) (split-array-at column i)
          (or (every (lambda (x) (< x el)) before-column)
              (every (lambda (x) (< x el)) after-column))))))

(defun view-distance (board i j)
  (let* ((line (line board i))
         (column (column board j))
         (el (index board i j)))
    (labels ((num-visible (ary)
               (if ary
                   (1+ (if (< (car ary) el) (num-visible (cdr ary)) 0))
                   0)))
      (*
       (destructuring-bind (before-line . after-line) (split-array-at line j)
         (* (num-visible (reverse before-line))
            (num-visible after-line)))
       (destructuring-bind (before-column . after-column) (split-array-at column i)
         (* (num-visible (reverse before-column))
            (num-visible after-column)))))))

;; Step 1
(defun step1 (board)
  (let* ((height (length board))
         (width (length (car board))))
    (loop :for i :from 0 :to (1- height)
          :summing
          (loop
            :for j :from 0 :to (1- width)
            :counting (is-visible board i j)))))

;; Step 2
(defun step2 (board)
  (let* ((height (length board))
         (width (length (car board))))
    (loop :for i :from 0 :to (1- height)
          :maximizing
          (loop
            :for j :from 0 :to (1- width)
            :maximizing (view-distance board i j)))))

; Tests
(defparameter *failed-tests* 0)
(defun asserting (r msg)
  (print (concatenate 'string
                      (if r "OK: "
                          (progn
                            (incf *failed-tests*)
                            "!!! KO: "))
                      msg)))

(defun run-tests ()
  (setf *failed-tests* 0)
  (let ((board '((3 0 3 7 3)
                 (2 5 5 1 2)
                 (6 5 3 3 2)
                 (3 3 5 4 9)
                 (3 5 3 9 0))))
    (asserting (= (length board) 5) "board height")

    (asserting (equalp (line board 0) '(3 0 3 7 3)) "Line 0")

    (asserting (equalp (column board 0) '(3 2 6 3 3)) "Column 0")

    (asserting (is-visible board 0 0) "i=0 is always visible")
    (asserting (is-visible board 0 1) "i=0 is always visible")
    (asserting (is-visible board 0 2) "i=0 is always visible")
    (asserting (is-visible board 0 3) "i=0 is always visible")
    (asserting (is-visible board 0 4) "i=0 is always visible")

    (asserting (is-visible board 4 0) "i=(h-1) is always visible")
    (asserting (is-visible board 4 1) "i=(h-1) is always visible")
    (asserting (is-visible board 4 2) "i=(h-1) is always visible")
    (asserting (is-visible board 4 3) "i=(h-1) is always visible")
    (asserting (is-visible board 4 4) "i=(h-1) is always visible")

    (asserting (is-visible board 0 0) "j=0 is always visible")
    (asserting (is-visible board 1 0) "j=0 is always visible")
    (asserting (is-visible board 2 0) "j=0 is always visible")
    (asserting (is-visible board 3 0) "j=0 is always visible")
    (asserting (is-visible board 4 0) "j=0 is always visible")

    (asserting (is-visible board 0 4) "j=(w-1) is always visible")
    (asserting (is-visible board 1 4) "j=(w-1) is always visible")
    (asserting (is-visible board 2 4) "j=(w-1) is always visible")
    (asserting (is-visible board 3 4) "j=(w-1) is always visible")
    (asserting (is-visible board 4 4) "j=(w-1) is always visible")

    (asserting (is-visible board 1 1) "(1,1) is visible because 5>2 (left), 5>0 (top)")
    (asserting (is-visible board 1 2) "(1,2) is visible because 5>1 2 (right), 5>3 (top)")
    (asserting (is-visible board 2 1) "(2,1) is visible because 5>3 3 2 (right)")
    (asserting (is-visible board 2 3) "(2,3) is visible because 3>2 (right)")
    (asserting (is-visible board 3 2) "(3,2) is visible because 5>3 (bottom)")

    (let ((ary (split-array-at '(1 2 3) 1)))
      (asserting (equal '(1) (car ary)) "split-array-at part 1")
      (asserting (equal '(3) (cdr ary)) "split-array-at part 2"))

    (asserting (= 21 (step1 board)) "there are 21 cells visible")

    (asserting (= 0 (view-distance board 0 0)) "i=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 0 1)) "i=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 0 2)) "i=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 0 3)) "i=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 0 4)) "i=0 has a view distance of zero")

    (asserting (= 0 (view-distance board 4 0)) "i=(h-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 4 1)) "i=(h-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 4 2)) "i=(h-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 4 3)) "i=(h-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 4 4)) "i=(h-1) has a view distance of zero")

    (asserting (= 0 (view-distance board 0 0)) "j=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 1 0)) "j=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 2 0)) "j=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 3 0)) "j=0 has a view distance of zero")
    (asserting (= 0 (view-distance board 4 0)) "j=0 has a view distance of zero")

    (asserting (= 0 (view-distance board 0 4)) "j=(w-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 1 4)) "j=(w-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 2 4)) "j=(w-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 3 4)) "j=(w-1) has a view distance of zero")
    (asserting (= 0 (view-distance board 4 4)) "j=(w-1) has a view distance of zero")

    (asserting (= 1 (view-distance board 1 1)) "(1,1) has a view distance of 1")
    (asserting (= 4 (view-distance board 1 2)) "(1,2) has a view distance of 4")
    (asserting (= 1 (view-distance board 1 3)) "(1,3) has a view distance of 1")
    (asserting (= 6 (view-distance board 2 1)) "(2,1) has a view distance of 6")
    (asserting (= 1 (view-distance board 2 2)) "(2,2) has a view distance of 1")
    (asserting (= 2 (view-distance board 2 3)) "(2,3) has a view distance of 2")
    (asserting (= 1 (view-distance board 3 1)) "(3,1) has a view distance of 1")
    (asserting (= 8 (view-distance board 3 2)) "(3,2) has a view distance of 8")
    (asserting (= 3 (view-distance board 3 3)) "(3,3) has a view distance of 3")

    (asserting (= 8 (step2 board)) "the best step2 candidate is 8")
    )
  (print "Number of failed tests:")
  (print *failed-tests*))

(run-tests)
(print (step1 (parse-file "data-small.txt")))
(print (step1 (parse-file "data.txt")))
(print (step2 (parse-file "data-small.txt")))
(print (step2 (parse-file "data.txt")))
