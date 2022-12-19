(quicklisp:quickload "alexandria")
(quicklisp:quickload "rove")

(uiop:define-package #:day8
  (:use #:cl)
  (:export :parse-file :line :column))

(defpackage #:step1
  (:use #:cl #:day8 #:alexandria)
  (:export :step1))

;(uiop:define-package #:day8/tests/day8
;    (:use #:cl
;          #:rove
;          #:day8))
