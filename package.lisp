;; ULF Pragmatics
;; Packaged on 2020-02-01

(in-package :cl-user)

(defpackage :ulf-pragmatics
  (:use :cl :ttt :cl-strings :ulf-lib :lisp-unit :gute :ulf2english)
  (:shadow :insert)
  (:export infer-all
           get-response-to-bw-presupposition-failure
           get-wh-question-presupposition))

;; Global variables.
(in-package :ulf-pragmatics)
(defparameter *debug-ulf-pragmatics* nil)
(defparameter *dynamic-polarity-dir*
  (merge-pathnames (parse-namestring "dynamic-polarity/")
                   ulf-pragmatics/config:*base-directory*))
; TODO(gene): check that we need the filpath below, if so, copy it in so we still have access.
(defparameter *top10000-word-filepath*
      "../resources/google-10000-english.txt")

