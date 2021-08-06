;; ULF Pragmatics
;; Packaged on 2020-02-01

(asdf:defsystem :ulf-pragmatics
  :name "ulf-pragmatics"
  :version "0.0.1"
  :author "Gene Louis Kim"
  :depends-on (:ttt :cl-strings :gute :ulf-lib :ulf2english :lisp-unit :uiop)
  :components ((:file "package")
               (:file "util")
               (:file "ttt-preds-and-functions")
               (:file "inference-core")
               (:file "counterfactual-inferences")
               (:file "request-inferences")
               (:file "question-inferences")
               (:file "dynamic-polarity/dynamic-polarity")
               (:file "raw-implicative-inferences")
               (:file "raw-implicative-weak-inferences")
               (:file "implicative-inferences")
               (:file "implicative-weak-inferences")
               (:file "situational-inferences")
               (:file "bw-inferences")
               (:file "inference")
               (:file "blocks-world-question-presuppositions"))
  :around-compile (lambda (next)
                    ; For debugging/development.
                    (proclaim '(optimize (debug 3) (safety 3) (space 0) (speed 0)))
                    ; For production.
                    ;(proclaim '(optimize (debug 0) (safety 1) (space 1) (speed 3)))
                    (funcall next)))

;; This is to store the path to the source code
;; suggested here https://xach.livejournal.com/294639.html
(defpackage #:ulf-pragmatics/config (:export #:*base-directory*))
(defparameter ulf-pragmatics/config:*base-directory*
  (asdf:system-source-directory "ulf-pragmatics"))

