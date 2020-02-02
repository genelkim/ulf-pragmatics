;; ULF Pragmatics
;; Packaged on 2020-02-01

(asdf:defsystem :ulf-pragmatics
  :name "ulf-pragmatics"
  :version "0.0.1"
  :author "Gene Louis Kim"
  :depends-on (:ttt :cl-strings :cl-json :cl-util :cl-ppcre :ulf-lib :ulf2english :lisp-unit :uiop)
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
               (:file "inference"))) 

