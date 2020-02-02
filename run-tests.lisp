;; Gene Kim 7-24-2018

(load "load")

;; Load testing code.
(load "test/test-util.lisp")
;(load "test/lisp-unit-example.lisp")
(load "test/pilot/pilot-test-util.lisp")
(load "test/pilot/len-pilot-tests.lisp")
(load "test/pilot/gene-devset-tests.lisp")
(load "test/pilot/select-sampled-ulf.lisp")
(load "test/pilot/general-sampled-ulf.lisp")
(load "test/counterfactual-unit-tests.lisp")
(load "test/question-unit-tests.lisp")
(load "test/request-unit-tests.lisp")
(load "test/implicative-unit-tests.lisp")
(load "test/ttt-preds-and-functions-tests.lisp")
(load "test/dynamic-polarity-tests.lisp")

(in-package :ulf-pragmatics)

;; Lisp unit printing options.
(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

;; Local printing options.
(setq *debug-ulf-pragmatics* nil)

(run-tests)

