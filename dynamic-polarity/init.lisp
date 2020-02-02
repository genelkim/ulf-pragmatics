;; Requires asdf and quicklisp
;; https://www.quicklisp.org/

(ql:quickload :cl-strings)
(ql:quickload :alexandria)
(ql:quickload :ulf2english)
(ql:quickload :ttt)

(setq *debug-ulf-pragmatics* nil)

;; Utility functions.
(load "../util.lisp")
(load "../util-from-pilot-project.lisp")
(load "../ttt-preds-and-functions.lisp")

(setq *top10000-word-filepath*
      "../../resources/google-10000-english.txt")

(setq *dynamic-polarity-dir* ".")
(load "dynamic-polarity.lisp")

