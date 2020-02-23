;;; Ben Kane 7-22-2018
;;;
;;; Parsed into TTT rules from Karl Stratos rules from Epilog package using /resources/stratos/parser.lisp.

(in-package :ulf-pragmatics)

;; Map the rules to simpler rules because there's a bug in TTT for these patterns...
(defparameter *infer-from-implicative-rules-weak-notense*
  (mapcar
    #'(lambda (r)
        (make-instance
          'implicative-rule-ttt
          :type (slot-value r 'type)
          :polarity (slot-value r 'polarity)
          :rule (let* ((rule (slot-value r 'rule))
                       (vb-patt (first (tree-find-if rule #'implicative-rule-verb?))))
                  (subst (implicative-rule-verb-to-notense vb-patt)
                         vb-patt
                         rule))))
    *infer-from-implicative-rules-weak*))
(defparameter *infer-from-implicative-rules-weak-onlytense*
  (mapcar
    #'(lambda (r)
        (make-instance
          'implicative-rule-ttt
          :type (slot-value r 'type)
          :polarity (slot-value r 'polarity)
          :rule (let* ((rule (slot-value r 'rule))
                       (vb-patt (first (tree-find-if rule #'implicative-rule-verb?))))
                  (subst (implicative-rule-verb-to-onlytense vb-patt)
                         vb-patt
                         rule))))
    *infer-from-implicative-rules-weak*))

(defun inf-result-apply-implicative-rules-weak (ulf)
  (let ((ttt-rules
          (mapcar (lambda (x)
                    (if (equalp (slot-value x 'polarity)
                                (get-implicative-rule-polarity ulf (slot-value x 'rule)))
                      (slot-value x 'rule)))
                  (append
                    *infer-from-implicative-rules-weak-notense*
                    *infer-from-implicative-rules-weak-onlytense*)))
        (preproc-ulf (remove-aux-not ulf)))
    (apply #'append ; flatten one level.
           ;; TODO: check perhaps presuppositional inferences should only be shallow?
           (mapcar #'(lambda (rule) (all-ttt-rule-inf-result rule preproc-ulf))
                   (remove-if #'null ttt-rules)))))


