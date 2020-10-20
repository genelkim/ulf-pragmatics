;;; Gene Kim 2020-02-03
;;;
;;; Inferences that are specific to blocks world ULFs.

;; This file must be loaded after situational-inferences.lisp

(in-package :ulf-pragmatics)

;; TTT rule that matches definite superlative expressions and returns
;; 1. the original expression, and
;; 2. a existential expression with the noun from the superlative.
;; 3. a tense-less propositional verb phrase asserting the existence of a thing
;;    that satisfies the full superlative expression.
(defparameter *infer-existence-from-bw-superlative-ttt*
  '(/ ((!1 ulf:det?) (most-n _!2 _!3))
      (; original expression
       (!1 (most-n _!2 _!3))
       ; subject
       (some.d _!3)
       ; propositional vp
       (be.v (= (!1 (most-n _!2 _!3)))))))

;; TODO(gene): generalize this to anything that takes a TTT rule that generates
;; a triple, an inference name, and returns a function that takes a ulf, tenses
;; the result and wraps it in inf-result.
;; TODO(gnee): add test.
(defun infer-existence-from-bw-superlative (ulf)
  (let (untensed-res tensed-res)
    (setf untensed-res
          (mapcar #'result-formula
                  (all-ttt-rule-inf-result
                    *infer-existence-from-bw-superlative-ttt*
                    ulf)))
    (setf tensed-res
          (mapcar
            #'(lambda (res)
                (let* ((original-expr (first res))
                       (subj (second res))
                       (untensed-vp (third res))
                       (tense (tense-of-phrase ulf original-expr))
                       (hv (find-vp-head untensed-vp)))
                  (list subj
                        (replace-vp-head untensed-vp (list tense hv)))))
            untensed-res))
    (mapcar
      #'(lambda (x)
          (make-instance
            'inf-result
            :result-formula (unhide-ttt-ops x)
            :inf-rule 'existence-from-bw-superlative
            :src-local-ulf nil
            :src-parent-ulf nil
            :src-full-ulf ulf))
      tensed-res)))

(defun premacro-bw-inferences (ulf)
  (cdar (results-from-applying-rules
          (list #'infer-existence-from-bw-superlative
                )
          (list ulf) t)))

