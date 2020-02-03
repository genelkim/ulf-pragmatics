;;; Gene Kim 3-1-2019
;;;
;;; Situational Inferences that are simple to generate.

;; This file must be loaded after question-inferences.lisp

(in-package :ulf-pragmatics)

(defun tense-of-vp! (ulf)
;``````````````````````````
; Returns the tense of a verb phrase. If it's not a tensed verb, it returns nil.
  (if (not (tensed-verb? ulf))
    nil
    (first (ulf:find-vp-head ulf))))


(defun tense-of-sent! (ulf)
;``````````````````````````
; Returns the tense of the sentence. If it's not a tensed sentence, then returns
; nil.
;; TODO: make a robust version that checks the types and searches everything.
;; For now, just look for one of the tenses.
  (let ((tenses (util:tree-find-if ulf #'ulf:lex-tense?)))
    (if tenses (first tenses) nil)))


(defun tense-of-phrase (ulf phrase)
;``````````````````````````````````
; Determines the tense of the given phrase within the ULF.  This function
; assumes that this phrase is unique in the ULF (it will take the first
; occurrence).
;
; Traverse ULF and on the way up keep track of whether the phrase has been
; found. If the phrase has been found, without already knowing the tense, and
; it's in a tensed sentence, return the tense of the sentence.
  (labels
    (;; Recursive helper function. Returns a cons of whether the phrase has been found
     ;; and the tense (nil if tense is not yet found).
     (recfn (curulf)
       (cond
         ((equal curulf phrase) (cons t nil))
         ((atom curulf) (cons nil nil))
         (t
           (let* ((carres (recfn (car curulf)))
                  (cdrres (recfn (cdr curulf)))
                  (found (or (car carres) (car cdrres)))
                  (tense (if (cdr carres) (cdr carres) (cdr cdrres))))
             (cond
               ;; Phrase and tense already found.
               (tense (cons found tense))
               ;; Phrase found but no tense.
               (found (cons found
                            (if (ulf:tensed-sent? curulf)
                              (tense-of-sent! curulf)
                              nil)))
               ;; Propagate (this should just be (nil . nil)).
               (t (cons found tense))))))))
    ;; Start the recursive function and strip off the extra info.
    (let ((prelim-result (cdr (recfn ulf))))
      (if prelim-result prelim-result
        (tense-of-sent! ulf)))))

;; Maps a noun phrase from the genitive form to the explicit possessive form.
(defun poss-noun-to-possession-form! (np)
  (if (plur-noun? np)
    (list 'k np)
    (list 'a.d np)))

;; This infernce should be run at the top level of the sentence.
(defparameter *infer-possession-from-genitives*
  '(/ ((!1 poss-det?) (!2 noun?))
      ((!1 !2)
       (poss-det-to-possessor! !1)
       (have.v (poss-noun-to-possession-form! !2)))))


(defun infer-possession-from-genitives (ulf)
;```````````````````````````````````````````
; For a sentence, get the tense-less genitive inferences from the ULF, then add
; the tenses.
  (let (untensed-res tensed-res)

    (setq untensed-res
          (mapcar #'result-formula
                  (all-ttt-rule-inf-result *infer-possession-from-genitives* ulf)))

    (setq tensed-res
          (mapcar
            #'(lambda (res)
                (let* ((original-gen (first res))
                       (np (second res))
                       (untensed-vp (third res))
                       (tense (tense-of-phrase ulf original-gen))
                       (hv (find-vp-head untensed-vp)))
                  (list np
                        (replace-vp-head untensed-vp (list tense hv)))))
            untensed-res))

    (mapcar
      #'(lambda (x)
          (make-instance
            'inf-result
            :result-formula (util:unhide-ttt-ops x)
            :inf-rule 'possession-from-genitive
            :src-local-ulf nil
            :src-parent-ulf nil
            :src-full-ulf ulf))
      tensed-res)))

(defun infer-possession-from-genitives-raw (ulf)
  (mapcar #'result-formula (infer-possession-from-genitives ulf)))


;; Substitute relativizer
(defun subst-relativizer! (newval ulfwrel)
  (subst-if newval #'ulf:lex-rel? ulfwrel))
(defun subst-relativizer-apply-subrep! (newval ulfwrel)
  (util:hide-ttt-ops
    (nth-value
      1 (ulf:apply-sub-macro ; TODO: write another sub-macro function that just returns the value...
          (nth-value
            1 (ulf:apply-rep-macro
                (subst-relativizer! newval
                                    (util:unhide-ttt-ops ulfwrel))
                :calling-package :ulf-pragmatics))
          :calling-package :ulf-pragmatics))))

(defparameter *infer-np+preds-embedded-relative-clause*
  '(/ (np+preds _!1 _*2
                (!3 ulf:relativized-sent?)
                _*4)
      (subst-relativizer-apply-subrep! _!1 !3)))

(defparameter *infer-n+preds-embedded-relative-clause*
  '(/ ((!1 ulf:det? ulf:noun-reifier? ulf:preposs-macro?)
       (n+preds _!2 _*3
                (!4 ulf:relativized-sent?)
                _*5))
      (subst-relativizer-apply-subrep! (!1 _!2) !4)))

(defun infer-np+preds-embedded-relative-clause (ulf)
  (all-ttt-rule-inf-result *infer-np+preds-embedded-relative-clause* ulf))
(defun infer-n+preds-embedded-relative-clause (ulf)
  (all-ttt-rule-inf-result *infer-n+preds-embedded-relative-clause* ulf))


;; Infer existence from n+preds/np+preds
;; (the.d (n+preds X Y)) -> ((some.d X) ((<tense> be.v) Y))
;; TODO(gene): handle this more generally by implementing n+preds expansion and
;; then running (the.d x) -> (some.d x) inference.

;; TTT rule that matches definite n+preds expressions and generates a list of
;; 1. the original expression, and
;; 2. the definite expression with the noun from n+preds.
;; 3. a tense-less propositional verb phrase asserting the existence of a thing
;;    that satisfies the post-modifying predicate.
(defparameter *infer-existence-from-definite-n+preds-ttt*
  '(/ ((!1 ulf:det?)
       (n+preds _!2 _*3
                (!4 ~ ulf:relativized-sent?) ; must be a simple predicate
                _*5))
      (; original expression
       (!1 (n+preds _!2 _!3 !4 _*5))
       ; subject
       (some.d _!2)
       ; propositional vp
       (be.v !4))))

;; TODO(gene): generalize this to anything that takes a TTT rule that generates
;; a triple, an inference name, and returns a function that takes a ulf, tenses
;; the result and wraps it in inf-result.
(defun infer-existence-from-definite-n+preds (ulf)
  (let (untensed-res tensed-res)
    (setf untensed-res
          (mapcar #'result-formula
                  (all-ttt-rule-inf-result
                    *infer-existence-from-definite-n+preds-ttt*
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
            :result-formula (util:unhide-ttt-ops x)
            :inf-rule 'existence-from-definite-n+preds
            :src-local-ulf nil
            :src-parent-ulf nil
            :src-full-ulf ulf))
      tensed-res)))


(defun premacro-sit-inferences (ulf)
  (cdar (results-from-applying-rules
          (list #'infer-possession-from-genitives
                #'infer-np+preds-embedded-relative-clause
                #'infer-n+preds-embedded-relative-clause
                #'infer-existence-from-definite-n+preds
                )
          (list ulf) t)))

