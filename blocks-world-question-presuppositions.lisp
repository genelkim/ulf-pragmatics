;;; Ben Kane
;;;
;;; Specific presupposition inferences from WH-questions for use in question-answering tasks.

(in-package :ulf-pragmatics)


(defun qmark? (ulf)
  (and (symbolp ulf) (equal ulf '?)))


(defun get-response-to-bw-presupposition-failure (rawulf &key (calling-package nil))
; `````````````````````````````````````````````````````````````````````````````````````
; Gets relevant (task-specific) response in the blocks world domain
; in the case of presupposition failure for a question. This is done
; by generating the relevant presuppositions, choosing the relevant one
; to reply to (currently just the first/only one in the list), and negating
; the ULF of the presupposition.
; NOTE: Currently unused due to TTT/package bug
;
  (util:inout-intern (rawulf ulf :ulf-pragmatics :callpkg calling-package)
    (negate-wh-question-presupposition (normalize-wh-question-presupposition
      (get-wh-question-presupposition ulf))))
) ; END get-response-to-bw-presupposition-failure


(defun get-wh-question-presupposition (rawulf &key (calling-package nil))
; `````````````````````````````````````````````````````````````````````````
; Gets relevant (task-specific) presupposition from a wh-question.
;
  (util:inout-intern (rawulf ulf :ulf-pragmatics :callpkg calling-package)
    (cond
      ((ttt:match-expr '(^* (at.p (what.d place.n))) ulf)
        (ttt:apply-rule '(/ ((sub (at.p (what.d place.n)) _!) qmark?)
                          (you.pro ((pres know.v) (ans-to (sub where.pq _!))))) ulf))
      ((ttt:match-expr '(^* (of.p (what.d color.n))) ulf)
        (if (ttt:match-expr '(^* most-n) ulf)
          (get-bw-inferences-of-type ulf 'infer-existence-from-bw-superlative)
          (get-bw-inferences-of-type ulf 'infer-existence-from-definite-n+preds)))
      (t (get-bw-inferences-of-type ulf 'infer-presuppositions-from-wh-q))))
) ; END get-wh-question-presuppositions


(defun get-bw-inferences-of-type (ulf type)
; ```````````````````````````````````````````
; Gets all inferences according to the function given in type.
;
  (car (mapcar (lambda (inf) (output-ulf-normalization (result-formula inf))) (funcall type ulf)))
) ; END get-bw-inferences-of-type


(defun normalize-wh-question-presupposition (ulf)
; `````````````````````````````````````````````````````````````
; Extracts result formula and applies task-specific normalization, including
; (for now) fixing bugs, such as the presupposition having "something.d block.n"
; instead of "some.d block.n".
;
  (ttt:apply-rules
    '((/ (something.d _!) (some.d _!))
      (/ (some.d (ulf:adj? (! ulf:noun? (plur ulf:noun?)))) (some.d !))
      (/ (nquan (somehow.mod-a many.a)) some.d)
      (/ (I.pro ((past ulf:verb?) _! (adv-e (! (^* (some.d _!1))))))
         (I.pro ((past ulf:verb?) _!)))
      (/ ((a.d _!) ((tense? be.v) _*))
         ((some.d _!) ((tense? be.v) _*)))
      (/ ((! ulf:verb? (tense? ulf:verb?)) (a.d _!)) (! (some.d _!)))
      )
    ulf)
) ; END normalize-wh-question-presupposition


(defun negate-wh-question-presupposition (ulf)
; `````````````````````````````````````````````
; Negates a presupposition by changing "some"/"something" to "no"/"nothing" (in
; the case of negations, we want to remove the double negative), or otherwise adding
; a negation to the ULF.
  (cond
    ((ttt:match-expr '(^* (! some.d something.pro)) ulf)
      (ttt:apply-rules
        '((/ something.pro nothing.pro)
          (/ (some.d _!) (no.d _!))
          (/ (nothing.pro ((? (tense? do.aux-s)) not.adv-a (ulf:verb? _*))) (everything.pro ((tense? ulf:verb?) _*)))
          (/ ((no.d _!) ((? (tense? do.aux-s)) not.adv-a (ulf:verb? _*))) ((every.d _!) ((tense? ulf:verb?) _*)))
          (/ (nothing.pro ((tense? ulf:verb?) not.adv-a _*)) (everything.pro ((tense? ulf:verb?) _*)))
          (/ ((no.d _!) ((tense? ulf:verb?) not.adv-a _*)) ((every.d _!) ((tense? ulf:verb?) _*)))
          (/ (every.d (! (^* (plur ulf:noun?)))) (all.d !)))
      ulf))
    (t
      (ttt:apply-rules
        '((/ (_! ((tense? be.v) _*)) (_! ((tense? be.v) not.adv-a _*)))
          (/ (_! ((tense? ulf:verb?) _*)) (_! ((tense? do.aux-s) not.adv-a (ulf:verb? _*)))))
      ulf :max-n 1 :shallow t)))
) ; END negate-wh-question-presupposition





;; (mapcar (lambda (ulf)
;;   (format t "~a~%" (ulf2english:ulf2english (get-response-to-bw-presupposition-failure ulf))))
;;   ;; (format t "~a~%" (get-response-to-bw-presupposition-failure ulf)))
;; '(
;;  (((THE.D (|Twitter| BLOCK.N)) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
;;  (((WHAT.D BLOCK.N) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
;;  (((WHAT.D (PLUR BLOCK.N)) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
;;  (((WHAT.D (PLUR BLOCK.N)) ((PRES DO.AUX-S) NOT.ADV-A (TOUCH.V (THE.D (|SRI | BLOCK.N))))) ?)
;;  (((WHAT.D (PLUR BLOCK.N)) ((PRES BE.V) NOT.ADV-A (TO_THE_LEFT_OF.P (THE.D (|Twitter| BLOCK.N))))) ?)
;;  (((PRES BE.V) THERE.PRO (A.D BLOCK.N) (ON.P (THE.D (|SRI | BLOCK.N)))) ?)
;;  (((PRES BE.V) THERE.PRO (K (PLUR BLOCK.N)) (ON.P (THE.D (|SRI | BLOCK.N)))) ?)
;;  (((PRES DO.AUX-S) (SOME.D BLOCK.N) (TOUCH.V (THE.D (SRI  BLOCK.N)))) ?)
;;  ((SUB (OF.P (WHAT.D COLOR.N)) ((THE.D (MOST-N LEFT.A BLOCK.N)) ((PRES BE.V) *H))) ?)
;;  ((SUB (OF.P (WHAT.D COLOR.N)) ((THE.D (N+PREDS BLOCK.N (ON.P (THE.D (|Twitter| BLOCK.N))))) ((PRES BE.V) *H))) ?)
;;  ((((NQUAN (HOW.MOD-A MANY.A)) (RED.A (PLUR BLOCK.N))) ((PRES BE.V) (ON.P (THE.D TABLE.N)))) ?)
;;  (((WHAT.D BLOCK.N) ((PRES BE.V) (BETWEEN.P ((THE.D (|SRI | BLOCK.N)) AND.CC (THE.D (|NVidia| BLOCK.N)))))) ?)
;;  ((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|SRI | BLOCK.N)) ((PRES BE.V) *H))) ?)
;;  ((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (MOST-N LEFT.A (GREEN.A BLOCK.N))) ((PRES BE.V) *H))) ?)
;;  ((WHAT.PRO ((PRES BE.V) (= (THE.D (MOST-N LEFT.A BLOCK.N))))) ?)
;;  ((SUB (WHAT.D BLOCK.N) ((THE.D (|Twitter| BLOCK.N)) ((PRES BE.V) (ON_TOP_OF.P *H)))) ?)
;;  (((WHAT.D (COLOR.A (PLUR BLOCK.N))) ((PRES BE.V) (TO_THE_LEFT_OF.P (THE.D (|Texaco| BLOCK.N))))) ?)
;;  (((WHAT.D (COLOR.A (PLUR BLOCK.N))) ((PRES BE.V) (ON.P (THE.D TABLE.N)))) ?)
;;  ((SUB (OF.P (WHAT.D COLOR.N)) ((THE.D (|Texaco| BLOCK.N)) ((PRES BE.V) *H))) ?)
;;  ((SUB (WHAT.D (PLUR BLOCK.N)) ((PAST DO.AUX-S) I.PRO (MOVE.V *H))) ?)
;;  (((THE.D (NVidia BLOCK.N)) (EVER.ADV-E ((PAST PERF) (TOUCH.V (THE.D (NVidia BLOCK.N)))))) ?)
;;  ((SUB (DURING.P (WHAT.D TURN.N)) ((PAST DO.AUX-S) I.PRO (MOVE.V (THE.D (SRI  BLOCK.N)) (ADV-E *H)))) ?)
;;  (((THE.D (|Twitter| BLOCK.N)) ((PRES BE.V) (ON.P (WHICH.D BLOCK.N)))) ?)
;; ))
