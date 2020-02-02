
(in-package :ulf-pragmatics)

(DEFUN TENSE-OR-ASPECT? (L)
  (IF (MEMBER L '(PAST PRES PERF PROG))
      T
      NIL))

(DEFCLASS IMPLICATIVE-RULE-TTT NIL
          ((TYPE :INITARG :TYPE :INITFORM 'S)
           (POLARITY :INITARG :POLARITY :INITFORM '+) (RULE :INITARG :RULE)))

(DEFPARAMETER *INFER-FROM-IMPLICATIVE-RULES-WEAK*
  (LIST
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! THINK.V (TENSE-OR-ASPECT? THINK.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PREDICT.V (TENSE-OR-ASPECT? PREDICT.V)) (THAT _!3)))
                    (PROBABLY (!1 BELIEVE.V (THAT _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PREDICT.V (TENSE-OR-ASPECT? PREDICT.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SAY.V (TENSE-OR-ASPECT? SAY.V)) (THAT _!3)))
                    (PROBABLY (!1 BELIEVE.V (THAT _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SAY.V (TENSE-OR-ASPECT? SAY.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SUPPOSE.V (TENSE-OR-ASPECT? SUPPOSE.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! GUESS.V (TENSE-OR-ASPECT? GUESS.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SPECULATE.V (TENSE-OR-ASPECT? SPECULATE.V))
                      (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SUSPECT.V (TENSE-OR-ASPECT? SUSPECT.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SURMISE.V (TENSE-OR-ASPECT? SURMISE.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ADVOCATE.V (TENSE-OR-ASPECT? ADVOCATE.V)) (THAT _!3)))
                    (PROBABLY (!1 BELIEVE.V (THAT _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ARGUE.V (TENSE-OR-ASPECT? ARGUE.V)) (THAT _!3)))
                    (PROBABLY (!1 BELIEVE.V (THAT _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CLAIM.V (TENSE-OR-ASPECT? CLAIM.V)) (THAT _!3)))
                    (PROBABLY (!1 BELIEVE.V (THAT _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! HOLD.V (TENSE-OR-ASPECT? HOLD.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! APPEAR.V (TENSE-OR-ASPECT? APPEAR.V)) (KA _!3)))
                    (PROBABLY (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SEEM.V (TENSE-OR-ASPECT? SEEM.V)) (KA _!3)))
                    (PROBABLY (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    (IT ((! APPEAR.V (TENSE-OR-ASPECT? APPEAR.V)) (THAT _!3)))
                    (PROBABLY _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/ (IT ((! SEEM.V (TENSE-OR-ASPECT? SEEM.V)) (THAT _!3)))
                      (PROBABLY _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DOUBT.V (TENSE-OR-ASPECT? DOUBT.V)) (THAT _!3)))
                    (NOT (!1 BELIEVE.V (THAT _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DOUBT.V (TENSE-OR-ASPECT? DOUBT.V)) (THAT _!3)))
                    (!1 BELIEVE.V (THAT (PROBABLY (NOT _!3))))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ATTEMPT.V (TENSE-OR-ASPECT? ATTEMPT.V)) (KA _!3)))
                    (!1 WANT.V (KA _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! INTEND.V (TENSE-OR-ASPECT? INTEND.V)) (KA _!3)))
                    (!1 WANT.V (KA _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?) ((! TRY.V (TENSE-OR-ASPECT? TRY.V)) (KA _!3)))
                    (!1 WANT.V (KA _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! STRIVE.V (TENSE-OR-ASPECT? STRIVE.V)) (KA _!3)))
                    (!1 WANT.V (KA _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! STRUGGLE.V (TENSE-OR-ASPECT? STRUGGLE.V)) (KA _!3)))
                    (!1 WANT.V (KA _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ATTACK.V (TENSE-OR-ASPECT? ATTACK.V))
                      (! (+ (!2 TERM?) (FOR (KA _!3)))
                       ((!2 TERM?) (FOR (KA _!3))))))
                    (!1 BELIEVE.V (THAT (!2 _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! BLAME.V (TENSE-OR-ASPECT? BLAME.V))
                      (! (+ (!2 TERM?) (FOR (KA _!3)))
                       ((!2 TERM?) (FOR (KA _!3))))))
                    (!1 BELIEVE.V (THAT (!2 _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CRITICIZE.V (TENSE-OR-ASPECT? CRITICIZE.V))
                      (! (+ (!2 TERM?) (FOR (KA _!3)))
                       ((!2 TERM?) (FOR (KA _!3))))))
                    (!1 BELIEVE.V (THAT (!2 _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ENJOY.V (TENSE-OR-ASPECT? ENJOY.V)) (KA _!3)))
                    (!1 (AT-LEAST-OCCASIONALLY.ADV _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LIKE.V (TENSE-OR-ASPECT? LIKE.V)) (KA _!3)))
                    (!1 (AT-LEAST-OCCASIONALLY.ADV _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LOVE.V (TENSE-OR-ASPECT? LOVE.V)) (KA _!3)))
                    (!1 (AT-LEAST-OCCASIONALLY.ADV _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ENABLE.V (TENSE-OR-ASPECT? ENABLE.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 (CAN.V _!3))))))
