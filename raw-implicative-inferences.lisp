
(in-package :ulf-pragmatics)

(DEFUN TENSE-OR-ASPECT? (L)
  (IF (MEMBER L '(PAST PRES PERF PROG))
      T
      NIL))

(DEFCLASS IMPLICATIVE-RULE-TTT NIL
          ((TYPE :INITARG :TYPE :INITFORM 'S)
           (POLARITY :INITARG :POLARITY :INITFORM '+) (RULE :INITARG :RULE)))

(DEFPARAMETER *INFER-FROM-IMPLICATIVE-RULES*
  (LIST
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ACCEPT.V (TENSE-OR-ASPECT? ACCEPT.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ACKNOWLEDGE.V (TENSE-OR-ASPECT? ACKNOWLEDGE.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ACKNOWLEDGE.V (TENSE-OR-ASPECT? ACKNOWLEDGE.V))
                      (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ADMIT.V (TENSE-OR-ASPECT? ADMIT.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ADMIT.V (TENSE-OR-ASPECT? ADMIT.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ADMONISH.V (TENSE-OR-ASPECT? ADMONISH.V))
                      (! (+ (!2 TERM?) (THAT _!3)) ((!2 TERM?) (THAT _!3)))))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ADMONISH.V (TENSE-OR-ASPECT? ADMONISH.V))
                      (! (+ (!2 TERM?) (THAT _!3)) ((!2 TERM?) (THAT _!3)))))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! AGREE.V (TENSE-OR-ASPECT? AGREE.V)) (THAT _!3)))
                    (PROBABLY _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! AGREE.V (TENSE-OR-ASPECT? AGREE.V)) (KA _!3)))
                    (PROBABLY (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ALLOW.V (TENSE-OR-ASPECT? ALLOW.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 (CAN.V _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ALLOW.V (TENSE-OR-ASPECT? ALLOW.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (NOT (!2 (CAN.V _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! AMAZE.V (TENSE-OR-ASPECT? AMAZE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! AMAZE.V (TENSE-OR-ASPECT? AMAZE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV AMAZE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV AMAZE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! AMUSE.V (TENSE-OR-ASPECT? AMUSE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! AMUSE.V (TENSE-OR-ASPECT? AMUSE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV AMUSE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV AMUSE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! ANNOY.V (TENSE-OR-ASPECT? ANNOY.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! ANNOY.V (TENSE-OR-ASPECT? ANNOY.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV ANNOY.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV ANNOY.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! APPRECIATE.V (TENSE-OR-ASPECT? APPRECIATE.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! APPRECIATE.V (TENSE-OR-ASPECT? APPRECIATE.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV APPRECIATE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV APPRECIATE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ASCERTAIN.V (TENSE-OR-ASPECT? ASCERTAIN.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV ASCERTAIN.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! ASTONISH.V (TENSE-OR-ASPECT? ASTONISH.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! ASTONISH.V (TENSE-OR-ASPECT? ASTONISH.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV ASTONISH.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV ASTONISH.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! ASTOUND.V (TENSE-OR-ASPECT? ASTOUND.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! ASTOUND.V (TENSE-OR-ASPECT? ASTOUND.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV ASTOUND.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV ASTOUND.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ATTEMPT.V (TENSE-OR-ASPECT? ATTEMPT.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! BAFFLE.V (TENSE-OR-ASPECT? BAFFLE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! BAFFLE.V (TENSE-OR-ASPECT? BAFFLE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV BAFFLE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV BAFFLE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! BEGIN.V (TENSE-OR-ASPECT? BEGIN.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! BERATE.V (TENSE-OR-ASPECT? BERATE.V))
                      (! (+ (!2 TERM?) (THAT _!3)) ((!2 TERM?) (THAT _!3)))))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! BERATE.V (TENSE-OR-ASPECT? BERATE.V))
                      (! (+ (!2 TERM?) (THAT _!3)) ((!2 TERM?) (THAT _!3)))))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! BEWILDER.V (TENSE-OR-ASPECT? BEWILDER.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! BEWILDER.V (TENSE-OR-ASPECT? BEWILDER.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV BEWILDER.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV BEWILDER.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! BOTHER.V (TENSE-OR-ASPECT? BOTHER.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! BOTHER.V (TENSE-OR-ASPECT? BOTHER.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV BOTHER.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV BOTHER.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! BOTHER.V (TENSE-OR-ASPECT? BOTHER.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! BOTHER.V (TENSE-OR-ASPECT? BOTHER.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CARE.V (TENSE-OR-ASPECT? CARE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CARE.V (TENSE-OR-ASPECT? CARE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CARE.V (TENSE-OR-ASPECT? CARE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CARE.V (TENSE-OR-ASPECT? CARE.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CEASE.V (TENSE-OR-ASPECT? CEASE.V)) (KA _!3)))
                    (PAST (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CEASE.V (TENSE-OR-ASPECT? CEASE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! COERCE.V (TENSE-OR-ASPECT? COERCE.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! COME.V (TENSE-OR-ASPECT? COME.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! COMPEL.V (TENSE-OR-ASPECT? COMPEL.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CONCEDE.V (TENSE-OR-ASPECT? CONCEDE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV CONCEDE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CONFESS.V (TENSE-OR-ASPECT? CONFESS.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CONFESS.V (TENSE-OR-ASPECT? CONFESS.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CONFIRM.V (TENSE-OR-ASPECT? CONFIRM.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV CONFIRM.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! CONFUSE.V (TENSE-OR-ASPECT? CONFUSE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! CONFUSE.V (TENSE-OR-ASPECT? CONFUSE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV CONFUSE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV CONFUSE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CONTINUE.V (TENSE-OR-ASPECT? CONTINUE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CONTINUE.V (TENSE-OR-ASPECT? CONTINUE.V)) (KA _!3)))
                    (PAST (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! CONVINCE.V (TENSE-OR-ASPECT? CONVINCE.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DARE.V (TENSE-OR-ASPECT? DARE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DARE.V (TENSE-OR-ASPECT? DARE.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DECLINE.V (TENSE-OR-ASPECT? DECLINE.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DECLINE.V (TENSE-OR-ASPECT? DECLINE.V)) (KA _!3)))
                    (PROBABLY (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DELIGHT.V (TENSE-OR-ASPECT? DELIGHT.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DELIGHT.V (TENSE-OR-ASPECT? DELIGHT.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DELIGHT.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DELIGHT.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DEMONSTRATE.V (TENSE-OR-ASPECT? DEMONSTRATE.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV DEMONSTRATE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DEPLORE.V (TENSE-OR-ASPECT? DEPLORE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DEPLORE.V (TENSE-OR-ASPECT? DEPLORE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DEPRESS.V (TENSE-OR-ASPECT? DEPRESS.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DEPRESS.V (TENSE-OR-ASPECT? DEPRESS.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DEPRESS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DEPRESS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DETEST.V (TENSE-OR-ASPECT? DETEST.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DETEST.V (TENSE-OR-ASPECT? DETEST.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISAPPOINT.V (TENSE-OR-ASPECT? DISAPPOINT.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISAPPOINT.V (TENSE-OR-ASPECT? DISAPPOINT.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DISAPPOINT.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DISAPPOINT.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISCONCERT.V (TENSE-OR-ASPECT? DISCONCERT.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISCONCERT.V (TENSE-OR-ASPECT? DISCONCERT.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DISCONCERT.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DISCONCERT.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISCOURAGE.V (TENSE-OR-ASPECT? DISCOURAGE.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISCOURAGE.V (TENSE-OR-ASPECT? DISCOURAGE.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DISCOURAGE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DISCOURAGE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DISCOVER.V (TENSE-OR-ASPECT? DISCOVER.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISGUST.V (TENSE-OR-ASPECT? DISGUST.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISGUST.V (TENSE-OR-ASPECT? DISGUST.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DISGUST.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DISGUST.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISILLUSION.V (TENSE-OR-ASPECT? DISILLUSION.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISILLUSION.V (TENSE-OR-ASPECT? DISILLUSION.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DISILLUSION.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DISILLUSION.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DISLIKE.V (TENSE-OR-ASPECT? DISLIKE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DISLIKE.V (TENSE-OR-ASPECT? DISLIKE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DISREGARD.V (TENSE-OR-ASPECT? DISREGARD.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! DISREGARD.V (TENSE-OR-ASPECT? DISREGARD.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV DISREGARD.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV DISREGARD.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISTRESS.V (TENSE-OR-ASPECT? DISTRESS.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! DISTRESS.V (TENSE-OR-ASPECT? DISTRESS.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV DISTRESS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV DISTRESS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! EMBARRASS.V (TENSE-OR-ASPECT? EMBARRASS.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! EMBARRASS.V (TENSE-OR-ASPECT? EMBARRASS.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV EMBARRASS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV EMBARRASS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! ENCOURAGE.V (TENSE-OR-ASPECT? ENCOURAGE.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! ENCOURAGE.V (TENSE-OR-ASPECT? ENCOURAGE.V))
                      (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV ENCOURAGE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV ENCOURAGE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ENVY.V (TENSE-OR-ASPECT? ENVY.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! ENVY.V (TENSE-OR-ASPECT? ENVY.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! EXCITE.V (TENSE-OR-ASPECT? EXCITE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! EXCITE.V (TENSE-OR-ASPECT? EXCITE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV EXCITE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV EXCITE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FAIL.V (TENSE-OR-ASPECT? FAIL.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FAIL.V (TENSE-OR-ASPECT? FAIL.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FIND.V (TENSE-OR-ASPECT? FIND.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FIND.V (TENSE-OR-ASPECT? FIND.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (PROBABLY (!2 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FORCE.V (TENSE-OR-ASPECT? FORCE.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV FORCE.V) (KA _!3))) (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FORGET.V (TENSE-OR-ASPECT? FORGET.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FORGET.V (TENSE-OR-ASPECT? FORGET.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FORGET.V (TENSE-OR-ASPECT? FORGET.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! FORGET.V (TENSE-OR-ASPECT? FORGET.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?) ((! GET.V (TENSE-OR-ASPECT? GET.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?) ((! GET.V (TENSE-OR-ASPECT? GET.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! HAPPEN.V (TENSE-OR-ASPECT? HAPPEN.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! HATE.V (TENSE-OR-ASPECT? HATE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! HATE.V (TENSE-OR-ASPECT? HATE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! HASTEN.V (TENSE-OR-ASPECT? HASTEN.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! HELP.V (TENSE-OR-ASPECT? HELP.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! HESITATE.V (TENSE-OR-ASPECT? HESITATE.V)) (KA _!3)))
                    (PROBABLY (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! HORRIFY.V (TENSE-OR-ASPECT? HORRIFY.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! HORRIFY.V (TENSE-OR-ASPECT? HORRIFY.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV HORRIFY.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV HORRIFY.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! IGNORE.V (TENSE-OR-ASPECT? IGNORE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! IGNORE.V (TENSE-OR-ASPECT? IGNORE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV IGNORE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV IGNORE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! IMPRESS.V (TENSE-OR-ASPECT? IMPRESS.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! IMPRESS.V (TENSE-OR-ASPECT? IMPRESS.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV IMPRESS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV IMPRESS.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! KNOW.V (TENSE-OR-ASPECT? KNOW.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! KNOW.V (TENSE-OR-ASPECT? KNOW.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV KNOW.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV KNOW.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LAMENT.V (TENSE-OR-ASPECT? LAMENT.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LAMENT.V (TENSE-OR-ASPECT? LAMENT.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LEARN.V (TENSE-OR-ASPECT? LEARN.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LEARN.V (TENSE-OR-ASPECT? LEARN.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LIE.V (TENSE-OR-ASPECT? LIE.V)) (THAT _!3)))
                    (NOT _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LIE.V (TENSE-OR-ASPECT? LIE.V)) (THAT _!3)))
                    (NOT _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LIKE.V (TENSE-OR-ASPECT? LIKE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! LOVE.V (TENSE-OR-ASPECT? LOVE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! MANAGE.V (TENSE-OR-ASPECT? MANAGE.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! MANAGE.V (TENSE-OR-ASPECT? MANAGE.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PAST MEAN.V) (KA _!3))) (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! MENTION.V (TENSE-OR-ASPECT? MENTION.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV MENTION.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! MISS.V (TENSE-OR-ASPECT? MISS.V)) (KA _!3)))
                    (PAST (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! MISS.V (TENSE-OR-ASPECT? MISS.V)) (KA _!3)))
                    (PAST (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NEGLECT.V (TENSE-OR-ASPECT? NEGLECT.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NEGLECT.V (TENSE-OR-ASPECT? NEGLECT.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV NEGLECT.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV NEGLECT.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NEGLECT.V (TENSE-OR-ASPECT? NEGLECT.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NEGLECT.V (TENSE-OR-ASPECT? NEGLECT.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NOTE.V (TENSE-OR-ASPECT? NOTE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NOTE.V (TENSE-OR-ASPECT? NOTE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NOTICE.V (TENSE-OR-ASPECT? NOTICE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! NOTICE.V (TENSE-OR-ASPECT? NOTICE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! OBSERVE.V (TENSE-OR-ASPECT? OBSERVE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! OUTRAGE.V (TENSE-OR-ASPECT? OUTRAGE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! OUTRAGE.V (TENSE-OR-ASPECT? OUTRAGE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV OUTRAGE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV OUTRAGE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! OVERLOOK.V (TENSE-OR-ASPECT? OVERLOOK.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! OVERLOOK.V (TENSE-OR-ASPECT? OVERLOOK.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV OVERLOOK.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV OVERLOOK.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PERMIT.V (TENSE-OR-ASPECT? PERMIT.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 (CAN.V _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PERMIT.V (TENSE-OR-ASPECT? PERMIT.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (NOT (!2 (CAN.V _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PERSUADE.V (TENSE-OR-ASPECT? PERSUADE.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!2 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PITY.V (TENSE-OR-ASPECT? PITY.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PITY.V (TENSE-OR-ASPECT? PITY.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV PITY.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV PITY.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV PLEASE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV PLEASE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! PLEASE.V (TENSE-OR-ASPECT? PLEASE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! PLEASE.V (TENSE-OR-ASPECT? PLEASE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PRAISE.V (TENSE-OR-ASPECT? PRAISE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PRAISE.V (TENSE-OR-ASPECT? PRAISE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV PRAISE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV PRAISE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PRETEND.V (TENSE-OR-ASPECT? PRETEND.V)) (THAT _!3)))
                    (NOT _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PRETEND.V (TENSE-OR-ASPECT? PRETEND.V)) (THAT _!3)))
                    (PROBABLY (NOT _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PRETEND.V (TENSE-OR-ASPECT? PRETEND.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PRETEND.V (TENSE-OR-ASPECT? PRETEND.V)) (KA _!3)))
                    (PROBABLY (NOT (!1 _!3)))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PROHIBIT.V (TENSE-OR-ASPECT? PROHIBIT.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (NOT (!2 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PROVE.V (TENSE-OR-ASPECT? PROVE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV PROVE.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! PROVE.V (TENSE-OR-ASPECT? PROVE.V)) (!2 TERM?)))
                    (!1 IS-A !2)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REALIZE.V (TENSE-OR-ASPECT? REALIZE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REALIZE.V (TENSE-OR-ASPECT? REALIZE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RECALL.V (TENSE-OR-ASPECT? RECALL.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RECOLLECT.V (TENSE-OR-ASPECT? RECOLLECT.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RECOGNIZE.V (TENSE-OR-ASPECT? RECOGNIZE.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RECOGNIZE.V (TENSE-OR-ASPECT? RECOGNIZE.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REFLECT.V (TENSE-OR-ASPECT? REFLECT.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REFRAIN-FROM.V (TENSE-OR-ASPECT? REFRAIN-FROM.V))
                      (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REFRAIN-FROM.V (TENSE-OR-ASPECT? REFRAIN-FROM.V))
                      (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REFUSE.V (TENSE-OR-ASPECT? REFUSE.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REFUSE.V (TENSE-OR-ASPECT? REFUSE.V)) (KA _!3)))
                    (PROBABLY (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REGRET.V (TENSE-OR-ASPECT? REGRET.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REGRET.V (TENSE-OR-ASPECT? REGRET.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REGRET.V (TENSE-OR-ASPECT? REGRET.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REGRET.V (TENSE-OR-ASPECT? REGRET.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV REGRET.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV REGRET.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REJOICE.V (TENSE-OR-ASPECT? REJOICE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REJOICE.V (TENSE-OR-ASPECT? REJOICE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REMEMBER.V (TENSE-OR-ASPECT? REMEMBER.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REMEMBER.V (TENSE-OR-ASPECT? REMEMBER.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REMEMBER.V (TENSE-OR-ASPECT? REMEMBER.V)) (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REMIND.V (TENSE-OR-ASPECT? REMIND.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (PROBABLY (!2 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RESTRAIN-FROM.V (TENSE-OR-ASPECT? RESTRAIN-FROM.V))
                      (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RESTRAIN-FROM.V (TENSE-OR-ASPECT? RESTRAIN-FROM.V))
                      (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RESUME.V (TENSE-OR-ASPECT? RESUME.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! RESUME.V (TENSE-OR-ASPECT? RESUME.V)) (KA _!3)))
                    (PAST (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REVEAL.V (TENSE-OR-ASPECT? REVEAL.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! REVEAL.V (TENSE-OR-ASPECT? REVEAL.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV REVEAL.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV REVEAL.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! SCARE.V (TENSE-OR-ASPECT? SCARE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! SCARE.V (TENSE-OR-ASPECT? SCARE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV SCARE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV SCARE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SCOLD.V (TENSE-OR-ASPECT? SCOLD.V))
                      (! (+ (!2 TERM?) (THAT _!3)) ((!2 TERM?) (THAT _!3)))))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SCOLD.V (TENSE-OR-ASPECT? SCOLD.V))
                      (! (+ (!2 TERM?) (THAT _!3)) ((!2 TERM?) (THAT _!3)))))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SEE.V (TENSE-OR-ASPECT? SEE.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! SHOCK.V (TENSE-OR-ASPECT? SHOCK.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! SHOCK.V (TENSE-OR-ASPECT? SHOCK.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV SHOCK.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV SHOCK.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SHOW.V (TENSE-OR-ASPECT? SHOW.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! START.V (TENSE-OR-ASPECT? START.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! STOP.V (TENSE-OR-ASPECT? STOP.V)) (KA _!3)))
                    (PAST (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! STOP.V (TENSE-OR-ASPECT? STOP.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SUCCEED-IN.V (TENSE-OR-ASPECT? SUCCEED-IN.V))
                      (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SUCCEED-IN.V (TENSE-OR-ASPECT? SUCCEED-IN.V))
                      (KA _!3)))
                    (NOT (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! SURPRISE.V (TENSE-OR-ASPECT? SURPRISE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! SURPRISE.V (TENSE-OR-ASPECT? SURPRISE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV SURPRISE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV SURPRISE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! SUSPECT.V (TENSE-OR-ASPECT? SUSPECT.V)) (THAT _!3)))
                    (PROBABLY _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! TEND.V (TENSE-OR-ASPECT? TEND.V)) (KA _!3)))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV TOUCH.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV TOUCH.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! TOUCH.V (TENSE-OR-ASPECT? TOUCH.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! TOUCH.V (TENSE-OR-ASPECT? TOUCH.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV TROUBLE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV TROUBLE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! TROUBLE.V (TENSE-OR-ASPECT? TROUBLE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! TROUBLE.V (TENSE-OR-ASPECT? TROUBLE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! TURN-OUT.V (TENSE-OR-ASPECT? TURN-OUT.V)) (!2 TERM?)))
                    (!1 IS-A !2)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! TURN-OUT.V (TENSE-OR-ASPECT? TURN-OUT.V)) (!2 TERM?)))
                    (NOT (!1 IS-A !2))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! UNDERSTAND.V (TENSE-OR-ASPECT? UNDERSTAND.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! UNDERSTAND.V (TENSE-OR-ASPECT? UNDERSTAND.V))
                      (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV UNDERSTAND.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV UNDERSTAND.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/
                    ((THAT _!3)
                     ((! UNNERVE.V (TENSE-OR-ASPECT? UNNERVE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/
                    ((THAT _!3)
                     ((! UNNERVE.V (TENSE-OR-ASPECT? UNNERVE.V)) (!1 TERM?)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '+ :RULE
                  '(/ ((!1 TERM?) ((PASV UNNERVE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S-PRS :POLARITY '- :RULE
                  '(/ ((!1 TERM?) ((PASV UNNERVE.V) (THAT _!3))) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! USE.V (TENSE-OR-ASPECT? USE.V))
                      (! (+ (!2 TERM?) (KA _!3)) ((!2 TERM?) (KA _!3)))))
                    (!1 _!3)))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! USED-TO.V (TENSE-OR-ASPECT? USED-TO.V)) (KA _!3)))
                    (PAST (!1 _!3))))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/
                    ((!1 TERM?)
                     ((! VERIFY.V (TENSE-OR-ASPECT? VERIFY.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '+ :RULE
                  '(/ ((THAT _!3) (PASV VERIFY.V)) _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/
                    ((!1 TERM?)
                     ((! WARN.V (TENSE-OR-ASPECT? WARN.V)) (THAT _!3)))
                    _!3))
   (MAKE-INSTANCE 'IMPLICATIVE-RULE-TTT :TYPE 'S :POLARITY '- :RULE
                  '(/ ((THAT _!3) (PASV WARN.V)) _!3))))
