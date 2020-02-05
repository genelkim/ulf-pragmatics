;;; Ben Kane 7-22-2018
;;;
;;; Parsed into TTT rules from Karl Stratos rules from Epilog package using /resources/stratos/parser.lisp.

(in-package :ulf-pragmatics)


;; TTT computable predicate; accepts tense/aspect operators in front of verb
;; NOTE: Can be restricted to just "past" and "pres"
;; GK: I renamed this so it doesn't get confused with lex-tense? in ulf-lib.
(DEFUN TENSE-OR-ASPECT? (L) (IF (MEMBER L '(PAST PRES PERF PROG)) T NIL))

;; Define class for implicative rules containing type ('S or 'S-PRS), polarity ('+ or '-) and the rule
(DEFCLASS IMPLICATIVE-RULE-TTT NIL
          ((TYPE :INITARG :TYPE :INITFORM 'S)
           (POLARITY :INITARG :POLARITY :INITFORM '+)
           (RULE :INITARG :RULE)))

(defparameter *INFER-FROM-IMPLICATIVE-RULES*
  (append
    (LIST
     (make-instance 'implicative-rule-ttt :type 's-prs :polarity '+ :rule
                    '(/ ((!1 TERM?)
                          ;; TODO: WHHHHHHYYYYYYYYYYYYYYYY DOES ONLY ? WORK HERE! WHY NOT '!'
                          ;; TODO: look at older version of TTT and see if it works. If so, figure out the semantic difference in the code.
                         ((! inform.v (tense-or-aspect? inform.v))
                          _!2 (that _!3)))
                        _!3))
     (make-instance 'implicative-rule-ttt :type 's-prs :polarity '- :rule
                    '(/ ((!1 TERM?)
                         ((! inform.v (tense-or-aspect? inform.v))
                          _!2 (that _!3)))
                        _!3)))
    *INFER-FROM-IMPLICATIVE-RULES*))



;; Matches (! X.v (tense-or-aspect? X.v))
(defun implicative-rule-verb? (x)
  (and (listp x) (= 3 (length x))
       (eql '! (first x))
       (listp (third x))
       (= 2 (length (third x)))
       (equal 'tense-or-aspect? (first (third x)))
       (equal (second x) (second (third x)))))

(defun implicative-rule-verb-to-notense (x)
  (if (implicative-rule-verb? x)
    (second x)
    x))
(defun implicative-rule-verb-to-onlytense (x)
  (if (implicative-rule-verb? x)
    (third x)
    x))

;; Map the rules to simpler rules because there's a bug in TTT for these patterns...
(defparameter *infer-from-implicative-rules-notense*
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
    *infer-from-implicative-rules*))
(defparameter *infer-from-implicative-rules-onlytense*
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
    *infer-from-implicative-rules*))


;; Applies implicative rules to ulf depending on polarity context
(defun apply-implicative-rules (ulf)
  (ttt:apply-rules
    (mapcar (lambda (x)
              (if (equalp (slot-value x 'polarity)
                          (get-implicative-polarity ulf))
                (slot-value x 'rule)))
            *infer-from-implicative-rules*)
    (remove-aux-not ulf) :shallow t))

(defun inf-result-apply-implicative-rules (ulf)
  (let ((ttt-rules
          (mapcar (lambda (x)
                    (if (equalp (slot-value x 'polarity)
                                (get-implicative-rule-polarity ulf (slot-value x 'rule)))
                      (slot-value x 'rule)))
                  (append
                    *infer-from-implicative-rules-notense*
                    *infer-from-implicative-rules-onlytense*)))
        (preproc-ulf (remove-aux-not ulf)))
    (apply #'append ; flatten one level.
           ;; TODO: check perhaps presuppositional inferences should only be shallow?
           (mapcar #'(lambda (rule) (all-ttt-rule-inf-result rule preproc-ulf))
                   (remove-if #'null ttt-rules)))))

;; Gets segments of implicative ulf and returns polarity
(defun get-implicative-polarity (ulf)
  (let ((seg (get-ulf-segments-vp ulf)))
    (get-segment-polarity (first seg) (second seg) (third seg))))
(util:memoize 'get-implicative-polarity)

(defun get-implicative-rule-polarity (ulf rule)
  (let ((seg (get-ttt-rule-ulf-segments ulf rule)))
    (get-segment-polarity (first seg) (second seg) (third seg))))
;(util:memoize 'get-implicative-rule-polarity)

