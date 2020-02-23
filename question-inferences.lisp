;;; Gene Kim 10-26-2018
;;;
;;; Inferences for Questions

(in-package :ulf-pragmatics)

(defparameter *infer-nomq-from-yn-q-act-mods*
;````````````````````````````````````````
; Did he go? -> whether he did go
; Will he go? -> whether he will go
'(/ ((+ sent-mod? sent? tensed-sent? ~ wh-word?)
     ((!5 (lex-tense? verbaux?)) ; inverted verb/aux
      (?1 not not.adv-s never.adv-f never.adv-s) ;
      _!2 ; subj.
      (?3 not not.adv-s never.adv-f never.adv-s) ; possible negation
      _+4) ; verb phrase
     (? [?]))
    (whether (_!2 (!5 ?1 _+4)))))

(defparameter *infer-nomq-from-yn-q-act-top-level*
'(/ ((* sent-mod? sent? tensed-sent? ~ wh-word?)
     (? please.adv-s)
     ((? please.adv-s) ; TODO: we should probably just filter out please before doing this inference...
      (!5 (lex-tense? verbaux?)) ; inverted verb/aux
      (?1 not not.adv-s never.adv-f never.adv-s) ;
      _!2 ; subj.
      (?6 please.adv-s)
      (?3 not not.adv-s never.adv-f never.adv-s) ; possible negation
      _+4
      (?7 please.adv-s)
      ) ; verb phrase
     [?])
    (whether (_!2 (!5 ?1 _+4)))))


(defun nomq-from-yn-q-act? (tree)
  (let ((res1 (ttt:apply-rule *infer-nomq-from-yn-q-act-mods* tree :shallow t :max-n 1))
        (res2 (ttt:apply-rule *infer-nomq-from-yn-q-act-top-level* tree :shallow t :max-n 1)))
    (or (not (equal res1 tree))
        (not (equal res2 tree)))))

(defun nomq-from-yn-q-act! (tree)
  (let ((res1 (ttt:apply-rule *infer-nomq-from-yn-q-act-mods* tree :shallow t :max-n 1))
        (res2 (ttt:apply-rule *infer-nomq-from-yn-q-act-top-level* tree :shallow t :max-n 1)))
    (cond
      ((not (equal res1 tree)) res1)
      (t res2))))

(defparameter *infer-you-know-from-yn-q-act*
;`````````````````````````````````````````````````
; Did he go? -> You know whether he did go -> You know whether he went (this last step can be post-processed -- remove unnecessary do.aux-s)
; Will he go? -> You know whether he will go
  '(/ (!1 nomq-from-yn-q-act?)
      (you.pro probably.adv-s
               ((pres know.v)
                (nomq-from-yn-q-act! !1)))))

(defparameter *infer-i-not-know-from-yn-q-act*
;`````````````````````````````````````````````````
; Did he go? -> You know whether he did go -> You know whether he went (this last step can be post-processed -- remove unnecessary do.aux-s)
; Will he go? -> You know whether he will go
  '(/ (!1 nomq-from-yn-q-act?)
      (i.pro
        ; probably.adv-s ; This isn't common, so remove.
               ((pres do.aux-s) not
                (know.v (nomq-from-yn-q-act! !1))))))

(defun wh-atom? (x)
  (member x '(what.pro when.pq where.pq where.a who.pro whom.pro how.adv-s why.adv-s how.adv-a)))

;; TODO: move this and a bunch of other "wh" stuff to ULF library.
(defvar *wh2some-alist*
  '((who . someone)
    (what . something)
    ;(where . (at.p (some.d place.n)))
    (where . somewhere)
    (when . (at.p (some.d time.n)))
    (how . somehow)
    (why . (for.p (some.d reason.n)))
    ;; TODO: change some to a/an
    ;(which . some)
    (which . a)
    (whose . (someone.pro 's))))

; Parameter to enforce that wh2some always maps to "some" rather than a more
; natural reading such as "a".
(defparameter *enforce-some-in-wh2some* nil)
(defvar *some-enforced-wh2some-alist*
  '((which . some)))

(defun wh-word? (x)
  (and (atom x)
       (ulf:has-suffix? x)
       (multiple-value-bind (word suffix) (ulf:split-by-suffix x)
         (declare (ignore suffix))
         (not (null (assoc word *wh2some-alist*))))))

(defun non-term-wh-word? (x)
  (member x '(when.pq where.pq where.a how.adv-a why.adv-s)))

(defun wh-q-front? (x)
  (or (and (atom x) (wh-atom? x))
      ;; wh-word operating on something.
      (and (listp x) (= (length x) 2)
           (member (first x) '(which.d what.d how.adv-a whose.d)))
      ;; NB: can add a conditions on operands if we want to be robust to bad ULFs.
      ;; whose -> (who.pro 's)
      (and (listp x) (eql (length x) 2)
           (listp (first x)) (eql (length (first x)) 2)
           (equal (caar x) 'who.pro)
           (equal (cadar x) '|'s|))))


(defparameter *infer-nomq-from-wh-q-act*
;`````````````````````````````````````````````````
; Where did you go? -> where you did go
; Who did you meet? -> who you did meet
; TODO: which dog did you pet? -> which dog you did pet; how long did you stay; what day is it
; TODO: in what car did you go?
  '(/ ((sub (!.7 wh-q-front?)
       (((!.5 lex-tense?) (!.6 verbaux?)) ; inverted verb/aux
        ;(?1 not not.adv-s never.adv-f never.adv-s) ;
        _!.2 ; subj.
        (?.3 not not.adv-s never.adv-f never.adv-s) ; possible negation
        _+.4)) ; verb phrase
       (? [?]))
      (ans-to (sub !.7 (_!.2 ?.3 ((!.5 !.6) _+.4))))))
(defparameter *infer-nomq-from-wh-q-act-nosub*
; Who kept the books? -> who kept the books
; Who left? -> who left
  '(/ (((!.7 wh-q-front? ~ non-term-wh-word?)
        (!.2 ((lex-tense? verbaux?)
             (?.1 not not.adv-s never.adv-f never.adv-s) ; possible negation
             _+)
            (lex-tense? verbaux?))) ; intransitive
        (?.2 [?])) ; args and mods.
       (ans-to (!.7 !.2))))
(defparameter *infer-nomq-from-wh-q-act-non-term-wh*
; Why did you go -> why he did go
; How did they know -> How they did know
  '(/ (((!.7 non-term-wh-word?)
        ((!.2 (lex-tense? verbaux?))
         (?.1 not not.adv-s never.adv-f never.adv-s) ; TODO: put this neg. in a function.
         _!.3 ; subj
         (?.4 not not.adv-s never.adv-f never.adv-s)
         _+.5))
       (?.6 [?]))
      (ans-to (!.7 (_!.3 (!.2 ?.1 ?.4 _+.5))))))

(defun nomq-from-wh-q-act? (tree)
  (let ((res1 (ttt:apply-rule *infer-nomq-from-wh-q-act* tree :shallow t :max-n 1))
        (res2 (ttt:apply-rule *infer-nomq-from-wh-q-act-nosub* tree :shallow t :max-n 1))
        (res3 (ttt:apply-rule *infer-nomq-from-wh-q-act-non-term-wh* tree :shallow t :max-n 1)))
    (or (not (equal res1 tree))
        (not (equal res2 tree))
        (not (equal res3 tree)))))
;; This assumes that nomq-from-wh-q-act? was already called to check for appropriateness.
(defun nomq-from-wh-q-act! (tree)
  (let ((res1 (ttt:apply-rule *infer-nomq-from-wh-q-act* tree :shallow t :max-n 1))
        (res2 (ttt:apply-rule *infer-nomq-from-wh-q-act-nosub* tree :shallow t :max-n 1))
        (res3 (ttt:apply-rule *infer-nomq-from-wh-q-act-non-term-wh* tree :shallow t :max-n 1)))
    (cond
      ((not (equal res1 tree)) res1)
      ((not (equal res2 tree)) res2)
      (t res3))))

(defparameter *infer-you-know-from-wh-q-act*
;`````````````````````````````````````````````````
; Where did you go? -> You know where you did go -> You know where you went
; Who did you meet? -> You know who you did meet -> You know who you met
  '(/ (! nomq-from-wh-q-act?)
      (you.pro probably.adv-s
               ((pres know.v)
                (nomq-from-wh-q-act! !)))))

(defparameter *infer-i-not-know-from-wh-q-act*
  '(/ (! nomq-from-wh-q-act?)
      (i.pro
        ; probably.adv-s ; not common, so remove
             ((pres do.aux-s) not
               (know.v
                (nomq-from-wh-q-act! !))))))


(defun find-wh-word (ulf)
;```````````````````````
; Finds the wh-phrase in this ULF (anywhere that could form a wh-question).
; That is, the wh-word isn't embedded within an ans-to or a quote.
  (labels
    ((recfn (curulf)
       (cond
         ((wh-word? curulf) (list curulf))
         ((atom curulf) nil)
         ((ttt:match-expr '(!1 (ans-to _!2) (|"| _+3 |"|)) curulf) nil)
         ((listp curulf) (apply #'append (mapcar #'recfn curulf)))
         (t nil))))
    (let ((res (recfn ulf)))
      (if res (first res) res))))
(util:memoize 'find-wh-word)


(defun contains-wh? (ulf)
  (find-wh-word ulf))


(defun wh-q? (ulf)
;`````````````````
; Returns whether the given ULF is a wh-question.
  (and (listp ulf) (= (length ulf) 2)
       (member (second ulf) '(? [?]))
       (find-wh-word ulf)))


(defun wh2some! (ulf)
;````````````````````
; Converts a wh-word to a some-word.
; who.pro -> someone.pro
; why.adv-s -> (adv-s (for.p (some.d reason.n)))
; how.adv-a -> somehow.adv-a
; which.d -> some.d
; whose.d -> (someone.pro 's)
  (if (not (and (atom ulf) (ulf:has-suffix? ulf))) (return-from wh2some! nil))
  (let (some-version)
    (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
      (setf some-version (cdr (assoc word *wh2some-alist*)))
      (when (and *enforce-some-in-wh2some*
                 (assoc word *some-enforced-wh2some-alist*))
        (setf some-version (cdr (assoc word *some-enforced-wh2some-alist*))))
      (if (not some-version) (return-from wh2some! nil))
      ; Generate the result or by adding suffix to some-version of the word, or
      ; handle some special cases.
      (cond
        ((and (eql 'd suffix) (eql 'something some-version)) 'some.d)
        ((atom some-version) (ulf:add-suffix some-version suffix))
        ((equal '(someone.pro 's) some-version) some-version)
        ((and (eql 'pq suffix) (member word '(when where)))
         (list 'adv-e some-version))
        ((and (eql 'pq suffix) (member word '(how)))
         (list 'adv-a some-version))
        ((or (ulf:advformer? suffix) (ulf:modformer? suffix))
         (list suffix some-version))
        (t ulf)))))


(defun wh2some-sent! (ulf)
;````````````````````
; Replaces the wh-phrase in ULF with the corresponding 'some' phrase.
; E.g. (who.pro ((past go.v) there.adv-e))
;      -> (someone.pro ((past go.v) there.adv-e))
  (let* ((whw (find-wh-word ulf))
         (smw (wh2some! whw)))
    (subst smw whw ulf)))

(defparameter *uninvert-be*
  '(/ ((!1 be.v (lex-tense? be.v))
       (*1 phrasal-sent-op?)
       (!2 term?)
       (*2 phrasal-sent-op?)
       (!3 pred?))
      (!2 (!1 *1 *2 !3))))
(defparameter *uninvert-aux*
  '(/ ((!1 (lex-tense? (! aux? do.v)))
       (*1 phrasal-sent-op?)
       (!2 term?)
       (*2 phrasal-sent-op?)
       (!3 verb?))
      (!2 (!1 *1 *2 !3))))
(defparameter *uninvert-have*
  '(/ ((!1 (lex-tense? have.v))
       (*1 phrasal-sent-op?)
       (!2 term?)
       (*2 phrasal-sent-op?)
       (!3 term?))
      (!2 (!1 *1 *2 !3))))
(defparameter *uninvert-exist-there*
  '(/ ((!1 (lex-tense? lex-verb?))
       there.pro
       (*1 phrasal-sent-op?)
       (!2 term?)
       (*2 phrasal-sent-op?))
      (there.pro (!1 *1 !2 *2))))

(defun uninvert-sent! (ulf)
  (ttt:apply-rules
    (list *uninvert-be*
          *uninvert-aux*
          *uninvert-have*
          *uninvert-exist-there*)
    ulf :max-n 100 :rule-order :slow-forward))


(defun tttsubst! (subval orgval tree)
  (subst subval orgval tree :test #'equal))

(defun apply-sub-macros! (ulf)
  (multiple-value-bind (_  res)
    (ulf:apply-sub-macro (util:unhide-ttt-ops ulf)
                         :calling-package :ulf-pragmatics)
    (declare (ignore _))
    res))

;; Parameters and function for removing "at some time" and "pres prog" in when
;; question inferences. These are not informative so people tend to omit these,
;; technically correct, descriptions.
(defparameter *postproc-past-at-some-time*
  '(/ ((!1 (past lex-verbaux?)) _*2 (adv-e (at.p (some.d time.n))) _*3)
      (!1 _*2 _*3)))
(defparameter *postproc-pres-prog-at-some-time*
  '(/ ((!1 (pres prog)) ; pres prog
       _*2
       (!2 (adv-e (at.p (some.d time.n)))      ; "at some time" clause here or as child.
           (^ (adv-e (at.p (some.d time.n)))))
       _*3)
      ((pres will.aux-s)
       _*2
       (tttsubst! (adv-e (in.p (the.d (near.a future.n))))
                  (adv-e (at.p (some.d time.n)))
                  !2)
       _*3)))
(defun clean-when-infs! (ulf)
  (ttt:apply-rules (list *postproc-past-at-some-time*
                         *postproc-pres-prog-at-some-time*)
                   ulf
                   :rule-order :slow-forward :max-n 1000))


;; TODO: move all the poss-det stuff below to ulf-lib
(defun replace-x (xs pair)
  (cond
   ((null xs) nil)
   ((atom xs)
    (if (equal xs (first pair)) (second pair) xs))
   (t (mapcar #'(lambda (x) (replace-x x pair))
              xs))))

(defparameter *poss-det-mappings*
    '((my.d (me.pro 's))
      (your.d (you.pro 's))
      (his.d (he.pro 's))
      (her.d (her.pro 's))
      (its.d (it.pro 's))
      (our.d (we.pro 's))
      (their.d (they.pro 's))
      (one's.d (one.pro 's))))

(defun expand-poss-det (f)
  (reduce #'replace-x *poss-det-mappings* :initial-value f))

(defparameter *poss-pro-mappings*
    '((mine.pro (me.pro 's))
      (yours.pro (you.pro 's))
      (his.pro (he.pro 's))
      (hers.pro (she.pro 's))
      (its.pro (it.pro 's))
      (ours.pro (us.pro 's))
      (one's.pro (one.pro 's))))

(defun expand-poss-pro (f)
  (reduce #'replace-x *poss-pro-mappings* :initial-value f))


(defparameter *lex-poss-dets*
  '(my your his her its our their))
(defun poss-det? (ulf)
  (cond
    ((and (atom ulf) (ulf:has-suffix? ulf))
     (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
       (and (eql 'd suffix) (member word *lex-poss-dets*))))
    ((and (listp ulf) (= (length ulf) 2))
     (ttt:match-expr '(! (term? 's)
                         (poss-by term?)
                         (mod-n (poss-by term?)))
                     ulf))
    (t nil)))
(defun poss-det-to-possessor! (ulf)
  (if (not (poss-det? ulf)) (return-from poss-det-to-possessor! ulf))
  (cond
      ((atom ulf)
       (let ((expanded (expand-poss-det ulf)))
         (if (equal ulf expanded) ulf (poss-det-to-possessor! expanded))))
      ((ttt:match-expr '(term? 's) ulf) (first ulf))
      ((ttt:match-expr '(poss-by term?) ulf) (second ulf))
      ((ttt:match-expr '(mod-n (poss-by term?)) ulf) (second (second ulf)))
      (t ulf)))

;; possession and noun post-modification to have inferences.
;; e.g.
;;  (something.pro ((pres be.v) (= (his.d dog.n))))
;;  -> (he.pro ((pres have.v) (a.d dog.n)))
;;  (something.pro ((pres be.v) (= (the.d (n+preds insignia.n
;;                                                 (for.p (the.d school.n)))))))
;;  -> ((the.d.school.n) ((pres have.v) (an.d insignia.n)))
(defparameter *poss-postmod-to-have*
  '(
    ;; Possessives.
    ;;  (something.pro ((pres be.v) (= (his.d dog.n))))
    ;;  -> (he.pro ((pres have.v) (a.d dog.n)))
    ;;  (something.pro ((pres be.v) (= ((| John| 's) name.n))))
    ;;  -> (| John| ((pres have.v) (a.d name.n)))
    (/ (something.pro (*1 phrasal-sent-op?)
       (((!1 lex-tense?) be.v)
        (*2 phrasal-sent-op?)
        (= ((!2 poss-det?) (!3 noun?)))
        (*3 phrasal-sent-op?)))
      ((poss-det-to-possessor! !2) *1 *2 *3 ((!1 have.v) (a.d !3))))
    ;; Post-modification.
    ;;  (something.pro ((pres be.v) (= (the.d (n+preds insignia.n
    ;;                                                 (for.p (the.d school.n)))))))
    ;;  -> ((the.d school.n) ((pres have.v) (an.d insignia.n)))
    (/ (something.pro (*1 phrasal-sent-op?)
        (((!1 lex-tense?) be.v)
         (*2 phrasal-sent-op?)
         (= (det? (n+preds (!2 noun?)
                           ((!3 for.p of.p {for}.p {of}.p
                                for.p-arg of.p-arg {for}.p-arg {of}.p-arg)
                            (!4 term?)))))
         (*3 phrasal-sent-op?)))
       (!4 *1 *2 *3 ((!1 have.v) (a.d !2))))))
(defun poss-postmod-to-have! (ulf)
  (ttt:apply-rules *poss-postmod-to-have* ulf
                   :rule-order :slow-forward
                   :max-n 1000))

;; "For some reason" clauses are not very informative so people tend to
;; filter them out.
(defun filter-for-some-reason (ulf)
  (ttt:apply-rule '(/ (_*1 (advformer? (for.p (some.d reason.n))) _*2)
                      (_*1 _*2))
                  ulf :max-n 1000))


(defun indefinite-article? (ulf)
;```````````````````````````````
  (cond
    ((not (atom ulf)) nil)
    (t (member ulf '(a.d an.d some.d)))))

(defun fix-indefinite-article! (ulf)
;```````````````````````````````````
; (a.d abbreviation.n) -> (an.d abbreviation.n)
; (a.d (official.a document.n)) -> (an.d (official.a document.n))
  (cond
    ((ttt:match-expr '(indefinite-article? noun?) ulf)
     (let ((front-word (first (remove-if-not #'ulf2english::is-surface-token?
                                             (mapcar #'ulf:make-explicit!
                                                     (alexandria:flatten (second ulf))))))
           raw-ia new-ia)
       (setq raw-ia (util:indefinite-article
                      (ulf2english front-word
                                   :add-punct? nil
                                   :capitalize-front? nil)))
       (setq new-ia
             (ulf:add-suffix
               (safe-intern raw-ia (symbol-package (first ulf)))
               'd))
       (list new-ia (second ulf))))
    (t ulf)))
(defparameter *fix-indefinite-article*
  '(/ (! (indefinite-article? noun?))
      (fix-indefinite-article! !)))
(defun fix-indefinite-articles (ulf)
  (ttt:apply-rule *fix-indefinite-article* ulf :max-n 1000))


(defparameter *infer-presuppositions-from-wh-q*
;``````````````````````````````````````````````
; Who went there? -> Someone went there
; Where did you go? -> You went somewhere
; What did you buy? -> You bought something
; When did he leave? -> He left (at some time)
; How does he do that? -> He does that (somehow)
; Why did he say that? -> He said that
; Which cup did he use? -> He used a cup
; Whose dog are you petting? -> You're petting someone's dog
  '(/ ((!1 contains-wh?) [?])
      (uninvert-sent! (apply-sub-macros! (wh2some-sent! !1)))))


(defparameter *infer-presuppositions-from-embedded-wh-q*
;```````````````````````````````````````````````````````
; He asked who went there -> someone went there
; What's the reason why you did that? -> you did that (for some reason)
  '(/ (^* (ans-to (!1 contains-wh?)))
      (uninvert-sent! (apply-sub-macros! (wh2some-sent! !1)))))


(defun infer-you-know-from-yn-q-act (ulf)
  (all-ttt-rule-inf-result *infer-you-know-from-yn-q-act* ulf))
(defun infer-i-not-know-from-yn-q-act (ulf)
  (all-ttt-rule-inf-result *infer-i-not-know-from-yn-q-act* ulf))
(defun infer-you-know-from-wh-q-act (ulf)
  (all-ttt-rule-inf-result *infer-you-know-from-wh-q-act* ulf))
(defun infer-i-not-know-from-wh-q-act (ulf)
  (all-ttt-rule-inf-result *infer-i-not-know-from-wh-q-act* ulf))
(defun infer-presuppositions-from-wh-q (ulf)
  (append
    (all-ttt-rule-inf-result *infer-presuppositions-from-embedded-wh-q* ulf
                             :shallow t :max-per-level 100)
    (all-ttt-rule-inf-result *infer-presuppositions-from-wh-q* ulf)))

; [raw variants]
; The *-raw variants of the rules return lists of formulas instead of
; 'inf-result instances.
;
(defun infer-you-know-from-yn-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-you-know-from-yn-q-act ulf)))
(defun infer-i-not-know-from-yn-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-i-not-know-from-yn-q-act ulf)))
(defun infer-you-know-from-wh-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-you-know-from-wh-q-act ulf)))
(defun infer-i-not-know-from-wh-q-act-raw (ulf)
  (mapcar #'result-formula
          (infer-i-not-know-from-wh-q-act ulf)))
(defun infer-presuppositions-from-wh-q-raw  (ulf)
  (mapcar #'result-formula
          (infer-presuppositions-from-wh-q ulf)))

(defun premacro-q-inferences (ulf)
  (cdar (results-from-applying-rules
          (list #'infer-you-know-from-yn-q-act
                #'infer-i-not-know-from-yn-q-act
                #'infer-you-know-from-wh-q-act
                #'infer-i-not-know-from-wh-q-act
                #'infer-presuppositions-from-wh-q)
          (list ulf) t)))

(defun postmacro-q-inferences (ulf)
  (cdar (results-from-applying-rules
          nil
          (list ulf) t)))

