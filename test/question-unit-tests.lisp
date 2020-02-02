;;; Gene Kim 10-26-2018
;;;
;;; Unite tests for functinos in question-inferences.lisp.

(in-package :ulf-pragmatics)

(defun qpresup-n-sub (ulf)
  (multiple-value-bind (_ res) 
    (ulf:apply-sub-macro
      (car (infer-presuppositions-from-wh-q-raw ulf))
      :calling-package *package*)
    (funcall
      (util:compose
        #'trivial-output-normalization 
        #'fix-indefinite-articles
        #'filter-for-some-reason
        #'poss-postmod-to-have! 
        #'clean-when-infs! 
        #'remove-redundant-do) 
      res)))

;``````````````````````````````````````
; Where did John go? -> You probably know where John went
;                    -> I probably don't know where John went
; When are you getting married?
;   -> You probably know when you are getting married
;   -> I probably don't know when you are getting married
; Did that happen? -> You probably know whether that happened
;                  -> I probably don't know whether that happened.
(define-test question-act-test
  "Tests for question act inferences"
  (:tag :q-act)
  (let (;; Where did John go
        (f1 '((sub where.pq ((past do.aux-s) |John| (go.v *h))) ?))
        ;; When are you getting married?
        (f2 '((sub when.pq ((pres prog) you.pro ((get.v married.a) *h))) ?))
        ;; Did that happen?
        (f3 '(((past do.aux-s) that.pro happen.v) ?))
        ;; Which dog did you take?
        (f4 '((sub (Which.d dog.n) ((past do.aux-s) you.pro (take.v *h))) ?)))
    (assert-equal
      '(you.pro probably.adv-s ((pres know.v) (ans-to (sub where.pq (|John| ((past do.aux-s) (go.v *h)))))))
      (car (infer-you-know-from-wh-q-act-raw f1)))
    (assert-equal
      '(I.pro probably.adv-s ((pres do.aux-s) not (know.v (ans-to (sub where.pq (|John| ((past do.aux-s) (go.v *h))))))))
      (car (infer-i-not-know-from-wh-q-act-raw f1)))
    ; TODO: use the do.aux-s eliminated version for the more comprehensive tests. This function occurs before the extra do.aux-s deletion.
    ;(assert-equal
    ;  '(you.pro probably.adv-s ((pres know.v) (ans-to (sub where.pq (|John| ((past go.v) *h))))))
    ;  (car (infer-you-know-from-wh-q-act-raw f1)))
    ;(assert-equal
    ;  '(I.pro probably.adv-s ((pres do.aux-s) not (know.v (ans-to (sub where.pq (|John| ((past go.v) *h)))))))
    ;  (car (infer-i-not-know-from-wh-q-act-raw f1)))


    (assert-equal
      '(you.pro probably.adv-s ((pres know.v) (ans-to (sub when.pq (you.pro ((pres prog) ((get.v married.a) *h)))))))
      (car (infer-you-know-from-wh-q-act-raw f2)))
    (assert-equal
      '(i.pro probably.adv-s 
        ((pres do.aux-s) not 
         (know.v (ans-to (sub when.pq (you.pro ((pres prog) ((get.v married.a) *h))))))))
      (car (infer-i-not-know-from-wh-q-act-raw f2)))

    (assert-equal
      '(you.pro probably.adv-s ((pres know.v) (whether (that.pro ((past do.aux-s) happen.v)))))
      (car (infer-you-know-from-yn-q-act-raw f3)))
    (assert-equal
      '(i.pro probably.adv-s ((pres do.aux-s) not (know.v (whether (that.pro ((past do.aux-s) happen.v))))))
      (car (infer-i-not-know-from-yn-q-act-raw f3)))
    ; TODO: use the do.aux-s eliminated version for the more comprehensive tests. This function occurs before the extra do.aux-s deletion.
    ;(assert-equal
    ;  '(you.pro probably.adv-s ((pres know.v) (whether (that.pro (past happen.v)))))
    ;  (car (infer-you-know-from-yn-q-act-raw f3)))
    ;(assert-equal
    ;  '(i.pro probably.adv-s ((pres do.aux-s) not (know.v (whether (that.pro (past happen.v))))))
    ;  (car (infer-i-not-know-from-yn-q-act-raw f3)))))

    (assert-equal
      '(you.pro probably.adv-s ((pres know.v) (ans-to (sub (which.d dog.n) (you.pro ((past do.aux-s) (take.v *h)))))))
      (car (infer-you-know-from-wh-q-act-raw f4)))
    (assert-equal
      '(i.pro probably.adv-s 
        ((pres do.aux-s) not 
         (know.v (ans-to (sub (which.d dog.n) (you.pro ((past do.aux-s) (take.v *h))))))))
      (car (infer-i-not-know-from-wh-q-act-raw f4)))))


(define-test question-presupposition-test
  "Tests for question presupposition inferences"
  (:tag :q-presup)
  (let
    (
     ;; Who did you meet yesterday?
     (f1 
       '((sub who.pro
              ((past do.aux-s) you.pro
               (meet.v *h yesterday.adv-e))) ?))
     ;; What is Betsy Ross famous for?
     (f2
       '((sub what.pro
              ((pres be.v) | Betsy Ross|
               (famous.a (for.p-arg *h)))) ?))
     ;; When did Beethoven die?
     (f3
       '((sub when.pq
              ((past do.aux-s) | Beethoven| (die.v *h))) ?))
     ;; How many people did Randy Craft kill?
     (f4
       '((sub ((nquan (how.mod-a many.a)) (plur person.n))
              ((past do.aux-s) | Randy Craft| (kill.v *h))) ?))
     ;; What is the official animal of Canada?
     (f5
       '((what.pro ((pres be.v) (= (the.d (n+preds (official.a animal.n)
                                                   (of.p | Canada|)))))) ?))
     ;; What is the abbreviation for micro?
     (f6
       '((what.pro ((pres be.v) 
                    (= (the.d (n+preds abbreviation.n 
                                       (for.p-arg (|"| micro.a |"|))))))) ?))
     ;; When are you getting married?
     (f7
       '((sub when.pq
              ((pres prog) you.pro
               (get.v married.a *h))) ?))
     ;; What is your dog’s name?
     (f8
       '((what.pro
           ((pres be.v) (= (((your.d dog.n) 's) name.n)))) ?))
     ;; Why did you go home?
     (f9
       '((sub why.adv-s 
              ((past do.aux-s) you.pro (go.v (k home.n) *h))) ?))
     ;; In which city does John live?
     (f10
       '((sub (adv-e (in.p (which.d city.n)))
              ((pres do.aux-s) | John| (live.v *h))) ?))
     ;; Whose party are you going to?
     (f11
       '((sub (whose.d party.n) ((pres prog) you.pro (go.v (adv-a (to.p *h))))) ?))
     ;; What were the main reasons why Ophelia went mad?
     (f12
       '((sub what.pro
              ((past be.v) 
               (the.d (main.a (n+preds (plur reason.n)
                                       (= (ans-to (why.adv-s
                                                    (| Ophelia| ((past go.v) mad.a))))))))
               (= *h))) ?))
     ) ; end of let variables.

  ;; Who did you meet yesterday? ⇒ You met someone yesterday
  (assert-equal
    '(you.pro ((past meet.v) someone.pro yesterday.adv-e))
    (qpresup-n-sub f1))
  ;; What is Betsy Ross famous for? ⇒ Betsy Ross is famous for something
  (assert-equal
    '(| Betsy Ross| ((pres be.v) (famous.a (for.p-arg something.pro))))
    (qpresup-n-sub f2))
  ;; When did Beethoven die? ⇒ Beethoven died
  (assert-equal
    '(| Beethoven| (past die.v))
    (qpresup-n-sub f3))
  ;; How many people did Randy Craft kill? ⇒ Randy Craft killed multiple people
  (assert-equal
    '(| Randy Craft| ((past kill.v) (some.d (plur person.n))))
    (qpresup-n-sub f4))
  ;; What is the official animal of Canada? ⇒ Canada has an official animal
  (assert-equal
    '(| Canada| ((pres have.v) (an.d (official.a animal.n))))
    (qpresup-n-sub f5))
  ;; What is the abbreviation for micro? ⇒ Micro has an abbreviation
  (assert-equal
    '((|"| micro.a |"|) ((pres have.v) (an.d abbreviation.n)))
    (qpresup-n-sub f6))
  ;; When are you getting married? ⇒ You will get married in the near future
  (assert-equal
    '(you.pro ((pres will.aux-s) (get.v married.a (adv-e (in.p (the.d (near.a future.n)))))))
    (qpresup-n-sub f7))
  ;; What is your dog’s name? ⇒ Your dog has a name
  (assert-equal
    '((your.d dog.n) ((pres have.v) (a.d name.n)))
    (qpresup-n-sub f8))
  ;; Why did you go home? ⇒ You went home for some reason
  (assert-equal
    '(you.pro ((past go.v) (k home.n)))
    (qpresup-n-sub f9))
  ;; In which city does John live? ⇒ John lives in a city
  (assert-equal
    '(| John| ((pres live.v) (adv-e (in.p (a.d city.n)))))
    (qpresup-n-sub f10))
  ;; Whose party are you going to? ⇒ You are going to someone’s party
  (assert-equal
    '(you.pro ((pres prog) (go.v (adv-a (to.p ((someone.pro 's) party.n))))))
    (qpresup-n-sub f11))
  ;; What were the main reasons why Ophelia went mad? ⇒ Ophelia went mad for some reasons 
  (assert-equal
    ;'(| Ophelia| ((past go.v) mad.a
    ;                          (adv-s (for.p (some.d (plur reason.n))))))
    '(| Ophelia| ((past go.v) mad.a))
    (qpresup-n-sub f12))
  ))

