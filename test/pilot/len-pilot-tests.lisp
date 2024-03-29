;;; Gene Kim 7-24-2018
;;; Ported from formulas defined by Len to test the pilot inference system.

(in-package :ulf-pragmatics)

;; Macro for running a test using 'run-subset-rules' to reduce the size of the
;; test declarations.
;; Arguments
;;  name:     The name of the test (as a symbol, i.e. unquoted)
;;  sentence: String surface sentence being tested
;;  ulf:      Source ULF
;;  expected: List of expected output ULFs (in order TODO: make the order not
;;            matter)
(defmacro define-len-pilot-subset-test (name sentence tags ulf expected)
  `(define-test ,name
     ,(format nil "'infer-all' test on the sentence '~a'" sentence) 
     (:tag :subset-rules :len-pilot ,@tags)
     (let ((result (run-subset-rules ,ulf))
           (ulf ,ulf)
           (expected ,expected))
        (if expected
          (assert-equal (first result) ulf 
                        (first result) ulf))
        (set-assert-equal (cdr result) expected))))

;; Macro for running a test using 'infer-all' (from the core inference code).
;; Arguments
;;  name:     The name of the test (as a symbol, i.e. unquoted)
;;  sentence: String surface sentence being tested
;;  ulf:      Source ULF
;;  expected: List of expected output ULFs (in order TODO: make the order not
;;            matter)
(defmacro define-len-pilot-infer-all-test (name sentence tags ulf expected)
  `(define-test ,name
     ,(format nil "Subset rule test on the sentence '~a'" sentence) 
     (:tag :infer-all :len-pilot ,@tags)
     (let ((actual (mapcar #'result-formula (old-infer-all ,ulf)))
           (expected ,expected))
        (set-assert-equal actual expected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define tests.
;;
;; This section defines tests specifically for the same subset of rules that
;; were used in the pilot inference task.  First, simple tests are listed, 
;; followed by sentences sampled from Tatoeba.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple tests (thought up).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-len-pilot-subset-test 
  test-subset-request1
  "Would you please speak up"
  (:rq)
  ; NB: This should have a question mark, but we should be robust to this error
  ;     since the inversion makes it clear.
  '((pres would.aux-v) you.pro please.adv-s speak_up.v) 
  '((i.pro ((pres want.v) you.pro (to speak_up.v)))
    (i.pro ((pres expect.v) (that (you.pro (pres speak_up.v)))))))
  
(define-len-pilot-subset-test
  test-subset-request2
  "Can somebody help me?"
  (:rq)
  '(((pres can.aux-v) somebody.pro (help.v me.pro)) ?)
  '((i.pro ((pres want.v) somebody.pro (to (help.v me.pro))))
    (i.pro ((pres expect.v) (that (somebody.pro ((pres help.v) me.pro)))))))

(define-len-pilot-subset-test
  test-subset-request3
  "Please will you close the door when you go out"
  (:rq)
  '((Please.adv-s
      ((pres will.aux-s) you.pro (close.v (the.d door.n)
       (adv-e (when.ps (you.pro ((pres go.v) out.adv-a))))))))
  '((i.pro ((pres want.v)
             you.pro 
             (to (close.v (the.d door.n)
                  (adv-e (when.ps (you.pro ((pres go.v) out.adv-a))))))))
    (i.pro ((pres expect.v)
       (that (you.pro ((pres close.v) (the.d door.n)
              (adv-e (when.ps (you.pro ((pres go.v) out.adv-a)))))))))))

(define-len-pilot-subset-test
  test-subset-request4
  "Could you turn on the light please"
  (:rq)
  '((((pres could.aux-v) you.pro (turn_on.v (the.d light.n)))
     please.adv-s) ?)
  '((i.pro ((pres want.v) you.pro (to (turn_on.v (the.d light.n)))))
    (i.pro ((pres expect.v) (that (you.pro ((pres turn_on.v) (the.d light.n))))))))

(define-len-pilot-subset-test
  test-subset-wish1
  "I wish he would finish it"
  (:cf :wish)
  '(i.pro ((pres wish.v) (tht (he.pro ((cf will.aux-s) (finish.v it.pro))))))
  '((he.pro ((pres will.aux-s) not.adv-s (finish.v it.pro)))))

(define-len-pilot-subset-test
  test-subset-wish2
  "I wish he would leave"
  (:cf :wish)
  '(i.pro ((pres wish.v) (tht (he.pro ((cf will.aux-s) leave.v)))))
  '((he.pro ((pres will.aux-s) not.adv-s leave.v))))

(define-len-pilot-subset-test
  test-subset-if1
  "If I were rich I would travel to Rome"
  (:cf :if-then)
  '((if.ps (I.pro ((cf were.v) rich.a)))
         (i.pro ((cf will.aux-s) (travel.v (to.p-arg |Rome|)))))
  '((i.pro ((pres be.v) not.adv-s rich.a))
    (i.pro ((pres will.aux-s) not.adv-s (travel.v (to.p-arg |Rome|))))))

(define-len-pilot-subset-test
  test-subset-if2
  "If she had a hammer she would swing it"
  (:cf :if-then)
  '((if.ps (she.pro ((cf have.v) (a.d hammer.n))))
           (she.pro ((cf will.aux-s) (swing.v it.pro))))
  '((she.pro ((pres do.aux-s) not.adv-s (have.v (a.d hammer.n))))
    ; NB: Should we keep this inference?  It's a donkey sentence, so maybe not.
    ;     For donkey sentences, the indexical is not meaningful without the
    ;     previous clause.
    ;(she.pro ((pres will.aux-s) not.adv-s (swing.v it.pro)))))
  ))

(define-len-pilot-subset-test
  test-subset-if3
  "If she had had a hammer she would swing it"
  (:cf :if-then)
  '((if.ps (she.pro ((cf perf) (have.v (a.d hammer.n)))))
           (she.pro ((cf will.aux-s) (swing.v it.pro))))
  '((she.pro ((past do.aux-s) not.adv-s (have.v (a.d hammer.n))))
   ;(she.pro ((pres will.aux-s) not.adv-s (swing.v it.pro)))))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tatoeba Sampled Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If-then constructions.

(define-len-pilot-subset-test
  test-subset-if-17577
  "If I were you I would be able to succeed."
  (:cf :if-then)
  '((If.ps (I.pro ((cf were.v) you.pro)))
           (I.pro ((cf will.aux-s) (be.v (able.a (to succeed.v))))))
  '((i.pro ((pres be.v) not.adv-s you.pro))))

; GK(7-31-2018): This requires preprocessing which isn't in the subset rules.
;                This is tested in the full pipeline, 'infer-all' tests.
;(define-len-pilot-subset-test
;  test-subset-if-17955
;  "If you were to fall from that bridge, it would be almost impossible to rescue you."
;  (:cf :if-then)
;  '((If.ps (you.pro ((cf were.v) 
;                     (to (fall.v (adv-a (from.p (that.d bridge.n)))))))) 
;           (it.pro ((cf will.aux-s) (be.v 
;            (almost.adv-a (impossible.a (adv-a (to.p (rescue.v you.pro)))))))))
;  '((you.pro
;       ((pres will.aux-s) not.adv-s
;        (fall.v (adv-a (from.p (that.d bridge.n))))))))

(define-len-pilot-subset-test
  test-subset-if-18636
  "If I had money, I would pay what I owe you."
  (:cf :if-then)
  '((If.ps (I.pro ((cf have.v) (k money.n))))
           (I.pro ((cf will.aux-s) 
                   (pay.v (sub what.pro (I.pro ((pres owe.v) you.pro *h)))))))
  '((i.pro ((pres do.aux-s) not.adv-s (have.v (k money.n))))
    (i.pro ((pres will.aux-s) not.adv-s 
            (pay.v (sub what.pro (I.pro ((pres owe.v) you.pro *h))))))))

(define-len-pilot-subset-test
  test-subset-if-16988
  "If only I had taken your advice."
  (:cf :if-then)
  '((If.ps only.adv-s (I.pro ((cf perf) (take.v (your.d advice.n))))) {ref1}.s)
  '((i.pro ((past do.aux-s) not.adv-s (take.v (your.d advice.n))))))

;;; Inverted if-then constructions.

(define-len-pilot-subset-test
  test-subset-inv-16968
  "Had I known your telephone number, I would have called you"
  (:cf :if-inv)
  '(((cf perf) I.pro (know.v (your.d (telephone.n number.n))))
     (I.pro ((cf will.aux-s) (perf (call.v you.pro))))) 
  '(;(i.pro ((past do.aux-s) not.adv-s (call.v you.pro)))
    ; The inference above is tested in infer-all since it's handled by 
    ; post-processing.
    (i.pro ((past will.aux-s) not.adv-s (call.v you.pro)))
    (i.pro
      ((past do.aux-s) not.adv-s
                       (know.v (your.d (telephone.n number.n)))))))
  
(define-len-pilot-subset-test
  test-subset-inv-16968-v2
  "Had I known your telephone number, I would have called you."
  (:cf :if-inv)
  '(({if}.ps (I.pro ((cf perf) (know.v (your.d (telephone.n number.n))))))
            (I.pro ((cf will.aux-s) (perf (call.v you.pro)))))
  '((i.pro ((past do.aux-s) not.adv-s
     (know.v (your.d (telephone.n number.n)))))
    ;(i.pro ((past do.aux-s) not.adv-s (call.v you.pro)))))
    ; The inference above is tested in infer-all since it's handled by 
    ; post-processing.
    (i.pro ((past will.aux-s) not.adv-s (call.v you.pro)))))

(define-len-pilot-subset-test
  test-subset-inv-18529
  "Were I rich, I would help the poor."
  (:cf :if-inv)
  '((((cf Were.v) I.pro rich.a))
    (I.pro ((cf will.aux-s) (help.v (the.d (poor.a {ref1}.n))))))
  '((i.pro ((pres will.aux-s) not.adv-s (help.v (the.d (poor.a {ref1}.n)))))
    (i.pro ((pres be.v) not.adv-s rich.a))))

(define-len-pilot-subset-test
  test-subset-inv-18529-v2
  "I were rich I would help the poor."
  (:cf :if-inv)
  '(({if}.ps (I.pro ((cf Were.v) rich.a))) 
           (I.pro ((cf will.aux-s) (help.v (the.d (poor.a {ref1}.n))))))
  '((i.pro ((pres be.v) not.adv-s rich.a))
    (i.pro ((pres will.aux-s) not.adv-s (help.v (the.d (poor.a {ref1}.n)))))))

;;; Requests.

(define-len-pilot-subset-test
  test-subset-req-1661
  "Could you dial for me?"
  (:rq)
  '(((pres Could.aux-v) you.pro ((dial.v {ref1}.pro) (adv-a (for.p me.pro)))) ?)
  '((i.pro
      ((pres want.v)
       you.pro (to ((dial.v {ref1}.pro) (adv-a (for.p me.pro))))))
    (i.pro
      ((pres expect.v)
       (that (you.pro (((pres dial.v) {ref1}.pro) (adv-a (for.p me.pro)))))))))
  
(define-len-pilot-subset-test
  test-subset-req-2242
  "Could you please repeat that?"
  (:rq)
  '(((pres Could.aux-v) you.pro please.adv-s (repeat.v that.pro)) ?)
  '((i.pro ((pres want.v) you.pro (to (repeat.v that.pro))))
    (i.pro ((pres expect.v) (that (you.pro ((pres repeat.v) that.pro)))))))

;;; Wish constructions.

(define-len-pilot-subset-test
  test-subset-wish-1393
  "I wish I could go to Japan."
  (:cf :wish)
  '(I.pro ((pres wish.v)
          (tht (I.pro ((pres could.aux-v) ; NB: the incorrect annotation
                        (go.v (adv-a (to.p |Japan|))))))))
  '())

(define-len-pilot-subset-test
  test-subset-wish-1393-v2
  "I wish I could go to Japan."
  (:cf :wish)
  '(I.pro ((pres wish.v)
           (tht (I.pro ((cf can.aux-v) 
                        (go.v (adv-a (to.p |Japan|))))))))
  '((i.pro ((pres can.aux-v) not.adv-s (go.v (adv-a (to.p |Japan|)))))))

(define-len-pilot-subset-test
  test-subset-wish-2470
  "I wish she would stop playing that stupid music."
  (:cf :wish)
  '(I.pro ((pres wish.v)
          (tht (she.pro ((pres would.aux-s) ; NB: the incorrect annotation
                          (stop.v (ka (play.v (that.d (stupid.a music.n))))))))))
  '())

(define-len-pilot-subset-test
  test-subset-wish-2470-v2
  "I wish she would stop playing that stupid music."
  (:cf :wish)
  '(I.pro ((pres wish.v)
          (tht (she.pro ((cf will.aux-s)
                          (stop.v (ka (play.v (that.d (stupid.a music.n))))))))))
  '((she.pro
      ((pres will.aux-s) not.adv-s
                         (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipeline tests.
;;
;; Here to the end of the file defines the integrated versions of the above
;; unit tests.  Here we use the full pipeline system rather than just the
;; appropriate subset of rules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple tests (thought up).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-len-pilot-infer-all-test 
  test-infer-all-request1
  "Would you please speak up"
  (:rq)
  ; NB: This should have a question mark, but we should be robust to this error
  ;     since the inversion makes it clear.
  '((pres would.aux-v) you.pro please.adv-s speak_up.v) 
  '((i.pro ((pres want.v) you.pro (to speak_up.v)))
    (i.pro ((pres expect.v) (that (you.pro (pres speak_up.v)))))))
  
(define-len-pilot-infer-all-test
  test-infer-all-request2
  "Can somebody help me?"
  (:rq)
  '(((pres can.aux-v) somebody.pro (help.v me.pro)) ?)
  '((i.pro ((pres want.v) somebody.pro (to (help.v me.pro))))
    (i.pro ((pres expect.v) (that (somebody.pro ((pres help.v) me.pro)))))))

(define-len-pilot-infer-all-test
  test-infer-all-request3
  "Please will you close the door when you go out"
  (:rq)
  '((Please.adv-s
      ((pres will.aux-s) you.pro (close.v (the.d door.n)
       (adv-e (when.ps (you.pro ((pres go.v) out.adv-a))))))))
  '((i.pro ((pres want.v)
             you.pro 
             (to (close.v (the.d door.n)
                  (adv-e (when.ps (you.pro ((pres go.v) out.adv-a))))))))
    (i.pro ((pres expect.v)
       (that (you.pro ((pres close.v) (the.d door.n)
              (adv-e (when.ps (you.pro ((pres go.v) out.adv-a)))))))))))

(define-len-pilot-infer-all-test
  test-infer-all-request4
  "Could you turn on the light please"
  (:rq)
  '((((pres could.aux-v) you.pro (turn_on.v (the.d light.n)))
     please.adv-s) ?)
  '((i.pro ((pres want.v) you.pro (to (turn_on.v (the.d light.n)))))
    (i.pro ((pres expect.v) (that (you.pro ((pres turn_on.v) (the.d light.n))))))))

(define-len-pilot-infer-all-test
  test-infer-all-wish1
  "I wish he would finish it"
  (:cf :wish)
  '(i.pro ((pres wish.v) (tht (he.pro ((cf will.aux-s) (finish.v it.pro))))))
  '((he.pro ((pres will.aux-s) not.adv-s (finish.v it.pro)))))

(define-len-pilot-infer-all-test
  test-infer-all-wish2
  "I wish he would leave"
  (:cf :wish)
  '(i.pro ((pres wish.v) (tht (he.pro ((cf will.aux-s) leave.v)))))
  '((he.pro ((pres will.aux-s) not.adv-s leave.v))))

(define-len-pilot-infer-all-test
  test-infer-all-if1
  "If I were rich I would travel to Rome"
  (:cf :if-then)
  '((if.ps (I.pro ((cf were.v) rich.a)))
         (i.pro ((cf will.aux-s) (travel.v (to.p-arg |Rome|)))))
  '((i.pro ((pres be.v) not.adv-s rich.a))
    (i.pro ((pres will.aux-s) not.adv-s (travel.v (to.p-arg |Rome|))))))

(define-len-pilot-infer-all-test
  test-infer-all-if2
  "If she had a hammer she would swing it"
  (:cf :if-then)
  '((if.ps (she.pro ((cf have.v) (a.d hammer.n))))
           (she.pro ((cf will.aux-s) (swing.v it.pro))))
  '((she.pro ((pres do.aux-s) not.adv-s (have.v (a.d hammer.n))))
    ; NB: Should we keep this inference?  It's a donkey sentence, so maybe not.
    ;     For donkey sentences, the indexical is not meaningful without the
    ;     previous clause.
    ;(she.pro ((pres will.aux-s) not.adv-s (swing.v it.pro)))))
  ))

(define-len-pilot-infer-all-test
  test-infer-all-if3
  "If she had had a hammer she would swing it"
  (:cf :if-then)
  '((if.ps (she.pro ((cf perf) (have.v (a.d hammer.n)))))
           (she.pro ((cf will.aux-s) (swing.v it.pro))))
  '((she.pro ((past do.aux-s) not.adv-s (have.v (a.d hammer.n))))
   ;(she.pro ((pres will.aux-s) not.adv-s (swing.v it.pro)))))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tatoeba Sampled Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If-then constructions.

(define-len-pilot-infer-all-test
  test-infer-all-if-17577
  "If I were you I would be able to succeed."
  (:cf :if-then)
  '((If.ps (I.pro ((cf were.v) you.pro)))
            (I.pro ((cf will.aux-s) (be.v (able.a (to succeed.v))))))
  '((i.pro ((pres be.v) not.adv-s you.pro))))

(define-len-pilot-infer-all-test
  test-infer-all-if-17955
  "If you were to fall from that bridge, it would be almost impossible to rescue you."
  (:cf :if-then)
  '((If.ps (you.pro ((cf were.v) 
                     (to (fall.v (adv-a (from.p (that.d bridge.n)))))))) 
           (it.pro ((cf will.aux-s) (be.v 
            (almost.adv-a (impossible.a (adv-a (to.p (rescue.v you.pro)))))))))
  '((you.pro
       ((pres will.aux-s) not.adv-s
        (fall.v (adv-a (from.p (that.d bridge.n))))))))

(define-len-pilot-infer-all-test
  test-infer-all-if-18636
  "If I had money, I would pay what I owe you."
  (:cf :if-then)
  '((If.ps (I.pro ((cf have.v) (k money.n))))
           (I.pro ((cf will.aux-s) 
                   (pay.v (sub what.pro (I.pro ((pres owe.v) you.pro *h)))))))
  '((i.pro ((pres do.aux-s) not.adv-s (have.v (k money.n))))
    (i.pro ((pres will.aux-s) not.adv-s 
            (pay.v (sub what.pro (I.pro ((pres owe.v) you.pro *h))))))))

(define-len-pilot-infer-all-test
  test-infer-all-if-16988
  "If only I had taken your advice."
  (:cf :if-then)
  '((If.ps (only.adv-s (I.pro ((cf perf) (take.v (your.d advice.n)))))) {ref1}.s)
  '((i.pro ((past do.aux-s) not.adv-s (take.v (your.d advice.n))))))

;;; Inverted if-then constructions.

(define-len-pilot-infer-all-test
  test-infer-all-inv-16968
  "Had I known your telephone number, I would have called you"
  (:cf :if-inv)
  '(((cf perf) I.pro (know.v (your.d (telephone.n number.n))))
     (I.pro ((cf will.aux-s) (perf (call.v you.pro))))) 
  '((i.pro ((past do.aux-s) not.adv-s (call.v you.pro)))
    (i.pro
      ((past do.aux-s) not.adv-s
                       (know.v (your.d (telephone.n number.n)))))))
  
(define-len-pilot-infer-all-test
  test-infer-all-inv-16968-v2
  "Had I known your telephone number, I would have called you."
  (:cf :if-inv)
  '(({if}.ps (I.pro ((cf perf) (know.v (your.d (telephone.n number.n))))))
            (I.pro ((cf will.aux-s) (perf (call.v you.pro)))))
  '((i.pro ((past do.aux-s) not.adv-s
     (know.v (your.d (telephone.n number.n)))))
    (i.pro ((past do.aux-s) not.adv-s (call.v you.pro)))))

(define-len-pilot-infer-all-test
  test-infer-all-inv-18529
  "Were I rich, I would help the poor."
  (:cf :if-inv)
  '((((cf Were.v) I.pro rich.a))
    (I.pro ((cf will.aux-s) (help.v (the.d (poor.a {ref1}.n))))))
  '((i.pro ((pres will.aux-s) not.adv-s (help.v (the.d (poor.a {ref1}.n)))))
    (i.pro ((pres be.v) not.adv-s rich.a))))

(define-len-pilot-infer-all-test
  test-infer-all-inv-18529-v2
  "I were rich I would help the poor."
  (:cf :if-inv)
  '(({if}.ps (I.pro ((cf Were.v) rich.a))) 
           (I.pro ((cf will.aux-s) (help.v (the.d (poor.a {ref1}.n))))))
  '((i.pro ((pres be.v) not.adv-s rich.a))
    (i.pro ((pres will.aux-s) not.adv-s (help.v (the.d (poor.a {ref1}.n)))))))

;;; Requests.

(define-len-pilot-infer-all-test
  test-infer-all-req-1661
  "Could you dial for me?"
  (:rq)
  '(((pres Could.aux-v) you.pro ((dial.v {ref1}.pro) (adv-a (for.p me.pro)))) ?)
  '((i.pro
      ((pres want.v)
       you.pro (to ((dial.v {ref1}.pro) (adv-a (for.p me.pro))))))
    (i.pro
      ((pres expect.v)
       (that (you.pro (((pres dial.v) {ref1}.pro) (adv-a (for.p me.pro)))))))))
  
(define-len-pilot-infer-all-test
  test-infer-all-req-2242
  "Could you please repeat that?"
  (:rq)
  '(((pres Could.aux-v) you.pro please.adv-s (repeat.v that.pro)) ?)
  '((i.pro ((pres want.v) you.pro (to (repeat.v that.pro))))
    (i.pro ((pres expect.v) (that (you.pro ((pres repeat.v) that.pro)))))))

;;; Wish constructions.

(define-len-pilot-infer-all-test
  test-infer-all-wish-1393
  "I wish I could go to Japan."
  (:cf :wish)
  '(I.pro ((pres wish.v)
          (tht (I.pro ((pres could.aux-v) ; NB: the incorrect annotation
                        (go.v (adv-a (to.p |Japan|))))))))
  '())

(define-len-pilot-infer-all-test
  test-infer-all-wish-1393-v2
  "I wish I could go to Japan."
  (:cf :wish)
  '(I.pro ((pres wish.v)
           (tht (I.pro ((cf can.aux-v) 
                        (go.v (adv-a (to.p |Japan|))))))))
  '((i.pro ((pres can.aux-v) not.adv-s (go.v (adv-a (to.p |Japan|)))))))

(define-len-pilot-infer-all-test
  test-infer-all-wish-2470
  "I wish she would stop playing that stupid music."
  (:cf :wish)
  '(I.pro ((pres wish.v)
          (tht (she.pro ((pres would.aux-s) ; NB: the incorrect annotation
                          (stop.v (ka (play.v (that.d (stupid.a music.n))))))))))
  '())

(define-len-pilot-infer-all-test
  test-infer-all-wish-2470-v2
  "I wish she would stop playing that stupid music."
  (:cf :wish)
  '(I.pro ((pres wish.v)
          (tht (she.pro ((cf will.aux-s)
                          (stop.v (ka (play.v (that.d (stupid.a music.n))))))))))
  '((she.pro
      ((pres will.aux-s) not.adv-s
                         (stop.v (ka (play.v (that.d (stupid.a music.n)))))))))

