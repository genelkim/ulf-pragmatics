;;; Gene Kim 7-23-2018
;;;
;;; Initially copied over functions written by Len for the pilot inference
;;; project.  The functions have been refactored for better overall project
;;; organization.

(in-package :ulf-pragmatics)

(defparameter *infer-want-from-request*
;``````````````````````````````````````
; Obtain a want-inference from a polite request (top-level).
; TTT operator ^* could be used, but we control search separately.
; e.g., "Can somebody help me?"
;       (((pres can.aux-v) somebody.pro (help.v me.pro)) [?])
;       => "I want that somebody helps me"
;       (I.pro ((pres want.v) (that (somebody.pro (help.v me.pro)))))
; NB: ttt symbols like !, ?, ~, + etc., appearing in the ULF must
;     first be square-bracketed so as not to "confuse" ttt.
; How to apply: use ttt:apply-rule after square-bracketing '?' or
;     applying hide-ttt-ops (if necessary recovering them via 'unhide-ttt-ops')
   '(/ ((pres (! aux-indicating-request?))
        (!1 you.pro someone.pro somebody.pro)
        (? please.adv-s) (+ ~ (^* [*h])))  ; will have to presubst for ?, !
       (I.pro ((pres want.v) !1 (to (+))))))
  ; We'll want to potentially chain from the last part, if it enables
  ; further attitudinal, state change/cessation/inception/continuation
  ; or other inferences.

(defparameter *infer-expect-from-request*
;```````````````````````````````````````
; Obtain an expect-inference from a polite request (top-level).
; TTT operator ^* could be used, but we control search separately.
; e.g., "Can somebody help me?"  => "I expect that somebody help me"
;       (Note: The rule could be refined to yield "...will help me")
   '(/ ((pres (! aux-indicating-request?))
        (!1 you.pro someone.pro somebody.pro)
        (? please.adv-s) (+ ~ (^* [*h])))
       (I.pro ((pres expect.v) (that (!1 (add-vp-tense! (+) pres)))))))
  ; We'll want to potentially chain from the last part, if it enables
  ; further attitudinal, state change/cessation/inception/continuation
  ; or other inferences.

(defparameter *infer-want-or-should-from-neg-request*
;````````````````````````````````````````````````````
; Obtain a "want or think you should"-inference from a negative request.
; e.g. "Can't you be friendly?"
;   -> I want you to be friendly or I think you should be friendly.
  '(/ ((pres (!1 aux-indicating-request?))
       (!2 not not.adv-s)
       (!3 you.pro someone.pro somebody.pro)
       (? please.adv-s) (+ ~ (^* [*h])))
      ((I.pro ((pres want.v) !3 (to +)))
       or.cc
       (I.pro ((pres think.v)
               (tht (!3 ((pres should.aux-v) (+)))))))))

(defparameter *infer-not-current-from-neg-request*
;`````````````````````````````````````````````````
; Obtain a "not or don't plan to"-inference from a negative request.
; e.g. "Can't you be friendly?"
;   -> You are not being friendly or you don't plan to be friendly
  '(/ ((pres (!1 aux-indicating-request?))
       (!2 not not.adv-s)
       (!3 you.pro someone.pro somebody.pro)
       (? please.adv-s) (+ ~ (^* [*h])))
      ((!3 ((pres prog) !2 (+)))
       or.cc
       (!3 ((pres do.aux-s) !2 (plan.v (to (+))))))))

;; Versions of the inferences above without the "or".
(defparameter *infer-want-or-should-from-neg-request-simple*
;````````````````````````````````````````````````````
; Obtain a "want or think you should"-inference from a negative request.
; e.g. "Can't you be friendly?"
;   -> I want you to be friendly.
  '(/ ((pres (!1 aux-indicating-request?))
       (!2 not not.adv-s)
       (!3 you.pro someone.pro somebody.pro)
       (? please.adv-s) (+ ~ (^* [*h])))
      (I.pro ((pres want.v) !3 (to (+))))))

(defparameter *infer-not-current-from-neg-request-simple*
;`````````````````````````````````````````````````
; Obtain a "not or don't plan to"-inference from a negative request.
; e.g. "Can't you be friendly?"
;   -> You are not being friendly
  '(/ ((pres (!1 aux-indicating-request?))
       (!2 not not.adv-s)
       (!3 you.pro someone.pro somebody.pro)
       (? please.adv-s) (+ ~ (^* [*h])))
      (!3 ((pres prog) !2 (+)))))

;; TODO: introduce tense-contexts for inference.
;;       e.g. (he said, "name ..") -> "you were obligated to.."
(defparameter *infer-obligation-from-imperative*
  '(/ (((!1 term?)
        ((pres (!2 lex-verbaux?))
         _+))
       [!])
      ((make-explicit! !1) ((pres (pasv obligate.v)) (to (!2 _+))))))

; Now the functionalized rules themselves (applying the TTT rules at
; multiple depths):

(defun infer-want-from-request (ulf)
;``````````````````````````````````
 (all-ttt-rule-inf-result *infer-want-from-request* ulf))

(defun infer-expect-from-request (ulf)
;`````````````````````````````````````
 (all-ttt-rule-inf-result *infer-expect-from-request* ulf))

(defun infer-want-or-should-from-neg-request (ulf)
;`````````````````````````````````````
  (all-ttt-rule-inf-result *infer-want-or-should-from-neg-request* ulf))

(defun infer-not-current-from-neg-request (ulf)
;`````````````````````````````````````
  (all-ttt-rule-inf-result *infer-not-current-from-neg-request* ulf))

(defun infer-want-or-should-from-neg-request-simple (ulf)
;`````````````````````````````````````
  (all-ttt-rule-inf-result *infer-want-or-should-from-neg-request-simple* ulf))

(defun infer-not-current-from-neg-request-simple (ulf)
;`````````````````````````````````````
  (all-ttt-rule-inf-result *infer-not-current-from-neg-request-simple* ulf))

(defun infer-obligation-from-imperative (ulf)
;`````````````````````````````````````
  (all-ttt-rule-inf-result *infer-obligation-from-imperative* ulf))
; [raw variants]
; The *-raw variants of the rules return lists of formulas instead of
; 'inf-result instances.

(defun infer-want-from-request-raw (ulf)
 (mapcar #'result-formula (infer-want-from-request ulf)))
(defun infer-expect-from-request-raw (ulf)
 (mapcar #'result-formula (infer-expect-from-request ulf)))

;; Define functions for full pipeline.
(defun premacro-request-inferences (ulf)
  (cdar (results-from-applying-rules
          (list #'infer-want-from-request
                #'infer-expect-from-request
                #'infer-want-or-should-from-neg-request
                #'infer-not-current-from-neg-request
                #'infer-want-or-should-from-neg-request-simple
                #'infer-not-current-from-neg-request-simple
                #'infer-obligation-from-imperative)
          (list ulf) t)))

