
(in-package :ulf-pragmatics)

;; Strips aux verbs and negation from ulf once polarity is extracted
;; (remove-aux-not '((the.d man.n) ((past do.aux-s) not (know.v (that (| Mary| ((past be.v) cold.a)))))))
;; =>
;; ((the.d man.n) (know.v (that (| Mary| ((past be.v) cold.a)))))

;; NOTE: prevent from going into (that ...) and (ka ...)
(defun remove-aux-not (ulf)
  (remove-nil (remove-double-list (cond
    ((null ulf) nil)
    ((or (is-aux ulf) (equalp 'NOT ulf)) nil)
    ((atom ulf) ulf)
    ((or (equalp (first ulf) 'KA) (equalp (first ulf) 'THAT)) ulf)
    (t (mapcar (lambda (x) (remove-aux-not x)) ulf))))))

;; Function to recursively remove doubled-up lists (e.g. (remove-double-list '(A ((B C)))) => (A (B C)))
(defun remove-double-list (tr)
  (cond
    ((null tr) nil)
    ((atom tr) tr)
    ((and (listp tr) (= 1 (length tr)) (listp (first tr))) (first tr))
    (t (mapcar (lambda (x) (remove-double-list x)) tr))))

;; Function to recursively remove nil from list
(defun remove-nil (x)
  (if (listp x)
    (mapcar #'remove-nil (remove nil x))
    x))

;; Given ulf, returns segment beginning with verb, its parent, and full ulf
;; (i.e. the format required for get-segment-polarity in dynamic-polarity/dynamic-polarity.lisp)
;; Example useage:
;; (get-ulf-segments-vp '((THE.D MAN.N) ((PAST DO.AUX-S) NOT (KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A)))))))
;; =>
;; ((KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A))))
;;  ((PAST DO.AUX-S) NOT (KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A)))))
;;  ((THE.D MAN.N) ((PAST DO.AUX-S) NOT (KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A)))))))
(defun get-ulf-segments-vp (ulf)
  (let ((res (call-tfdfs-par ulf (until-vp))))
    (if res (list (first res) (third res) ulf) nil)))

;; Return true if given item is a verb (i.e. ends in .V) or tense+verb (i.e. (PAST *.V))
(defun is-verb (x)
  (cond ((and (symbolp x) (search ".V" (string x))) t)
  ((and (listp x) (= 2 (length x))
    (symbolp (first x)) (symbolp (second x)) (search ".V" (string (second x)))) t)
  (t nil)))

;; Return true if given item is a aux-s (i.e. ends in .AUX-S) or tense+aux-s (i.e. (PAST *.AUX-S))
(defun is-aux (x)
  (cond ((and (symbolp x) (search ".AUX-S" (string x))) t)
  ((and (listp x) (= 2 (length x))
    (symbolp (first x)) (symbolp (second x)) (search ".AUX-S" (string (second x)))) t)
  (t nil)))

;; Renaming for ttt:apply-rule for the sole purpose of focused memoizing.
(defun apply-ttt-rule (ttt-rule x)
  (ttt:apply-rule ttt-rule x))
(util:memoize 'apply-ttt-rule)

(defun get-ttt-rule-ulf-segments (ulf ttt-rule)
  (let* ((search-result
          (call-tfdfs-par 
            ulf
            (until-fn
              #'(lambda (x)
                  (let ((rule-result (apply-ttt-rule ttt-rule x)))
                      (and rule-result (not (equal rule-result x)))))))))
    (if search-result
      (list (first search-result) (third search-result) ulf)
      nil)))


(defun results-from-applying-rules (rule-names ulfs filter-out-failures)
;``````````````````````````````````````````````````````````````````````
; To apply a list of rules to a list of ulfs, to obtain a list of all possible 
; results (possibly nil) for each fact, in the format (fact result1 ... resultk)
; If 'filter-out-failures' is non-nil, facts that didn't produce inferences
; are omitted from the results.
;
; Each rule must take a single ULF argument and return a list of unique results
; from applying the rule.
;
; We first hide (square-bracket) any ttt-op symbols in the ulfs. Note that
; rules that look for '?', '!', '+', '*', '{}', etc. (which they don't at 
; the time of writing (Nov 7/17)) should be formulated using [?], [!], [+], 
; [*], [{}] etc. At the end we "unhide" the bracketed punctuation.
;
 (cond ((or (null rule-names) (null ulfs)) 
        (return-from results-from-applying-rules nil))
       ((or (atom rule-names) (atom ulfs)) 
        (return-from results-from-applying-rules
          (format nil "** Improper use of 'results-from-applying-rules': ~s, ~s" 
                      rule-names ulfs))))
 (let (ulfs[] result results)
      (setq ulfs[] (mapcar #'hide-ttt-ops ulfs))
      (dolist (ulf ulfs[])
          (setq result 
             (apply #'append 
                    (mapcar #'(lambda (f) (funcall f ulf)) rule-names)))
          (if result 
            (push (cons ulf (remove nil result)) results)
            (if (not filter-out-failures) (push (list ulf) results))))
      (mapcar #'unhide-ttt-ops (reverse results)))
 ); end of results-from-applying-rules


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following functions define a number of recursive
;;; "thunk"-type functions for tree traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrapper function to call tfdfs given evaluation function
;; Example useage:
;; (call-tfdfs '((A (B C)) D) (until-m 5)) => (C NIL)
;; (call-tfdfs '((A (B C)) D) (until-sym 'D)) => (D NIL)
(defun call-tfdfs (tr fn)
  (tfdfs tr (funcall fn 0)))

;; Wrapper function to call tfdfs-par given evaluation function
;; Example useage:
;; (call-tfdfs-par '((A (B C)) D) (until-m 5)) => (C NIL (B C))
;; (call-tfdfs-par '((A (B C)) D) (until-sym 'D)) => (D NIL ((A (B C)) D))
(defun call-tfdfs-par (tr fn)
  (tfdfs-par tr (funcall fn 0) nil))

;; TODO: add documentation on how to use this thunk-based search.
;; Depth-first traversal of tree, returns element on which evaluation function 'fn' returns true
(defun tfdfs (tr fn)
    (if (null tr) (list nil fn)
      (let*
          ((res (funcall fn tr))
           (evl (first res))
           (newfn (second res))
           testnewfn)
        (cond
          (evl (list tr nil))
          ((atom tr) (list nil newfn))
          (t
            (dolist (st tr)
              (let ((tmpres (tfdfs st newfn)))
                (if (equalp st '(A B C)) (setf testnewfn newfn))
                (if (first tmpres)
                  (return-from tfdfs tmpres))
                (setf newfn (second tmpres))))
            (list nil newfn))))))

;; Depth-first traversal of tree, returns element on which evaluation function 'fn' returns true, as
;; well as that element's parent
(defun tfdfs-par (tr fn tr-par)
    (if (null tr) (list nil fn nil)
      (let*
          ((res (funcall fn tr))
           (evl (first res))
           (newfn (second res))
           testnewfn)
        (cond
          (evl (list tr nil tr-par))
          ((atom tr) (list nil newfn tr))
          (t
            (dolist (st tr)
              (let ((tmpres (tfdfs-par st newfn tr)))
                (if (equalp st '(A B C)) (setf testnewfn newfn))
                (if (first tmpres)
                  (return-from tfdfs-par tmpres))
                (setf newfn (second tmpres))))
            (list nil newfn tr))))))

;; Depth-first traversal of tree as in tfdfs, but uses car/cdr rather than dolist in traversal
;; (unused currently)
(defun tfdfs-car (tr fn)
  (if (null tr) (list nil fn)
    (let*
        ((res (funcall fn tr))
        (evl (first res))
        (newfn (second res)) carres)
      (cond
        (evl (list tr nil))
        ((atom tr) (list nil newfn))
        (t
          (setq carres (tfdfs-car (car tr) newfn))
          (if (first (car res)) carres
            (tfdfs-car (cdr tr) (second carres))))))))

;; Sample tfdfs evaluation function : runs until 'm' increments are reached
(defun until-m (m)
  (labels 
    ((out (n)
      (lambda (tree)
        (labels
          ((next (_)
            (declare (ignore _))
            (list (if (= n m) t nil)
            (out (+ n 1))))
          ) ; end labels def
        (next tree))))) #'out))

;; Sample tfdfs evaluation function : runs until symbol 'sym' is reached
(defun until-sym (sym)
  (labels
    ((out (s)
      (lambda (tree)
        (labels
          ((next (x)
            (list (if (equalp x sym) t nil)
            (out s)))
          ) ; end labels def
        (next tree))))) #'out))

;; Runs until a tree having a verb as the first element is found
(defun until-vp ()
  (labels
    ((out (s)
      (lambda (tree)
        (labels
          ((next (x)
            (list (if (and (listp x) (is-verb (first x))) t nil)
            (out s)))
          ) ; end labels def
        (next tree))))) #'out))

;; Runs until a tree satisfying the given function is found.
(defun until-fn (fn)
  (labels
    ((out (s) ; s doesn't do anything here other than keep the type signature the same as the counting thunks.
      (lambda (tree)
        (labels
          ((next (x)
            (list (if (funcall fn x) t nil)
            (out s)))
          ) ; end labels def
        (next tree))))) #'out))

