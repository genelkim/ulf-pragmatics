;;; Gene Kim 7-29-2018
;;; Code to hold shared parameters and functions.

(in-package :ulf-pragmatics)

;; Define applicable rule subset for request and counterfactuals that we want
;; to test.
(defparameter *rule-names* 
 '(infer-want-from-request-raw infer-expect-from-request-raw 
   infer-falsehood-from-positive-counterfactual-raw
   infer-falsehood-from-inverted-positive-counterfactual-raw
   infer-fact-from-negative-counterfactual-raw
   infer-fact-from-inverted-negative-counterfactual-raw
 ))
;; Run the selected subset of rules on the given ULF and return a list of:
;; (ulf result1 result2 ...)
(defun run-subset-rules (ulf)
  (mapcar #'flatten-adv-s
          (car (results-from-applying-rules *rule-names* (list ulf) t))))

;; Performs all possible inferences on the given ulf, but only for those
;; inferences that were supported during the pilot experiment.
(defun old-infer-all (ulf)
  (labels
    ; Define local versions of inferences that don't include post-pilot inferences.
    ((premacro-inferences (f)
       (apply #'append
              (mapcar #'(lambda (inf-fn) (funcall inf-fn f))
                      (list #'premacro-request-inferences
                            #'premacro-counterfactual-inferences))))
     (postmacro-inferences (f) nil))
    ; Basically the body of infer-all
    (let ((ulf1 (initial-ulf-normalization ulf))
          ulf1m ulf2 preinfs postinfs recableinfs recinfs rawinfs)
      ;; Perform preliminary inferences.
      (setf preinfs (premacro-inferences ulf1))
      ;; Expand macros and fully normalize.
      (setf ulf1m (expand-macros ulf1))
      (setf ulf2 (fully-normalize ulf1m))
      ;; Perform post-macro inferences.
      (setf postinfs (postmacro-inferences ulf2))
      ;; Recursively infer where applicable.
      (setf recableinfs (remove-if-not #'recursable-inference?
                                       (append preinfs postinfs)))
      (setf recinfs (apply #'append
                           (mapcar #'old-infer-all recableinfs)))
      ;; Return all inferences in a list.
      (setf rawinfs (append preinfs postinfs recinfs))
      (mapcar #'(lambda (inf-res)
                  (setf (result-formula inf-res)
                        (output-ulf-normalization (result-formula inf-res)))
                  inf-res)
              rawinfs))))

