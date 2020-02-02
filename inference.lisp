;;; Gene Kim 7-23-2018
;;;
;;; Code for high-level inference management.
;;; This is loaded last because it refers to functions defined in all the other
;;; components.

(in-package :ulf-pragmatics)

;; Returns t if inf-result allows recursing inferences from this result.
;; Raises an error if the input is not an inf-result class.
(defun recursable-inference? (inf-result)
  (if (typep inf-result 'inf-result)
    (slot-value inf-result 'is-entailment)
    (error "ERROR: The argument to recursable-inference? must be an instance of class inf-result.")))


;; Functions for initial ULF normalization.
(defparameter *initial-ulf-normalization-fns*
  (list #'normalize-aliases
        #'flatten-singly-bracketed-constituents))
;; Performs initial ULF normalization, NOT INCLUDING
;;  1. macro expansions or
;;  2. word reordering.
(defun initial-ulf-normalization (ulf)
  (reduce #'(lambda (acc new) (funcall new acc)) 
          *initial-ulf-normalization-fns* 
          :initial-value ulf))

;; Functions for normalizing output.
(defparameter *output-ulf-normalization-fns*
  (list #'past-will-to-past-do
        #'remove-redundant-do
        #'clean-when-infs!
        #'poss-postmod-to-have!
        #'filter-for-some-reason
        #'fix-indefinite-articles
        #'flatten-singly-bracketed-constituents
        #'flatten-verb-args
        ))
(defun output-ulf-normalization (ulf)
  (reduce #'(lambda (acc new) (funcall new acc))
          *output-ulf-normalization-fns*
          :initial-value ulf))

;; Functions that perform inferences from premacro ULFs.
(defparameter *premacro-inference-fns*
  (list #'premacro-request-inferences
        #'premacro-counterfactual-inferences
        #'premacro-q-inferences
        #'premacro-implicative-inferences
        #'premacro-sit-inferences
        #'it-cleft-inferences))
;; Obtains inferences from ULFs before macros are applied.
;; These include inferences that rely on the question inversions or cleft
;; constructions.
(defun premacro-inferences (ulf)
  (apply #'append
         (mapcar #'(lambda (inf-fn) (funcall inf-fn ulf))
                 *premacro-inference-fns*)))

;; Functions that perform inferences on post-macro expanded ULFs.
(defparameter *postmacro-inference-fns*
  (list #'postmacro-q-inferences
        #'natural-logic-entailments
        #'implicative-entailments))
;; Obtains inferences from ULFs after macros are applied.
(defun postmacro-inferences (ulf)
  (apply #'append
         (mapcar #'(lambda (inf-fn) (funcall inf-fn ulf))
                 *postmacro-inference-fns*)))


;; Performs all possible inferences on the given ulf.
;; Returns a list of inferences.
(defun infer-all (ulf)
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
                         (mapcar #'infer-all recableinfs)))
    ;; Return all inferences in a list.
    (setf rawinfs (append preinfs postinfs recinfs))
    (mapcar #'(lambda (inf-res)
                (setf (result-formula inf-res)
                      (output-ulf-normalization (result-formula inf-res)))
                inf-res)
            rawinfs)))
(util:memoize 'infer-all)

