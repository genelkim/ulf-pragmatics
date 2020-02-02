; Gene Kim 7-17-2018
; Functions for get polarity annotations from a ULF formula dynamically. 
;
; These functions currently rely on using the Stanford CoreNLP polarity
; annotations from the NatLog project. This is retrieved by running the java
; program. Please make sure you have java appropriately configured and 
; have the necessary corenlp jars. run_natlog.sh is the simple script that
; we use to call the java program and shows what dependencies are needed.

(in-package :ulf-pragmatics)

;; Memoization parameters.
(defparameter *run-natlog-memo* (make-hash-table :test #'equal))

;; Polarity markings we will use.
(defparameter *polarities*
  '(+ - o))

;; run-natlog takes a list of sentence strings and obtains polarity annotations
;; at the token level via the Stanford CoreNLP NatLog system. The returned
;; value is a list of pairs, where each pair is the token string followed by
;; the polarity, e.g. 
;;  Input: '("all cats have tails" 
;;           ...)
;;  Output: '((("all" +) ("cats" -) ("have" +) ("tails" +)) 
;;            ...)
;; NB: The input strings MUST NOT contain any newlines. Newlines will cause
;;     the sentence to be treated as separate sentences.
(defun run-natlog (strlst &key (memoize nil))
  ;; 1. Write strings for processing to temporary file.
  ;; 2. Run shell script.
  ;; 3. Read in tokenized string and annotations.
  ;; 4. Reformat into desired format.
  (let* ((tempfile-raw ".natlog_raw2.temp")
         (tempfile-ann ".natlog_ann2.temp")
         (full-tempfile-raw (concatenate 'string *dynamic-polarity-dir*
                                         "/"
                                         tempfile-raw))
         (full-tempfile-ann (concatenate 'string *dynamic-polarity-dir*
                                         "/"
                                         tempfile-ann))
         raw-fh ann-fh natlog-stranns newstrs)
    
    ;; 1. Write string to temporary file.
    (setq newstrs (if memoize 
                    (remove-if #'(lambda (s) (gethash s *run-natlog-memo*)) 
                               strlst)
                    strlst))
    (when newstrs
      (progn
        (setq raw-fh (open full-tempfile-raw :direction :output :if-exists :supersede))
        (dolist (str newstrs)
          (write-line str raw-fh))
        (close raw-fh)
        ;; 2. Run shell script
        ;; TODO: suppress shell output (or make a flag for it).
        ;; TODO: change to run-program or inferior-shell
        ;(asdf:run-shell-command 
        (uiop:run-program
          (format nil 
                  (concatenate 'string 
                               *dynamic-polarity-dir* 
                               "/run_natlog.sh ~a ~a ~a") 
                  *dynamic-polarity-dir* tempfile-raw tempfile-ann))
        ;; 3. Read in tokenized string and annotations
        (setq ann-fh (open full-tempfile-ann))
        (setq natlog-stranns 
              (mapcar #'(lambda (x) 
                          (declare (ignore x))
                          (list (read-line ann-fh)
                                (read-line ann-fh)))
                      strlst))
        (close ann-fh)
        ;; Delete files.
        (delete-file full-tempfile-raw)
        (delete-file full-tempfile-ann)))
    
    ;; 3b. Save new memos and lookup previous memos to combine with new output.
    (if memoize
      (dolist (raw-nl-strann (mapcar #'list strlst natlog-stranns))
        (let ((rawstr (first raw-nl-strann))
              ;(nl-str (first (second raw-nl-strann)))
              (nl-ann (second (second raw-nl-strann))))
          (setf (gethash rawstr *run-natlog-memo*) nl-ann))))
    (if memoize
      (setq natlog-stranns
            (mapcar #'(lambda (s) (list s (gethash s *run-natlog-memo*)))
                    strlst)))

    ;; 4. Reformat into desired format.
    ;; Words (as tokenized by NatLog) are paired with the corresponding
    ;; polarity, where the polarity is represented as one of [+,-,o].
    (mapcar 
      #'(lambda (strann)
          (let ((natlog-str (first strann))
                (natlog-ann (second strann)))
            (mapcar #'list 
                    (cl-strings:split (cl-strings:clean natlog-str))
                    (mapcar #'(lambda (natlog-pol)
                                (cond ((equal natlog-pol "down") '-)
                                      ((equal natlog-pol "up") '+)
                                      ((equal natlog-pol "flat") 'o)))
                            (cl-strings:split 
                              (cl-strings:clean natlog-ann))))))
      natlog-stranns)))
(util:memoize 'run-natlog)

;; Run NatLog on a list of ULFs.
(defun run-natlog-ulfs (ulfs)
  (run-natlog (mapcar #'(lambda (ulf)
                          (ulf2english:ulf2english
                            ulf :add-punct? nil :capitalize-front? nil))
                      ulfs)))

;; Aligns the atomic elements in 'ulf' with the polarity annotations from 'pol'.
;; The alignment produces a polarity annotated ULF formula with full semantic,
;; but partial symbolic coverage. e.g.,
;;   ulf: '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N)))))
;;   pol: '(("all" +) ("cat" -) ("have" +) ("tail" +))
;;   return: (((ALL.D +) (PLUR (CAT.N -))) ((PRES (HAVE.V +)) (K (PLUR (TAIL.N +)))))
;;
;; Notice that, only the polarities of tokens corresponding to annotations from
;; 'pol' are annotated, but polarity for the remaining atomic elements and all
;; the compositional elements can be inferred from the existing polarities.
(defparameter *polarity-alignment-window-size* 5)
(defun align-ulf-polarity (ulf pol)
  ;; For now we just to greedy search.
  ;; Depth-first search along ULF. For each token in the ULF, check if it's a
  ;; surface token. If so pair with next polarity if the next natlog token
  ;; is a subseq of the ulf token. Since NatLog doesn't merge tokens, they
  ;; will always have the shortened version of the token.
  ;; TODO: need to handle underscored tokens... and strings?
  (labels
    ((clean-str (str)
       (util:remove-punctuation
         (string-downcase str)))

     (rec-helper (u p) 
       (cond 
         ;; If nil, return (nil pol)
         ((null u)
          (list nil p))
         ;; Plural nouns.
         ((and (ttt:match-expr '(plur _!) u)
               (atom (second u)))
          (let ((plur (ulf:split-by-suffix (ulf2english::pluralize! (second u))))
                (ptok (caar p))
                (pp (cadar p)))
            (if (search (clean-str ptok) (clean-str (string plur)))
              (list (list (list (first u) pp)
                          (list (second u) pp)) (cdr p))
              (list u p))))

         ;; Tensed tokens.
         ((and (ttt:match-expr '(ulf:lex-tense? _!) u)
               (atom (second u)))
          (let ((tensed (ulf:split-by-suffix (ulf2english::add-tense! u)))
                (ptok (caar p))
                (pp (cadar p)))
            (if (search (clean-str ptok) (clean-str (string tensed)))
              (list (list (list (first u) pp)
                          (list (second u) pp))(cdr p))
              (list u p))))
            
         ;; If token, do token comparison.
         ((and (atom u) (ulf2english::is-surface-token? u))
          (let ((ptoks (mapcar #'car (subseq p 0 (min *polarity-alignment-window-size* 
                                                      (length p)))))
                (pps (mapcar #'cadr (subseq p 0 (min *polarity-alignment-window-size*
                                                     (length p)))))
                (procu (funcall (util:compose
                                  #'ulf2english::post-format-ulf-string 
                                  #'ulf:strip-suffix 
                                  #'util:atom2str) 
                                u)))
            ;; If any of the NatLog tokens in the given window is a
            ;; subsequence, then mark the current ULF with the polarity.
            ;; Otherwise, just return the ulf.
            (loop for i to (1- (length ptoks)) do
                  (let ((ptok (nth i ptoks))
                        (pp (nth i pps)))
                    (if (search (clean-str ptok) (clean-str procu))
                      ;; TODO: Factorize this so it doesn't sue return-from
                      (return-from rec-helper (list (list u pp) 
                                                    (nthcdr (1+ i) p))))))
            (list u p)))

         ;; If token, but not surface, just return.
         ((atom u) (list u p))
         ;; Otherwise, recursion!
         (t
           (let* ((lrec (rec-helper (car u) p))
                  (lu (first lrec))
                  (lp (second lrec))
                  (rrec (rec-helper (cdr u) lp))
                  (ru (first rrec))
                  (rp (second rrec)))
             (list (cons lu ru) rp)))))) ; end of labels

    ;; Strip off the returned remaining polarity.
    (first (rec-helper ulf pol)))) ; end of align-ulf-polarity
 
;; Infers polarities for a complete ULF given polarity annotations of tokens
;; corresponding to surface strings.
(defun infer-polarities (ulf)
  (if *debug-ulf-pragmatics*
    (format t "In infer-polarities~%ulf: ~s~%" ulf))
  (labels
    (;; Returns t if x is a polarized item.
     (polar? (x)
       (and (listp x) (= (length x) 2)
            (member (second x) *polarities*)))
     ;; Returns t if x is a polarized atomic element.
     (polar-atom? (x)
       (and (polar? x) (atom (first x))))
     ;; Polarities are propagated up the tree from the right, and down to the
     ;; left. Assumes all right-most left-nodes are polarity marked.
     ;;   1. Recurse right-most and get polarity.
     ;;   2. Recurse to other children with polarity.
     (updown-helper (f parentpol)
       (cond 
         ;; Error condition.
         ((and (atom f) (null parentpol))
          (error "updown-helper precondition starting polarity annotations has failed"))
         ;; Polarized atom, return as is.
         ((polar-atom? f) f)
         ;; Atom otherize, take parent polarity.
         ((atom f) (list f parentpol))
         ;; Recursive case. Take right-most child's polarity if without one,
         ;; then propagate to other children.
         (t (let* (;; Get current polarity -- nil if None.
                   (curpol (if (polar? f)
                             (car (last f)) 
                             nil))
                   ;; Remove polarity if one exists.
                   (npf (if curpol (second f) f))
                   ;; Recurse to right.
                   (rrec (updown-helper (car (reverse npf)) parentpol))
                   (rpol (second rrec))
                   ;; Update polarity.
                   (newpol (if curpol curpol rpol))) 
              ;; Recurse to rest and return.
              (list
                (reverse (cons rrec      
                               (mapcar #'(lambda (x) 
                                           (updown-helper x newpol)) 
                                       (cdr (reverse npf)))))
                newpol)))))
     ;; Rightmost polarity for default rightmost polarity.
     (rightmost-polarity (f)
       (cond
         ((polar? f) (second f))
         ((atom f) nil)
         (t
           (loop for e in (reverse f) do
                 (let ((res (rightmost-polarity e)))
                   (when res 
                     (return-from rightmost-polarity res))))
           nil)))
     ) ; end of labels defs

    ;; TODO: write function that preprocesses 'ulf' to ensure that if follows
    ;; the precondition for updown-helper
    ;; Give the polarity of the last polarized item as the initial polarity.
    ;; This will get propagated down and used as a default if the right-most leaf in the
    ;; tree is not polarized.
    (let ((def-pol '+)
          (right-pol (rightmost-polarity ulf)))
      (updown-helper ulf (if right-pol right-pol def-pol)))))


;; Annotates the ULF with polarity. The polarity annotations are incomplete so
;; some segments 
(defun annotate-polarity (ulf)
  (infer-polarities         ; complete polarity coverage
    (align-ulf-polarity     ; align text polarity to ulf
      ulf
      (first (run-natlog           ; get text token polarity
        (list (ulf2english:ulf2english
                ulf :add-punct? nil :capitalize-front? nil)))))))

;; Returns the polarity for the given segment of the ulf and its parent.
;; NB: 'segment' and 'parent' must be referentially equivalent to the
;;      corresponding segments within the 'fullulf'. This is necessary to 
;;      ensure correctness when there are equivalent subsegments in different 
;;      contexts, e.g.
;;       I am a person, but Spot is not a person.
;;     The first occurrence is in a positive context (we can substitute in
;;     'living thing' for it), while the second occurrence is in a negative
;;     context (same substitution is not possible).
;; e.g.
;;  segment:  'cat.n
;;  parent:   '(all.d cat.n)
;;  fullulf:  '((all.d cat.n) ((pres have.v) (k (plur tail.n))))
;;  return:   '-
;;
;; If 'segment' is the root, please supply nil as 'parent'.
(defun get-segment-polarity (segment parent fullulf)
  (labels
    (
     ;; Searches rawulf and polulf for segment that matches in rawulf and 
     ;; returns the corresponding polarity in polulf. If no match is found,
     ;; returns nil.
     (find-segment (rawulf parulf polulf)
       (let* ((curpol (second polulf))
              (nopolulf (first polulf)))
         ;; Check that the parallel versions are coherent.
         (assert (or
                   (and (atom rawulf) (atom nopolulf) (equal rawulf nopolulf))
                   (equal (length rawulf) (length nopolulf))))
         (cond
           ;; Found a match!
           ((and (eq segment rawulf)
                 (eq parent parulf)) curpol)
           ;; Base case.
           ((atom rawulf) nil)
           ;; Recursive case.
           (t (reduce #'(lambda (acc new) 
                          (if acc acc
                            (apply #'find-segment 
                                   (list (first new) rawulf (second new)))))
                      (mapcar #'list rawulf nopolulf) ; zip raw and polar
                      :initial-value nil)))))
     ) ; end of labels defs

  ;; 1. Get polarity alignment of the ULF, 'pulf'
  ;; 2. Parallel search of 'fullulf' and 'pulf' for 'segment' and get polarity
  (find-segment fullulf
                nil
                (annotate-polarity fullulf))))

