
(in-package :ulf-pragmatics)

(define-test test-polarity-alignment
  "Tests the basic polarity alignment functionality."
  (:tag :dynamic-polarity :polarity-align)
;; Test with the following has worked so far.
;; NB: since we're assuming the transparent operators aren't interacting, we
;; can merge them with their children in terms of polarity (e.g. (plur (cat.n
;; +)) -> ((plur +) (cat.n +))).
  (let ((ulf1 '(|Mary| ((PRES KNOW.V) (THAT (|John| ((PAST GO.V) (K HOME.N)))))))
        (ulf2 '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N)))))
        nlog1 aligned1 nlog2 aligned2)
    (setf nlog1 (run-natlog (list (ulf2english:ulf2english ulf1 :add-punct? nil :capitalize-front? nil))))
    (setf aligned1 (align-ulf-polarity ulf1 (first nlog1)))
    (assert-equal '((|Mary| +) (((pres +) (know.v +)) 
                                ((that +) ((|John| +) (((past +) (go.v +)) 
                                                       (k (home.n +)))))))
                  aligned1)

    (setf nlog2 (run-natlog (list (ulf2english:ulf2english ulf2 :add-punct? nil :capitalize-front? nil))))
    (setf aligned2 (align-ulf-polarity ulf2 (first nlog2)))
    (assert-equal '(((all.d +) ((plur -) (cat.n -)))
                    (((pres +) (have.v +)) (k ((plur +) (tail.n +)))))
                  aligned2)))

(define-test test-get-segment-polarity
  "Tests the basic functionality for get-segment-polarity."
  (:tag :dynamic-polarity :get-segment-polarity)
  (let* ((ulfpart1 '((PRES HAVE.V) (K (PLUR TAIL.N)))) ;VP
         (ulfpart2 'CAT.N) ;N
         (ulfpart2-par (list 'plur ulfpart2)) ;plur N
         (ulfpart3 (list 'all.d ulfpart2-par)) ;det + N
         (compulf (list ulfpart3 ulfpart1)) ;Formula
         (diffulf '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N))))))
    (assert-equal '+ (get-segment-polarity ulfpart1 compulf compulf))
    (assert-equal nil (get-segment-polarity ulfpart1 compulf diffulf))
    (assert-equal nil (get-segment-polarity ulfpart1 diffulf compulf))
    (assert-equal nil (get-segment-polarity ulfpart1 diffulf diffulf))
    (assert-equal '- (get-segment-polarity ulfpart2 ulfpart2-par compulf))
    (assert-equal nil (get-segment-polarity ulfpart2 ulfpart2-par diffulf))
    (assert-equal nil (get-segment-polarity ulfpart2 nil compulf)))

  ; Variables for possible future testing.
  (let* ((ulf3part1 '(KNOW.V (THAT (| MARY| ((PAST BE.V) COLD.A)))))
         (ulf3part2 '(THE.D MAN.N))
         (ulf3part3 (list '(PAST DO.AUX-S) 'NOT ulf3part1))
         (compulf3 (list ulf3part2 ulf3part3)))
    nil))

