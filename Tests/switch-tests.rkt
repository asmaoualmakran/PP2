#lang racket
(require "../Infrabel/switch.rkt")
(require rackunit)

;----------------------------------------------------------
; Initialisation tests
; Check if the switch's fields are correctly initialised.
;----------------------------------------------------------

(let ([switch (new Switch%)])

 (send switch setID! 's1)
 (send switch initialise!)
  
  (let ([ID (send switch getID)]
        [conns (send switch getConnections)]
        [firstY (send switch getFirstYConnection)]
        [secondY (send switch getSecondYConnection)]
        [init (send switch initialised?)]
        [available (send switch getAvailable)])

(test-case
 "Initialisation test"
 (check-true init)
 (check-eq? ID 's1)
 (check-match (list 'none 'none 'none) conns)
 (check-eq? firstY 'none)
 (check-eq? secondY 'none)
 (check-true available))))

;------------------------------------------------------------
; Connection tests
; Check if ID's can be correctly connected to the switch
;------------------------------------------------------------

(let ([switch (new Switch%)]
      [t1 't1]
      [t2 't2]
      [t3 't3])
  (send switch setID! 's1)
  (send switch initialise!)

(test-case
 "Initialisation test"
 (check-true (send switch initialised?)))
  
  (send switch setConnections! t1 t2 t3)
  
  (let ([yConnections (send switch getYConnection)])
    
  (test-case
   "Connection test switch"
   (check-eq? t1 (send switch getConnection))
   (check-eq? t2 (send switch getFirstYConnection))
   (check-eq? t3 (send switch getSecondYConnection))
   (check-match (list t2 t3) yConnections))))

;-------------------------------------------
; Failure Tests
; Check if the code fails in these tests.
;-------------------------------------------

(let ([switch (new Switch%)])
  (test-case
   "Failure test switch"
   (check-false (send switch initialised?))
   (check-exn exn:fail?
              (lambda () (send switch getID)))))
   
 
 