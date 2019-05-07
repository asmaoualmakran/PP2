#lang racket
(require "../Infrabel/railwayManager.rkt")
(require "../NMBS/trainManager.rkt")
(require graph)
(require rackunit)

;---------------------------------------------------------
; Initialisation tests
; Check if the railway manager is correctly initialised.
;---------------------------------------------------------

(let ([railwayManager (new RailwayManager%)]
      [trainManager (new TrainManager%)]
      [graph (unweighted-graph/directed '((a b) (c d)))])

  (send railwayManager initialise! graph trainManager)
  
  (test-case
   "RailwayManager Initialisation tests"

   (check-true (send railwayManager initialised?))
   (check-eq? trainManager (send railwayManager getTrainManager))
   (check-eq? graph (send railwayManager getGraph))))

;------------------------------------------------------------
; Creation tests
; Check if the objects are correctly created and initialised
;------------------------------------------------------------

(let ([railwayManager (new RailwayManager%)]
      [trainManager (new TrainManager%)]
      [graph (unweighted-graph/directed '((a b) (c d)))])

  (send railwayManager initialise! graph trainManager)

  (test-case
   "Creation and initialisation tests"

   (check-true (send railwayManager initialised?))))


;------------------------------------------
; Failure tests
; Check if the code fails in these tests
;------------------------------------------

(let ([railwayManager (new RailwayManager%)]
      [trainManager (new TrainManager%)]
      [graph (unweighted-graph/directed '((a b) (c d)))])

  (test-case
   "Failure test railway manager wrong init pm"

   (check-false (send railwayManager initialised?))
   (check-exn exn:fail?
              (lambda () (send railwayManager getGraph)))
   (check-exn exn:fail?
              (lambda () (send railwayManager initialise! 5 4))))

  (send railwayManager initialise! graph trainManager)
  
  (test-case
   "Failure test railway manager wrong creation pm"
   (check-exn exn:fail?
              (lambda () (send railwayManager createDetectionblock! 5)))
   (check-exn exn:fail?
              (lambda () (send railwayManager createSwitch! 5)))
   (check-exn exn:fail?
              (lambda () (send railwayManager createTrack! 5)))))
   
   