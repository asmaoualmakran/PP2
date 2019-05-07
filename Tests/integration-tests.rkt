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

  (let ([switch (send railwayManager createSwitch! 's)]
        [track (send railwayManager createTrack! 't)]
        [detectionblock (send railwayManager createDetectionblock! 'd)])
    (let ([getSwitch (send railwayManager getSwitch 's)]
          [getTrack (send railwayManager getTrack 't)]
          [getBlock (send railwayManager getDetectionblock 'd)])

      (test-case
       "Creation and initialisation tests"

       (check-true (send railwayManager initialised?))
       (check-eq? (send railwayManager getObject 's) getSwitch)
       (check-eq? (send railwayManager getObject 't) getTrack)
       (check-eq? (send railwayManager getObject 'd) getBlock)
       (check-true (send getSwitch initialised?))
       (check-true (send getTrack  initialised?))
       (check-true (send getBlock  initialised?))
       (check-true (send railwayManager isSwitch? 's))
       (check-true (send railwayManager isTrack? 't))
       (check-true (send railwayManager isDetectionblock? 'd))
       )))

  (send railwayManager deleteSwitch! 's)
  (send railwayManager deleteTrack! 't)
  (send railwayManager deleteDetectionblock! 'd)

  (test-case
   "Deletion tests"

  (check-false (send railwayManager isSwitch? 's))
  (check-false (send railwayManager isTrack? 't))
  (check-false (send railwayManager isDetectionblock? 'd)))

  (test-case
   "Dubble deletion tests"
   
  (check-exn exn:fail?
             (lambda () (send railwayManager deleteSwitch! 's)))
  (check-exn exn:fail?
             (lambda () (send railwayManager deleteTrack! 't)))
  (check-exn exn:fail?
             (lambda () (send railwayManager deleteDetectionblock! 'd)))          
   ))

;------------------------------------------
; Failure tests
; Check if the code fails in these tests
;------------------------------------------

(let ([railwayManager (new RailwayManager%)]
      [trainManager (new TrainManager%)]
      [graph (unweighted-graph/directed '((a b) (c d)))])
  
  (let([switch (send railwayManager createSwitch! 's)]
       [track (send railwayManager createTrack! 't)]
       [detectionblock (send railwayManager createDetectionblock! 'd)])

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
                (lambda () (send railwayManager createTrack! 5))))

    (test-case
     "Failure test wrong deletion pm"

     (check-exn exn:fail?
                (lambda () (send railwayManager deleteDetecionblock! 4)))
     (check-exn exn:fail?
                (lambda () (send railwayManager deleteDetectionblock! 't)))

     (check-exn exn:fail?
                (lambda () (send railwayManager deleteTrack! 5)))
     (check-exn exn:fail?
                (lambda () (send railwayManager deleteTrack! 'd)))

     (check-exn exn:fail?
                (lambda () (send railwayManager deleteSwitch! 5)))
     (check-exn exn:fail?
                (lambda () (send railwayManager deleteSwitch! 'd)))
     )))
   
