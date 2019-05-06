#lang racket
(require rackunit)
(require "../Infrabel/track.rkt")

;--------------------------------------------------------
; Initialisation tests
; Check if the track's fields are correctly initialised
;--------------------------------------------------------

(let ([track (new Track%)])

  (send track setID! 't1)
  (send track initialise!)

  (let ([ID          (send track getID)]
        [init        (send track initialised?)]
        [connections (send track getConnections)]
        [firstConn   (send track getFirstConnection)]
        [secondConn  (send track getSecondConnection)])

  (test-case
   "Initialisation tests"

   (check-true init)
   (check-eq? 't1 ID)
   (check-match (list 'none 'none) connections)
   (check-eq? firstConn 'none)
   (check-eq? secondConn 'none))))

;---------------------------------------------------------
; Connection tests
; Check if ID's can be correctly connected to the track
;---------------------------------------------------------

(let ([track (new Track%)])

  (send track setID! 't1)
  (send track initialise!)

  (let ([ID (send track getID)]
        {init (send track initialised?)}
        [t2 't2]
        [t3 't3]
        [connections (send track getConnections)]
        [firstConn (send track getFirstConnection)]
        [secondConn (send track getSecondConnection)])

    (test-case
     "Initialiseation tests"

     (check-true init)
     (check-eq? ID 't1)
     (check-match (list 'none 'none) connections)
     (check-eq? 'none firstConn)
     (check-eq? 'none secondConn))

    (send track setConnections! 't2 't3)
    (set! firstConn (send track getFirstConnection))
    (set! secondConn (send track getSecondConnection))

    (test-case
     "Connection tests"

     (check-match (list 't2 't3) connections)
     (check-eq? 't2 firstConn)
     (check-eq? 't3 secondConn))
    ))