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