#lang racket

(require graph)

(require "railwayManager.rkt")
(require "railwayGraph.rkt")
(require "securityProtocol.rkt")
(require "fileReader.rkt")

(provide railwayManager)
(provide railwayGraph)
(provide startInfrabel)
(provide fileReader)

(define railwayManager (new RailwayManager%))
(define railwayGraph (new RailwayGraph%))
(define fileReader (new FileReader%))
;;(define securityProtocol(new SecurityProtocol%))

(define (startInfrabel)
(send railwayGraph initialise! railwayManager)
;(define graph (send railwayGraph getGraph))
(send fileReader initialise! railwayManager))
;(send securityProtocol initialise! railwayManager))
