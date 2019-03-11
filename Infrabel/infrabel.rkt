#lang racket

(require "railwayManager.rkt")
(require "railwayGraph.rkt")
(require "securityProtocol.rkt")

(provide railwayManager)
(provide railwayGraph)
(provide startInfrabel)

(define railwayManager (new RailwayManager%))
(define railwayGraph (new RailwayGraph%))
(define securityProtocol(new SecurityProtocol%))

(define (startInfrabel)
(send railwayGraph initialise! railwayManager)
;(define graph (send railwayGraph getGraph))
(send railwayManager setGraph! (send railwayGraph getGraph))
(send securityProtocol initialise! railwayManager))
