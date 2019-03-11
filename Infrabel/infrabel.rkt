#lang racket

(require "railwayManager.rkt")
(require "railwayGraph.rkt")
(require "securityProtocol.rkt")

(provide railwayManager)

(define railwayManager (new RailwayManager%))
(define railwayGraph (new RailwayGraph%))
(define securityProtocol(new SecurityProtocol%))

(send railwayGraph initialise! railwayManager)
(send securityProtocol initialise! railwayManager)
