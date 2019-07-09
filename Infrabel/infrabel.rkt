#lang racket

;(require graph)

;(require "railwayManager.rkt")
;(require "railwayGraph.rkt")
;(require "securityProtocol.rkt")
;(require "fileReader.rkt")

;(provide railwayManager)
;(provide railwayGraph)
;(provide startInfrabel)
;(provide fileReader)

;(define railwayManager (new RailwayManager%))
;(define railwayGraph (new RailwayGraph%))
;(define fileReader (new FileReader%))
;;(define securityProtocol(new SecurityProtocol%))

;(define (startInfrabel)
;(send railwayGraph initialise! railwayManager)
;(define graph (send railwayGraph getGraph))
;(send fileReader initialise! railwayManager))
;(send securityProtocol initialise! railwayManager))

(require "railwayManager.rkt")
(require "fileReader.rkt")
(require "railwayGraph.rkt")
(require "securityProtocol.rkt")
(require "../TCP/server.rkt")
(require "interface.rkt")
(require "railwaySystem.rkt")

(provide server)
(provide startInfrabel)
(provide railwayManager)

(define railwayManager (new RailwayManager%))
(define fileReader (new FileReader%))
(define railwayGraph (new RailwayGraph%))
(define securityProtocol (new SecurityProtocol%))
(define server (new Server%))
(define interface (new Interface%))
(define railwaySystem (new RailwaySystem%))


(define (startInfrabel)

  (send fileReader initialise! railwayManager)
  (send railwayGraph initialise! railwayManager)
  (send securityProtocol initialise! railwayManager)
  (send server initialise! interface)
  (send interface initialise! railwayManager railwaySystem fileReader railwayGraph)

)

