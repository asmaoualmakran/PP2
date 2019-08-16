#lang racket

(require "railwayManager.rkt")
(require "fileReader.rkt")
(require "railwayGraph.rkt")
(require "securityProtocol.rkt")
(require "../TCP/server.rkt")
(require "interface.rkt")
(require "railwaySystem.rkt")
(require "railwayController.rkt")

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
(define railwayController (new RailwayController%))


(define (startInfrabel)

  (send fileReader initialise! railwayManager)
  (send railwayGraph initialise! railwayManager)
  (send securityProtocol initialise! railwaySystem railwayController)
  (send server initialise! interface)
  (send interface initialise! railwayManager railwaySystem fileReader railwayGraph)
  (send railwayController initialise! railwayManager)
  (send railwayManager setSecurity! securityProtocol)

)

