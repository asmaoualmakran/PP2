#lang racket

(require "../gui/gui.rkt")
(require "trainManager.rkt")
(require "routeManager.rkt")
(require "routeCalculator.rkt")
(require "interface.rkt")
(require "../TCP/client.rkt")

(provide client)
(provide GUI)
(provide startNMBS)

(define GUI (new GUI%))
(define trainManager (new TrainManager%))
(define routeManager (new RouteManager%))
(define routeCalculator (new RouteCalculator%))
(define interface (new Interface%))
(define client (new Client%))


(define (startNMBS)
    
    (send trainManager initialise! client)
    (send routeManager initialise! client trainManager routeCalculator)
    (send routeCalculator initialise! client)
    (send interface initialise! trainManager routeManager)
    (send client initialise! interface)
    (send GUI initialise! client routeManager)
)

