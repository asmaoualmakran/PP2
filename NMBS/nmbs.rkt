#lang racket
(require graph)

(require "trainManager.rkt")
(require "routeCalculator.rkt")
(require "routeManager.rkt")

(provide trainManager)
(provide routeCalculator)
(provide startNmbs)

(define trainManager (new TrainManager%))
(define routeCalculator (new RouteCalculator%))
(define routeManager (new RouteManager%))

(define (startNmbs railwayMan railwaygraph)
  (if (eq? (object-name railwayMan) 'object:RailwayManager%)
      
      (if (send railwayMan initialised?)
             
          (send routeManager initialise! trainManager routeCalculator) ;railwaygraph)
          (error "nmbs startNmbs: Cannot start nmbs before railwaymanager is initialised"))
      (error "nmbs startNmbs: Contract violation expected route manager received" railwayMan)))