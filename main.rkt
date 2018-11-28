#lang racket

(require "NMBS/trainManager.rkt")
(require "Infrabel/railwayManager.rkt")
(require "GUI/gui.rkt")



(define trainManager (make-object TrainManager%))

(define railwayManager (make-object RailwayManager%))

(define gui (make-object GUI%))