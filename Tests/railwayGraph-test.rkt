#lang racket
(require rackunit)

(require "../Infrabel/railwayManager.rkt")
(require "../Infrabel/fileReader.rkt")
(require "../NMBS/trainManager.rkt")
(require "../Infrabel/railwayGraph.rkt")

(let ([railwayManager (new RailwayManager%)]
      [trainManager (new TrainManager%)]
      [fileReader (new FileReader%)]
      [railwayGraph (new RailwayGraph%)])

  'body)