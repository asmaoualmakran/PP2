#lang racket

(provide RouteManager%)

(define RouteManager%
  (class object%
   (super-new)

    (field [railwayManager 'none])
    
    (define routeTable (make-hash))

    (

    ))