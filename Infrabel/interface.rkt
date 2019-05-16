#lang racket

(require "railwayManager.rkt")
(require "simulatorZetHetJuistepad")


(define Interface%
  (class object%
    (super-new)

    (field [railwayManager 'none])

    (define railwayManObj 'object:RailwayManager%)

    (define railwayManFunc (make-hash))
    (define railwayFunc    (make-hash))

    (define/public (initialse! railwayManager)
    'body
    )

    (define/public (initialised?)
    'body
    )
    
    (define/private (addBasicFunctions!)
    'body
    )

    (define/public (callFunction lst)
    'body
    )

    ))