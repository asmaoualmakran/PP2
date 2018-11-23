#lang racket

(require racket/class)

(define Railway%
  (class object%
    (super-new)

    (init-field railwayManager)

    (define/public (init)
      'test)

    (define/public (update!)
      'test)

    ))