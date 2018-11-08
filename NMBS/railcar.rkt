#lang racket

(require racket/class)
(require "moveableObject.rkt")

(provide Railcar%)

(define Railcar%
  (class MoveableObject%
    (super-new)

    (field
     [capacity 'uninitialised])

    
;-------------------------------------------------------
; Function: initialised?
; Parameters: n/a
; Output:
;    boolean: boolean
;     Use: Determine wheter the object is initialised.
; Use: Determine wheter the object is initialised.
;-------------------------------------------------------

    (define/public (initialised?)
      (not(eq? capacity 'uninitialised)))

    (define/public (setCapacity! number)
      (if (and (number? number)
               (> number 0))
          (set! capacity number)
          (error "Railcar% setCapacity!: contract violation expected positive number received" number)))

    (define/public (getCapacity)
      (if (initialised?)
          capacity
          (error "Railcar% getCapacity: object not initialised, please initialise before use")))
    
    ))