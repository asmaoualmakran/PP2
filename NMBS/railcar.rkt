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

    (define (initialised?)
      (not(eq? capacity 'uninitialised)))
    (augment initialised?)

    ;--------------------------------------
    ; Function: setCapacity!
    ; Parameters:
    ;     number: number
    ;      Use: The railcar's capacity.
    ; Output: n/a
    ; Use: Set the railcar's capacity.
    ;--------------------------------------
    
    (define/public (setCapacity! number)
      (if (and (number? number)
               (> number 0))
          (set! capacity number)
          (error "Railcar% setCapacity!: contract violation expected positive number received" number)))

    ;-------------------------------------------
    ; Function: getCapacity
    ; Parameters: n/a
    ; Output:
    ;     capacity: number
    ;       Use: The capacity of the railcar.
    ; Use: Retrieve the railcar's capacity.
    ;-------------------------------------------
    
    (define/public (getCapacity)
      (if (initialised?)
          capacity
          (error "Railcar% getCapacity: object not initialised, please initialise before use")))
    
    ))