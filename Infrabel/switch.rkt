#lang racket

(require racket/class)
(require "infrastructure.rkt")

(provide Switch%)

;------------------------------------------------
; Class: Switch%
; Parameters: n/a 
; Use: Create an object that represents a switch.
;------------------------------------------------

(define Switch%
  (class Infrastructure%
    (super-new)

    (field 
           [state       'uninitialised]
           [direction   'uninitialised])

;-------------------------------------------------------
; Variable: connections : list
;      Use: All the connected objects.
; Use: Get the connections field from the super class.
;-------------------------------------------------------

    (define connections (generic DriveableObject% getConnections))
    (define maximalConnections (generic DriveableObject% getMaximalConnections))

;----------------------------------------------------
    (define/public (initialised?)
      (and (not(eq? state 'uninitialised))
           (not(eq? direction 'uninitialised))
           (send DriveableObject% initialised?)))

    (define/public (initState! newstate)
      (if (and(or (eq? newstate 'left)
                  (eq? newstate 'right))
              (not(initialised?)))       ; only allowed to initialise the fields, when they are uninitialised.
          (set! state newstate)
          (error "Switch% initState!: State not initialised or given direction is not correct" newstate)))
    

    (define/public (setState!)
      (cond ((eq? state 'uninitialised)(error "Switch% initState!: State not initialised" state))
            ((eq? state 'left)(set! direction 'right))
            (else(set! state 'right))))

    (define/public (getState)
      (if (initialised?)
          state
          (error "Switch% initState!: State not initialised or given direction is not correct")))


    (define/public (setDirection! dir)
      (if (and (number? dir)
               (or (= 1 dir)
                   (= 2 dir)))    ;For the direction, numbers 1 and 2 are used.
          (set! direction dir)
          (error "Switch% setDirection!: contract violation, numbers 1 or 2 expected give" dir)))
          

    ))