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

    (define connections (generic Infrastructure% getConnections))
    (define maximalConnections (generic Infrastructure% getMaximalConnections))

;----------------------------------------------------
; Funtion: initialised?
; Parameters: n/a
; Output:
;   boolean: boolean
;     Use: Wheter or not the object is initialised.
; Use: Determine if the object is initialised.
;----------------------------------------------------
    (define (initialised?)
      (and (not(eq? state 'uninitialised))
           (not(eq? direction 'uninitialised))))
    (augment initialised?)

;-------------------------------------------
; Function: initState!
; Parameters:
;     newstate: symbol
;       Use: The state of the switch.
; Output: n/a
; Use: Initialise the state of the switch.
;-------------------------------------------

    (define/public (initState! newstate)
      (if (and(or (eq? newstate 'left)
                  (eq? newstate 'right))
              (not(initialised?)))       ; only allowed to initialise the fields, when they are uninitialised.
          (set! state newstate)
          (error "Switch% initState!: State not initialised or given direction is not correct" newstate)))

;----------------------------------------
; Funtion: setState!
; Parameters: n/a
; Output: n/a
; Use: Change the state of the switch.
;---------------------------------------

    (define/public (setState!)
      (cond ((initialised?)(error "Switch% initState!: State not initialised" state))
            ((eq? state 'left)(set! direction 'right))
            (else(set! state 'right))))

;--------------------------------------
; Function: getState
; Parameters: n/a
; Output:
;    state: symbol
;      Use: The state of the switch.
; Use: Get the state of the switch.
;--------------------------------------
    
    (define/public (getState)
      (if (initialised?)
          state
          (error "Switch% initState!: State not initialised or given direction is not correct")))

;--------------------------------------------------------
; Function: setDirection!
; Parameters:
;    dir: number
;     Use: The driving direction of the switch.
; Output: n/a
; Use: Setting the dirving direction of the switch.
;--------------------------------------------------------
    
    (define/public (setDirection! dir)
      (if (and (number? dir)
               (or (= 1 dir)
                   (= 2 dir)))    ;For the direction, numbers 1 and 2 are used.
          (set! direction dir)
          (error "Switch% setDirection!: contract violation, numbers 1 or 2 expected give" dir)))

;----------------------------------------------------------
; Function: getDirection
; Parameters: n/a
; Output:
;   direction: number
;      Use: The driving direction of the switch.
; Use: Retrieving the diriving direction of the switch.
;----------------------------------------------------------
    
    (define/public (getDirection)
      (if (initialised?)
          direction
          (error "Switch% getDirection: Object is not initialised, please initialise before use.")))
    ))