#lang racket

(require racket/class)
(require "moveableObject.rkt")

(provide Locomotive%)

(define Locomotive%
  (class MoveableObject%
    (super-new)

    (field
     [direction 'unitialised])

;-------------------------------------------------------
; Function: initialised?
; Parameters: n/a
; Output:
;    boolean: boolean
;     Use: Determine wheter the object is initialised.
; Use: Determine wheter the object is initialised.
;-------------------------------------------------------

    (define/public (initialised?)
      (not (eq? direction 'uninitialised)))

;-----------------------------------------------------------------------------------------------
; Function: getDirection
; Parameters: n/a
; Output:
;   direction: symbol
;     Use: The driving direction of the train.
; Use: Retrieving the driving direction of the locomotive, this can be either 'left or 'right.
;-----------------------------------------------------------------------------------------------

    (define/public (getDirection)
      (if (initialised?)
          direction
          (error "Locomotive% getDirection: object not initialised please initialise before use")))

;------------------------------------------------------------------------------------------
; Function: setDirection!
; Parameters:
;    direct: symbol
;      Use: The driving direction of the locomotive
; Output: n/a
; Use: Setting the driving direction of the locomotive, this can be eiter 'left or 'right'
;-------------------------------------------------------------------------------------------

    (define/public (setDirection! direct)
      (if (and (symbol? direct)
               (or (eq? direct 'left)
                   (eq? direct 'right)))
          (set! direction direct)
          (error "Locomotive% setDirection!: no correct direction given expected symbol 'left 'right received" direct)))
    
          
    
    ))