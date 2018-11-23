#lang racket

(require racket/class)
(require "infrastructure.rkt")

(provide Track%)

;TODO Look at the connections methods
;------------------------------------------------
; Class: Track%
; Parameters: n/a 
; Use: Create an object that represents a track.
;------------------------------------------------

(define Track%
  (class Infrastructure%
    (super-new)
    
    (field
     [length      'uninitialised]
     [curve       'uninitialised]
     [detectionID 'uninitialised])
    (inherit-field connections
                   maximalConnections)


    ;-----------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output: boolean
    ;   use: Determine if all fields are set.
    ; Use: Predicate to determine wheter all fields are initialised.
    ;-----------------------------------------------------------------

    (define (initialised?)
      (and (not (eq? length 'uninitialised))
           (not (eq? curve  'uninitialised))
           (not (eq? detectionID 'uninitialised))))
    (augment initialised?)

    ;---------
    ;TODO
    ;---------

    (define/public (setDetectionblock! id)
      (if (initialised?)
          (set! detectionID id)
          (error "Track% setDetectionblock!: object is not initialised please initialise before use")))

    ;-------
    ;TODO
    ;-------
    
    (define/public (deleteDetectionblock!)
      (if (initialised?)
          (set! detectionID 'none)
          (error "Track% deleteDetectionblock!: object is not initialised please initialise before use")))


    ;--------------------------------------
    ; Functie: setLength!
    ; Parameters:
    ;     numb: number
    ;       Use: The length of the track. 
    ; Output: n/a
    ; Use: Set the length of the track.
    ;--------------------------------------

    (define/public (setLength! numb)
      (if (number? numb)
          (set! length numb)
          (error "Track% setLength!: contract violation expected number, received" numb)))

    ;-------------------------------------
    ; Function: getLength
    ; Parameters: n/a
    ; Output:
    ;   lenght: number
    ;     Use: The length of the track.
    ; Use: Get the length of the track.
    ;-------------------------------------
    
    (define/public (getLength)
      (if (eq? length 'uninitialised)
          (error "Track% getLength: length is not initialised, please initialise before use")
          (length)))

    ;-------------------------------------------------------------
    ; Function: setCurve!
    ; Parameters:
    ;     cur: number
    ;      Use: The size of the curve and direction in degrees.
    ; Output: n/a
    ; Use: Set the size of the curve and the direction.
    ;-------------------------------------------------------------

    (define/public (setCurve! cur)
      (if (number? cur)
          (set! curve cur)
          (error "Track% setCurve!: contract violation, number is expected, received" cur)))

    ;--------------------------------------------------
    ; Function: getCurve
    ; Parameters: n/a
    ; Output:
    ;    curve: number
    ;      Use: The size and the direction of the curve.
    ; Use: Get the size of the curve and it's direction.
    ;---------------------------------------------------

    (define/public (getCurve)
      (if (eq? curve 'uninitialised)
          (error "Track% getCurve: curve is not initialised, please initialised before use")
          (curve)))
    ))