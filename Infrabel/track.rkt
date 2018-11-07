#lang racket

(require racket/class)
(require "driveableObject.rkt")

(provide Track%)
;------------------------------------------------
; Class: Track%
; Parameters: n/a 
; Use: Create an object that represents a track.
;------------------------------------------------

(define Track%
  (class DriveableObject%
    (super-new)
    
    (field
           [length      'uninitialised]
           [curve       'uninitialised])

;-------------------------------------------------------
; Variable: connections : list
;      Use: All the connected objects.
; Use: Get the connections field from the super class.
;-------------------------------------------------------

  ;  (define connections (get-field connections DriveableObject%))
    ;super form
     (define connections (generic DriveableObject% getConnections))
    (define maximalConnections (generic DriveableObject% getMaximalConnections))

;-----------------------------------------------------------------
; Function: initialised?
; Parameters: n/a
; Output: boolean
;   use: Determine if all fields are set.
; Use: Predicate to determine wheter all fields are initialised.
;-----------------------------------------------------------------

    (define/public (initialised?)
      (and (not (eq? length 'uninitialised))
           (not (eq? curve  'uninitialised))
           (send DriveableObject% initialised?)))    ;make sure that all the fields of the super class are set too
    
;-----------------------------------------------------
; Function: setConnectionID!
; Parameters:
;   id: symbol
;     use: The identification of the connected track.
; Output: n/a
; Use: Set the connections with an other track.
;-----------------------------------------------------
    
    (define/override (setConnectionID! id)
      (cond ((not(symbol? id))(error "Track% setConnectionID!: symbol expected received" ))
            ((<= (length connections) 3)(error "Track% setConnectionID!: no locations free, please delete one before adding"))  ;length of correct list is longer than 2 when full
            (else
             (if (symbol? connections)
                 (set! connections (list id))
             (set! connections (append id connections))))))
    
;-----------------------------------------------------
; Function: getConnections
; Parameters: n/a
; Output:
;      connections: list<symbol,symbol>
;         Use: The connections of the track.
; Use: Get the connections with an other track.
;-----------------------------------------------------

    ;TODO Dit herbedenken/schrijven

    (define/override (getConnections)   ;TODO check is "send" or "send-generic" is needed to call this function.
      (if (null? connections)           ; is connections symb null, it needs to be initialised
          (print "Track% getConnections: no connections are set, please add connections")
          (connections)))
    

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