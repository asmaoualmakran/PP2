#lang racket

;TODO: alle functies checken en testen 

(require racket/class)

(provide Infrastructure%)
;---------------------------------------------------
; Class: Infrastructure%Object%
; Parameters: n/a 
; Use: This is the super class used by track and switch.
;---------------------------------------------------

(define Infrastructure%
  (class object%
    (super-new)
    (field [ID          'uninitialised]
           [available   'uninitialised]
           [speedlimit  'uninitialised]
           [standardSpeed 100]
           [x 'uninitialised]
           [y 'uninitialised])    

    ;-------------------------------------------------------
    ; Function: init?
    ; Parameters: n/a
    ; Output: boolean
    ; Use: Determine if the object is completly initialised.
    ;-------------------------------------------------------

    (define/private (init?)
      (and (not (eq? ID 'uninitialised))
           (not (eq? available 'uninitialised))))

    ;-----------------------------------------------------------
    ; Function: setID! 
    ; Parameters:
    ;      id: symbol
    ;        Use: The identifiaction of the Infrastructure object.
    ; Output: n/a
    ; Use: Initialise the id of the Infrastructure object.
    ;----------------------------------------------------------
    
    (define/public (setID! id)
      (if (symbol? id)
          (if (eq? ID 'uninitialised)      ;ID can only be set at initialisation
              (set! ID id)
              (error "Infrastructure% setID!: ID is already initialised" ID))
          (error "Infrastructure% setID!: Contract violation expected a symbol, recieved:" id)))
    
    ;--------------------------------------------------------
    ; Function: getID
    ; Parameters: n/a
    ; Output:
    ;      ID: symbol
    ;        Use: The identification of the Infrastructure object.
    ; Use: Retreive the ID of the Infrastructure object.
    ;--------------------------------------------------------

    (define/public (getID)
      (if (init?)
          ID
          (error "Infrastructure% getID: object is not initialised, please initialise before use")))

    ;------------------------------------------------------------------
    ; Function: setLocation!
    ; Parameters:
    ;      coordinate: pair
    ;        Use: Set the location of the track.
    ; Output: n/a
    ; Use: Set the location of the track.
    ;-------------------------------------------------------------------

    (define/public (setLocation! coordinate)
      (if (or (null? coordinate)
              (not (pair? coordinate)))
          (error "Infrastructure% setLocation!: contract violation expected pair, given" coordinate ID)
          (begin (set! x (car coordinate))
                 (set! y (cdr coordinate))
                 (display "location set to" coordinate))))
      
    ;-------------------------------------------------    
    ; Function: getLocation
    ; Parameters: n/a
    ; Output:
    ;     coordinate: pair
    ;         Use: The location of the track.
    ; Use: Get the location of the track, in a pair.
    ;-------------------------------------------------

    (define/public (getLocation)
      (if (init?)
          (cons x y)
          (error "Infrastructure% getLocation: object not initialised please initialise before use")))
    

    ;-----------------------------------------------
    ; Function: setAvailable!
    ; Parameters:
    ;     av: boolean or symbol
    ;       Use: The availability of the track.
    ; Output: n/a
    ; Use: Set the availability of the track.
    ;-----------------------------------------------

    (define/public (setAvailable! av)
      (if (or(boolean? av)
             (symbol? av))
          (set! available av)
          (error "Infrastructure% setAvailable!: contract violation, boolean or symbol expected, received" av ID)))

    ;-----------------------------------------------------------------
    ; Function: getAvailable
    ; Parameters: n/a
    ; Output:
    ;    available: boolean
    ;      Use: boolean to determine the availability of the track.
    ; Use: Get the availability of the track.
    ;-----------------------------------------------------------------

    (define/public (getAvailable)
      (if (init?)
          available
          (error "DriveableObect% getAvailable: available is not initialised please initialise before use" ID)))

    ;-------------------------------------------------------------
    ; Function: setSpeedlimit!
    ; Parameters:
    ;   speed: number
    ;     Use: The speedlimit to where it needs to be set.
    ; Output: n/a
    ; Use: Set the speedlimit for a track, this must be positive.
    ;-------------------------------------------------------------

    (define/public (setSpeedlimit! speed)
      (if (and(number? speed)
              (<= 0 speed))         ;speedlimit needs to be positive
          (set! speedlimit speed)
          (error "Infrastructure% setSpeedlimit!: contract violation, expected positive number received" speed ID)))

    ;----------------------------------------
    ; Function: getSpeedlimit
    ; Parameters: n/a
    ; Output:
    ;   speedlimit: number
    ;     Use: The speedlimit on the track.
    ; Use: Get the speedlimit on the track.
    ;----------------------------------------

    (define/public (getSpeedlimit)
      (if (init?)
          speedlimit
          (error "Infrastructure% getSpeedlimit available is not initialised please initialise before use" ID)))
    ))