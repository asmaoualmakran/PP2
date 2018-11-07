#lang racket

;TODO: alle functies checken en testen + errors aanpassen

(require racket/class)

(provide DriveableObject%)
;---------------------------------------------------
; Class: DriveableObject%
; Parameters: n/a 
; Use: This is the super class used by track and switch.
;---------------------------------------------------

(define DriveableObject%
  (class object%
    (super-new)
    (field [ID          'uninitialised]
           [connections 'uninitialised]
           [x           'uninitialised]
           [y           'uninitialised]
           [available   'uninitialised]
           [speedlimit  'uninitialised]
           [maximalConnections 'uninitialised])    ;TODO write getter and setter

;-------------------------------------------------------
; Function: initialised?
; Parameters: n/a
; Output: boolean
; Use: Determine if the object is completly initialised.
;-------------------------------------------------------

    (define/public (initialised?)
      (if (and (eq? ID 'uninitialised)
               (eq? connections 'uninitialised)
               (eq? x 'uninitialised)
               (eq? y 'uninitialised)
               (eq? available 'uninitialised)
               (eq? maximalConnections 'uninitialised)
               (eq? speedlimit 'uninitialised))  ; if one is not initialised #f is returned and warining is displayed
          (begin #f
                 (display "DriveableObject% initialised?: not all fields are initialised"))
        #t))

    ;TODO: write initialiser
;-----------------------------------------------------------
; Function: setID! 
; Parameters:
;      id: symbol
;        Use: The identifiaction of the driveableObject.
; Output: n/a
; Use: Initialise the id of the driveableObject.
;----------------------------------------------------------
    
    (define/public (setID! id)
      (if (eq? ID 'uninitialised)
      (set! ID id)
      (error "DriveableObject% setID!: ID is already initialised" ID)))
    
;--------------------------------------------------------
; Function: getID
; Parameters: n/a
; Output:
;      ID: symbol
;        Use: The identification of the driveableObject.
; Use: Retreive the ID of the driveableObject.
;--------------------------------------------------------

    (define/public (getID)
      ID)
    
;-------------------------------------------------------------
; Function: setMaximalConnections!
; Parameters:
;      number: number
;         Use: The number of connections the object can have.
; Output: n/a
; Use: Set the maximal connections the object can have.
;-------------------------------------------------------------

    (define/public (setMaximalConnections! number)
      (if (and (number? number)
               (<= 1 number))     ; minimal number for a track is 1, this enables to set dead ends.
          (set! maximalConnections number)
          (error "DriveableObject% setMaximalConnections!: number of connections need to be at least 1 given" number)))

;-------------------------------------------------------------
; Function: getMaximalConnections!
; Parameters: n/a
; Output:
;    maximalConnections: number
;       Use: The number of connections the object can have.
; Use: Retreive the maximal connections the object can have.
;-------------------------------------------------------------

    (define/public (getMaximalConnections)
      maximalConnections)

;-----------------------------------------------------
; Function: SetConnectionID!
; Parameters:
;   id: symbol
;     use: The identification of the connected track.
; Output: n/a
; Use: Set the connections with an other track.
;-----------------------------------------------------
    
    (define/public (setConnectionID! id)
      (cond ((not(symbol? id))(error "DriveableObject% setConnectionID!: symbol expected received" id ID))
            ((<= (length connections) maximalConnections)(error "DriveabelObject% setConnectionID!: no locations free, please delete one before adding" ID))  ;length of correct list is longer than 2 when full
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

    (define/public (getConnections)
      (if (or (symbol? connections)(null? connections))            ; is connections is a symbol or null, it needs to be initialised
          (print "DriveabelObject% getConnections: no connections are set, please add connections" ID)
          (connections)))
    
;--------------------------------------------------------------------    
; Function: deleteConnection!
; Parameters:
;    id: symbol
;     Use: The identification of the track connection to be deleted.
; Output: n/a
; Use: Delete a connection with an other track.
;--------------------------------------------------------------------

    (define/public (deleteConnection! id)
      (cond ((not (symbol? id))(error "DriveableObject% deleteConnection!: contract violation, expected symbol given" id ID))
            ((or(null? connections)
                (eq? connections 'uninitialised))(error "DriveableObject deleteConnection!: there is no connection to be deleted" connections ID))
            ((memq id connections)(set! connections (remove id connections)))
            (else (error "DriveableObject% deleteConnection!: the given connection is not connected to this track" id ID))))
      

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
          (error "DriveableObject% setLocation!: contract violation expected pair, given" coordinate ID)
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
      (if (or (eq? x 'uninitialised)
                 (eq? y 'uninitialised))
          (error "DriveableObject% getLocation: Location is not initialised, please initialise" ID)
          ((cons x y))))
    

;-----------------------------------------------
; Function: setAvailable!
; Parameters:
;     boolean: boolean
;       Use: The availability of the track.
; Output: n/a
; Use: Set the availability of the track.
;-----------------------------------------------

    (define/public (setAvailable! boolean)
      (if (boolean? boolean)
          (set! available boolean)
          (error "DriveableObject% setAvailable!: contract violation, boolean expected, given" boolean ID)))

;-----------------------------------------------------------------
; Function: getAvailable
; Parameters: n/a
; Output:
;    available: boolean
;      Use: boolean to determine the availability of the track.
; Use: Get the availability of the track.
;-----------------------------------------------------------------

    (define/public (getAvailable)
      (if (eq? available 'uninitialised)
          (error "DriveableObect% getAvailable: available is not initialised please initialise before use" ID)
           available))

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
          (error "DriveableObject% setSpeedlimit!: contract violation, expected number received" speed ID)))

;----------------------------------------
; Function: getSpeedlimit
; Parameters: n/a
; Output:
;   speedlimit: number
;     Use: The speedlimit on the track.
; Use: Get the speedlimit on the track.
;----------------------------------------

    (define/public (getSpeedlimit)
      (if (eq? speedlimit 'uninitialised)
          (error "DriveableObject% getSpeedlimit available is not initialised please initialise before use" ID)
           speedlimit))
    ))
;----------------------------------------------------------------------
;test code
;----------------------------------------------------------------------
(define test (new DriveableObject%))