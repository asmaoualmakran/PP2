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
      ;   ;  [connections 'uninitialised]  -> already in the sub classes
       ;    [x           'uninitialised]
        ;   [y           'uninitialised]
            [available   'uninitialised])
          ; [speedlimit  'uninitialised]
          ; [maximalConnections 'uninitialised])    

    ;-------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output: boolean
    ; Use: Determine if the object is completly initialised.
    ;-------------------------------------------------------

    (define/pubment (init?)
     (and (not (eq? ID 'uninitialised?))
          (not (eq? available 'unitialised))))

;    (define/pubment (initialised?)     ;using pubment to enable augmenting the method in the subclasses
 ;     (and (not(eq? ID 'uninitialised))
  ;     ;    (not(eq? connections 'uninitialised))
   ;        (not(eq? x 'uninitialised))
    ;       (not(eq? y 'uninitialised))
     ;      (not(eq? available 'uninitialised))
      ;     (not(eq? speedlimit 'uninitialised))
       ;    (not(eq? maximalConnections 'uninitialised))))

    (define/pubment (init!)
      (setAvailable! #t))

    ;---------------------------------------------------
    ; Functions: initConnections!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Initialise the connections to an empty list.
    ;---------------------------------------------------
    
 ;   (define/public (initConnections!)
 ;     (if (not(initialised?))
 ;         (set! connections (list))
 ;         (error "Infrastructure% initConnections: object already initialised, can not initialise connections")))
          

    ;-----------------------------------------------------------
    ; Function: setID! 
    ; Parameters:
    ;      id: symbol
    ;        Use: The identifiaction of the Infrastructure object.
    ; Output: n/a
    ; Use: Initialise the id of the Infrastructure object.
    ;----------------------------------------------------------
    
    (define/public (setID! id)
      (if (eq? ID 'uninitialised)      ;ID can only be set at initialisation
          (set! ID id)
          (error "Infrastructure% setID!: ID is already initialised" ID)))
    
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
    
  

    ;--------------------------------------------------------------------------------------
    ; Function: isConnected?
    ; Parameters:
    ;      id: symbol
    ;       Use: The identifiaction of the object who's connection needs to be checked.
    ; Output:
    ;      boolean: boolean
    ;       Use: Boolean to determine if an object is connected.
    ; Use: Check if the object with the given id is connected to the current object.
    ;--------------------------------------------------------------------------------------
    
;    (define/public (isConnected? id)
;      (memq (getConnections) id))

    ;---------------------------------------------------------------------
    ; Function: connectionAvailable?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;       Use: Boolean to determine if there is a connection available.
    ; Use: Check if there is a connection available.
    ;---------------------------------------------------------------------
    
  ;  (define/public (connectionAvailable?)
  ;    (if (initialised?)
  ;        (< (length connections) maximalConnections)
  ;        (error "Infrastructure% connectionAvailable?: object is not initialised, please initialise before use")))
    
    ;-----------------------------------------------------
    ; Function: addConnectionID!
    ; Parameters:
    ;   id: symbol
    ;     use: The identification of the connected track.
    ; Output: n/a
    ; Use: Set the connections with an other track.
    ;-----------------------------------------------------
    
 ;   (define/public (addConnectionID! id)
 ;     (cond ((not(symbol? id))(error "Infrastructure%% addConnectionID!: symbol expected received" id ID))
 ;           ((>= (length connections) maximalConnections)(error "Infrastructure% setConnectionID!: no locations free, please delete one before adding" ID))  ;length of correct list is longer than 2 when full
 ;           (else
 ;            (if (symbol? connections)   ;if the field is not initialised a list gets created
 ;                (set! connections (list id))
 ;                (set! connections (append id connections))))))  
    
    ;-----------------------------------------------------
    ; Function: getConnections
    ; Parameters: n/a
    ; Output:
    ;      connections: list<symbol,symbol,...>
    ;         Use: The connections of the track.
    ; Use: Get the connections with an other track.
    ;-----------------------------------------------------

 ;   (define/public (getConnections)
 ;     (if (or (not (initialised?))(null? connections))            ; is connections is a symbol or null, it needs to be initialised
 ;         (print "Infrastructure% getConnections: no connections are set, please add connections" ID)
 ;         (connections)))
    
    ;--------------------------------------------------------------------    
    ; Function: deleteConnection!
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the track connection to be deleted.
    ; Output: n/a
    ; Use: Delete a connection with an other track.
    ;--------------------------------------------------------------------

;    (define/public (deleteConnection! id)
;      (cond ((not(initialised?))(error "Infrastructure% deleteConnection!: object not initialised please initialise before use"))
;            ((not (symbol? id))(error "Infrastructure% deleteConnection!: contract violation, expected symbol given" id ID))
;            ((null? connections)(error "Infrastructure% deleteConnection!: there is no connection to be deleted" connections ID))
;            ((isConnected? id)(set! connections (remove id connections)))
;            (else (error "Infrastructure% deleteConnection!: the given connection is not connected to this track" id ID))))
      

    ;------------------------------------------------------------------
    ; Function: setLocation!
    ; Parameters:
    ;      coordinate: pair
    ;        Use: Set the location of the track.
    ; Output: n/a
    ; Use: Set the location of the track.
    ;-------------------------------------------------------------------

  ;  (define/public (setLocation! coordinate)
  ;    (if (or (null? coordinate)
  ;            (not (pair? coordinate)))
  ;        (error "Infrastructure% setLocation!: contract violation expected pair, given" coordinate ID)
  ;        (begin (set! x (car coordinate))
  ;               (set! y (cdr coordinate))
  ;               (display "location set to" coordinate))))
      
    ;-------------------------------------------------    
    ; Function: getLocation
    ; Parameters: n/a
    ; Output:
    ;     coordinate: pair
    ;         Use: The location of the track.
    ; Use: Get the location of the track, in a pair.
    ;-------------------------------------------------

;    (define/public (getLocation)
;      (if (initialised?)
;          (cons x y)
;          (error "Infrastructure% getLocation: object not initialised please initialise before use")))
    

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

 ;   (define/public (setSpeedlimit! speed)
 ;     (if (and(number? speed)
 ;             (<= 0 speed))         ;speedlimit needs to be positive
 ;         (set! speedlimit speed)
 ;         (error "Infrastructure% setSpeedlimit!: contract violation, expected positive number received" speed ID)))

    ;----------------------------------------
    ; Function: getSpeedlimit
    ; Parameters: n/a
    ; Output:
    ;   speedlimit: number
    ;     Use: The speedlimit on the track.
    ; Use: Get the speedlimit on the track.
    ;----------------------------------------

;    (define/public (getSpeedlimit)
;      (if (initialised?)
;          speedlimit
;          (error "Infrastructure% getSpeedlimit available is not initialised please initialise before use" ID)))
    ))