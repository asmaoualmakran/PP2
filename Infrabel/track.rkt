#lang racket

(require racket/class)
(require "infrastructure.rkt")

(provide Track%)

;------------------------------------------------
; Class: Track%
; Parameters: n/a 
; Use: Create an object that represents a track.
;------------------------------------------------

(define Track%
  (class Infrastructure%
    (super-new)
    
    (field
     ;   [length      'uninitialised]
     [connections 'uninitialised]
     ;   [curve       'uninitialised]
     [detectionID 'uninitialised])

    ;-----------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output: boolean
    ;   use: Determine if all fields are set.
    ; Use: Predicate to determine wheter all fields are initialised.
    ;-----------------------------------------------------------------

    (define (init?)
      ;    (and (not (eq? length 'uninitialised))
      ;         (not (eq? curve  'uninitialised))
      (and (not (eq? detectionID 'uninitialised))
           (not (eq? connections 'uninitialised)))
      )
    (augment init?)

    (define (init!)
      (init!))
    (augment init!)

    (define/public (initialised?)
      (init?))

    (define/public (initialise!)
      (init!)
      (setDetectionblockID! 'none)
      (set! connections (list 'none 'none))
      )

    ;-------------------------------------------------------------------------------------------------------
    ; Function: setConnections!
    ; Parameters:
    ;       con1: symbol
    ;        Use: One of the connections
    ;       con2: symbol
    ;        Use: One of the connections
    ; Output: n/a
    ; Use: Add connections to the track, the connections are the ID of the to be connected railway objects.
    ;-------------------------------------------------------------------------------------------------------
    
    (define/public (setConnections! con1 con2)
     
      (if (and (symbol? con1)
               (symbol? con2))
          (set! connections (list con1 con2))
          (error "Track% setConnections!: Contract violation, expected two symbols, recieved:" con1 con2)))

    ;-------------------------------------------------------------------------
    ; Function: setConnection!
    ; Parameters:
    ;        con: symbol
    ;         Use: The connection that needs to be added.
    ; Output: n/a
    ; Use: Add a single connection is there is room for an extra connection.
    ;-------------------------------------------------------------------------
   
    (define/public (setConnection! con)
      (if (initialised?)
          (if (symbol? con)
              (cond ((eq? (car (getConnections)) 'none)(set! connections (list con (cadr (getConnections)))))
                    ((eq? (cadr (getConnections)) 'none)(set! connections (list (car (getConnections)) con)))
                    (else (error "Track% setConnection!: All connections are in use")))
              (error "Track% setConnection!: Contract violation expected a symbol, recieved:" con))
          (error "Track% setConnection!: Object is not initialised, please initialise before use.")))
    
    ;------------------------------------------------------------------
    ; Function: getConnecions
    ; Parameters: n/a
    ; Output:
    ;      connections: pair<symbol>
    ;        Use: The pair that contains the connections of the track.
    ; Use: Get the connections of the track.
    ;------------------------------------------------------------------
    
    (define/public (getConnections)
      (if (initialised?)
          connections
          (error "Track% getConnections!: Object is not initialised, please initialse before use")))

    ;---------------------------------------------------
    ; Function: hasConnections?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;       Use: Determine if there are connections.
    ; Use: Determine if the track has connections.
    ;---------------------------------------------------
    
    (define/public (hasConnections?)
      (if (initialised?)
          (or (not (null? (getConnections)))
              (not (and (eq? 'none (car (getConnections)))
                        (eq? 'none (cdr (getConnections))))))
          (error "Track% hasConnections?: Object is not initialised, please initialise before use")))

    ;-----------------------------------------------------------------
    ; Function: deleteConnection!
    ; Parameters:
    ;       con: symbol
    ;        Use: ID of the connection that needs to be deleted.
    ;       arg: symbol
    ;        Use: ID of the connection that needs to be deleted.
    ; Output: n/a
    ; Use: Delete one of two connections from the connection lists.
    ;-----------------------------------------------------------------
    
    (define/public (deleteConnection! con . arg)
      (if (initialised?)
          (if (not (null? (getConnections)))
              (if (null? arg)
                  (if (member con (getConnections))
                  
                      (cond ((eq? (car (getConnections)) con)(setConnections!  'none (cdr (getConnections))))
                            ((eq? (cdr (getConnections)) con)(setConnections! (car (getConnections)) 'none)))
                      (error "Track% deleteConnection!: Given ID does not belong to the track's connections, recieved:" con))

                  (let ([atom? (lambda (x)
                                 (and (not (null? x))
                                      (not (pair? x))))])
                    (if (atom? arg)
                        (if (and (member con)
                                 (member arg))
                            (setConnections! 'none 'none)
                            (error "Track% deleteConnection!: Given ID's does not belong to the track's connections, recieved:" con arg))
                        (error "Track% deleteConnecion!: Contract violation expected two or one arguments, recieved:" con arg))))
                        
              (error "Track% deleteConnection!: Object has no connections, no connections can be deleted."))
          (error "Track% deleteConnection!: Object is not initialised, please initialise before use.")))
          
    ;--------------------------------------------------------------------
    ; Function: getDetectionblockID
    ; Parameters: n/a
    ; Output:
    ;     id: symbol
    ;      Use: The identification of the connected detectionblock.
    ; Use: Retrieve the identification of the connected detectionblock.
    ;--------------------------------------------------------------------

    (define/public (getDetectionblockID)
      (if (and (initialised?)
               (hasDetectionblock?))
          detectionID
          (error "Track% getDetectionblock: object is not initialised please initialise before use" detectionID)))
    
    ;----------------------------------------------------------------------------
    ; Function: setDetectionblockID!
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the detectionblock that need to be set.
    ; Output: n/a
    ; Use: Set the identification of the relationg detectionblock.
    ;----------------------------------------------------------------------------

    (define/public (setDetectionblockID! id)
          (when (symbol? id)
            (set! detectionID id)))

    ;----------------------------------------------------------------
    ; Function: deleteDetectionblock!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Delete the identification of the relating detectionblock.
    ;----------------------------------------------------------------
    
    (define/public (deleteDetectionblock!)
      (if (initialised?)
          (set! detectionID 'none)
          (error "Track% deleteDetectionblock!: object is not initialised please initialise before use")))

    ;--------------------------------------------------------
    ; Function: hasDetectionblock?
    ; Parameters: n/a
    ; Output
    ;    boolean: boolean
    ;      Use: Determine if a track has a detectionblock.
    ; Use: Determine if a track has a detectionblock.
    ;-------------------------------------------------------

    (define/public (hasDetectionblock?)
      (and (initialised?)
           (not (eq? detectionID 'none))))
                
    ;--------------------------------------
    ; Function: setLength!
    ; Parameters:
    ;     numb: number
    ;       Use: The length of the track. 
    ; Output: n/a
    ; Use: Set the length of the track.
    ;--------------------------------------

    ;  (define/public (setLength! numb)
    ;    (if (number? numb)
    ;        (set! length numb)
    ;        (error "Track% setLength!: contract violation expected number, received" numb)))

    ;-------------------------------------
    ; Function: getLength
    ; Parameters: n/a
    ; Output:
    ;   lenght: number
    ;     Use: The length of the track.
    ; Use: Get the length of the track.
    ;-------------------------------------
    
    ; (define/public (getLength)
    ;   (if (eq? length 'uninitialised)
    ;       (error "Track% getLength: length is not initialised, please initialise before use")
    ;       (length)))

    ;-------------------------------------------------------------
    ; Function: setCurve!
    ; Parameters:
    ;     cur: number
    ;      Use: The size of the curve and direction in degrees.
    ; Output: n/a
    ; Use: Set the size of the curve and the direction.
    ;-------------------------------------------------------------

    ;   (define/public (setCurve! cur)
    ;     (if (number? cur)
    ;         (set! curve cur)
    ;        (error "Track% setCurve!: contract violation, number is expected, received" cur)))

    ;--------------------------------------------------
    ; Function: getCurve
    ; Parameters: n/a
    ; Output:
    ;    curve: number
    ;      Use: The size and the direction of the curve.
    ; Use: Get the size of the curve and it's direction.
    ;---------------------------------------------------

    ;   (define/public (getCurve)
    ;     (if (eq? curve 'uninitialised)
    ;         (error "Track% getCurve: curve is not initialised, please initialised before use")
    ;         (curve)))
    ))