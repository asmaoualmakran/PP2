#lang racket

(require logger)
(require "railwayManager.rkt")
(require "railwaySystem.rkt")


(provide Interface%)

(define Interface%
  (class object%
    (super-new)

    (field [railwayManager 'uninitialised]
           [fileReader     'uninitialised]
           [railwayGraph   'uninitialised]
           [railwaySystem  'uninitialised])

    (define railwayManObj 'object:RailwayManager%)
    (define fileObj       'object:FileReader%)
    (define graphObj      'object:RailwayGraph%)
    (define railSysObj    'object:RailwaySystem%)

    ; The tables that contain the used functions.

    (define railwayManFunc (make-hash))
    (define railwayFunc    (make-hash))

    (define railwayManSym 'railwayManager)
    (define railwaySym 'railway)

    (define path "railwaySetup\\hardware.txt")

    ;---------------------------------------------
    ; Function: initialise!
    ; Parameters: 
    ;       railwayMan: object:RailwayManager%
    ;         Use: Set the railway manager.
    ; Output: n/a 
    ; Use: Initialise the object.
    ;----------------------------------------------

    (define/public (initialise! railwayMan railwaySys reader graph)
      
      (if (and (eq? railwayManObj (object-name railwayMan))
               (eq? railSysObj (object-name railwaySys))
               (eq? fileObj (object-name reader))
               (eq? graphObj (object-name graph)))
         
          (begin
          (set! railwayManager railwayMan)
          (set! railwaySystem railwaySys)
          (set! fileReader  reader)
          (set! railwayGraph graph)
          (addBasicFunctions!)
          (display "Functions added"))

      (error "Interface% initialise!: Contract violation expected a railwayManager object, recieved: " railwayMan)))
  

    (define/private (selectRailway rail)
      (if (symbol? rail)
        (cond ((eq? rail 'Hardware) (send fileReader loadRailway path)
                                    (display(send railwayManager getAllSwitchID)
                                    ))
        
        (else (info "Given railway is unkown please create and add before use, recieved: " rail)))
      
      (error "Interface% selectRailway: Contract violation expected a symbol, recived: " rail)))

    ;----------------------------------------------------
    ; Function: initialised? 
    ; Parameters: n/a 
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if the object is initialised.
    ; Use: Determine if the object is initialised.
    ;-----------------------------------------------------

    (define/public (initialised?)
      (and (not (eq? 'uninitialised railwayManager))
           (not (empty? railwayManFunc))
           (not (empty? railwayFunc))))
    
    ;--------------------------------------------------------------------
    ; Function: addFunction! 
    ; Parameters: 
    ;         dest: symbol
    ;           Use: The table where the function will be stored.
    ;         key: symbol
    ;           Use: The symbol used to hash the function in the table. 
    ;         function: procedure
    ;           Use: The function that needs to be added.
    ; Output: n/a 
    ; Use: Add a function to one of the hashtables.
    ;---------------------------------------------------------------------

    (define/public (addFunction! dest key function)
      (if (and (symbol? key)
               (procedure? function))
          
          (if (and (not (hash-has-key? railwayManFunc key))
                   (not (hash-has-key? railwayFunc key)))
                
                (cond ((eq? dest railwayManSym) (hash-set! railwayManFunc key function)
                                                (display "Function added: ") (display "manager ") (display key) (newline))
                      ((eq? dest railwaySym) (hash-set! railwayFunc key function)
                                              (display "Function added: ") (display "railway ") (display key) (newline))
                (else (error "Interface! addFunction!: Destiantion is unknown, recieved: " dest)))

                (error "Interface% addFunction!: Key is not unique, function already exists, recieved: " key))
               (error "Interface% addFunction!: Contract vioaltion expected a symbol and a function, recieved: " key function)))
    
    ;----------------------------------------------------------------------
    ; Function: addBasicFunctions!
    ; Parameters: n/a 
    ; Output: n/a 
    ; Use: Add basic functions that need to be supported by the interface.
    ;----------------------------------------------------------------------

    (define/private (addBasicFunctions!)
    
    ;RailwayManager functions 

    ;------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         detectionID: symbol
    ;           Use: The ID of the detectionblock that needs to be checked.
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if the given ID belongs to a detectionblock.
    ; Use: Determine if the given ID belongs to a detectionblock.
    ;------------------------------------------------------------------------

    (addFunction! railwayManSym 'isDetectionblock? (lambda (detectionID)
                                                      (send railwayManager isDetectionBlock? detectionID)))

    ;---------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         trackID: symbol
    ;           Use: The ID of the track that needs to be checked.
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if the ID belongs to a track.
    ; Use: Determine if the ID belongs to a track.
    ;----------------------------------------------------------------

    (addFunction! railwayManSym 'isTrack? (lambda (trackID)
                                              (send railwayManager isTrack? trackID)))

    ;-------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       switchID: symbol
    ;         Use: The ID of the switch that needs to be checked.
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if the ID belongs to a track.
    ; Use: Determine if the ID belongs to a switch.
    ;-------------------------------------------------------------

    (addFunction! railwayManSym 'isSwitch? (lambda (switchID)
                                              (send railwayManager isSwitch? switchID)))

    ;--------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         detectionID: symbol
    ;           Use: The ID of the detectionblock who's track needs to be get.
    ; Output: 
    ;      trackID: symbol
    ;       Use: The ID of the track connected to the detectionblock. 
    ; Use: Get the ID of the track connected to the detectionblock.
    ;---------------------------------------------------------------------------

    (addFunction! railwayManSym 'getTrackID (lambda (detectionID)
                                              (let ([detectionblock (send railwayManager getDetectionblock detectionID)]
                                                    [trackID 'none])
                                                (set! trackID (send detectionblock getTrackID))
                                                trackID)))

    ;-----------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         trackID: symbol
    ;           Use: The ID of the track who's detectionblock needs to be get.
    ; Output: 
    ;      detectionID: symbol
    ;       Use: The ID of the detectionblock connected to the track.
    ; Use: Get the ID of the detectionblock connected to the track.
    ;-----------------------------------------------------------------------------

    (addFunction! railwayManSym 'getDetectionID (lambda (trackID)
                                                  (let ([track (send railwayManager getTrack trackID)]
                                                        [detectionID 'none])
                                                      (set! detectionID (send track getDetectionblockID))
                                                      detectionID)))

    ;---------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: n/a 
    ; Output: 
    ;     allDectectionblocks: list<symbol>
    ;       Use: All the detectionblocks in the railway system.
    ; Use: Get all the detectionblocks in the railway system.
    ;----------------------------------------------------------------

    (addFunction! railwayManSym 'getAllDetectionID (lambda ()
                                                      (send railwayManager getAllDetectionblockID)))

    ;----------------------------------------------------------
    ; Function: n/a 
    ; Parameters: n/a 
    ; Output: 
    ;     allSwitches: list<symbol>
    ;       Use: All the switches in the railway system.
    ; Use: Get all the switches in the railway system.
    ;-----------------------------------------------------------

    (addFunction! railwayManSym 'getAllSwitchID (lambda ()
                                                    (send railwayManager getAllSwitchID)))

    ;---------------------------------------------------
    ; Function: n/a 
    ; Parameters: n/a 
    ; Output:
    ;     allTrack: list<symbol>
    ;       Use: All the tracks in the railway system.
    ; Use: Get all the tracks in the railway system.
    ;----------------------------------------------------

    (addFunction! railwayManSym  'getAllTrackID (lambda ()
                                                    (send railwayManager getAllTrackID)))

    ;------------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         switchID: symbol
    ;           Use: The ID of the switch where one of the connections is needed.   
    ;         conn: symbol
    ;           Use: One of the Y connections of the switch.
    ; Output:
    ;     oppositeConnection: symbol
    ;       Use: The connection opposite from conn.
    ; Use: Get the opposite connection of a given Y connection.
    ;------------------------------------------------------------------------------

    (addFunction! railwayManSym 'getOppositeYConnection (lambda (switchID conn)
                                                          (let ([switch (send railwayManager getSwitch switchID)]
                                                                [connection 'none])
                                                              (set! connection (send switch getOppositeYConnection conn))
                                                              connection)))

    ;----------------------------------------------------
    ; Function: n/a
    ; Parameters: 
    ;       ID: symbol
    ;         Use: The ID of the switch or track.
    ; Output: 
    ;     connections: list<symbol>
    ;       Use: The connections of the object.
    ; Use: Get the connection of the object.
    ;----------------------------------------------------

    (addFunction! railwayManSym 'getConnections (lambda (ID)
                                                  (let ([object (send railwayManager getObject ID)]
                                                        [connections 'none])
                                                      (set! connections (send object getConnections))
                                                      connections)))

    ;----------------------------------------------
    ; Fuction: n/a 
    ; Parameters: 
    ;       switchID: symbol
    ;         Use: The ID of the switch.
    ; Output: 
    ;     yConnection: list<symbol>
    ;       Use: The y connections of the switch.
    ; Use: Get the y connections of the switch.
    ;-----------------------------------------------

    (addFunction! railwayManSym 'getYConnection (lambda (switchID)
                                                  (let ([switch (send railwayManager getSwitch switchID)]
                                                        [yConnection 'none])
                                                      (set! yConnection (send switch getYConnection))
                                                      yConnection)))

    ;-----------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       switchID: symbol
    ;         Use: The ID of the switch.
    ; Output: 
    ;     connection: symbol
    ;       Use: The straight connection of the switch.
    ; Use: Get the straight connection of the switch.
    ;------------------------------------------------------

    (addFunction! railwayManSym 'getSwitchConnection (lambda (switchID)
                                                        (let ([switch (send railwayManager getSwitch switchID)]
                                                              [connection 'none])
                                                            (set! connection (send switch getConnection))
                                                            connection)))

    (addFunction! railwayManSym 'startRailway (lambda (railway)
                                              (selectRailway railway)
                                              (send railwayGraph generateGraph!)
                                              (send railwayManager setGraph! (send railwayGraph getGraph))
                                              ))

    ;Railway functions

    ;----------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       status: symbol
    ;         Use: Symbol defining if the simulator or the Z21 library is used
    ; Output: n/a 
    ; Use: Start the system and/or the simulator with the chosen setup
    ;----------------------------------------------------------------------------

    (addFunction! railwaySym 'startSimulator (lambda (railway status)
                                                (send railwaySystem setStatus! status)
                                                (send railwaySystem startSystem railway)))

    ;----------------------------------------
    ; Function: n/a 
    ; Parameters: n/a 
    ; Output: n/a 
    ; Use: Stop the railway simulator (z21).
    ;-----------------------------------------

    (addFunction! railwaySym 'stopSimulator (lambda ()
                                                (send railwaySystem stopSystem)))


    ;-----------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: The ID of the to be created train. 
    ;       dir: symbol
    ;         Use: The previous location to determine the direction.
    ;       loc: symbol
    ;         Use: The current location of the train.
    ;------------------------------------------------------------------

   ; (addFunction! railwaySym 'createTrain (lambda (trainID dir loc)
    ;                                        (add-loco dir loc)))

    ;-----------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;        trainID: symbol
    ;         Use: The ID of the train.
    ;        speed: number
    ;         Use: The speed whereto the speeds need to be set.
    ; Output: n/a 
    ; Use: Change the speed of the train.
    ;------------------------------------------------------------

   ; (addFunction! railwaySym 'setSpeed! (lambda (trainID speed)
   ;                                         (set-loco-speed! trainID speed)))

    ;---------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       trainID: symbol
    ;        Use: The ID of the train.
    ; Output: 
    ;      speed: number
    ;       Use: The current speed of the train.
    ; Use: Get the speed of the train.
    ;----------------------------------------------

 ;   (addFunction! railwaySym 'getSpeed (lambda (trainID)
 ;                                         (get-loco-speed trainID)))

    ;-------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         trainID: symbol
    ;           Use: The ID of the train.
    ; Output: 
    ;     detectionblock: symbol
    ;       Use: The detectionblock where the train is located.
    ; Use: Get the location (detectionblock) of the train.
    ;--------------------------------------------------------------

  ;  (addFunction! railwaySym 'getLocationTrain (lambda (trainID)
  ;                                                (get-loco-detection-block trainID)))

    ;-------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       switchID: symbol
    ;         Use: The ID of the switch.
    ; Output: 
    ;     position: number
    ;       Use: The position of the switch
    ; Use: Get the position of the switch.
    ;--------------------------------------------

  ;  (addFunction! railwaySym 'getSwitchPos (lambda (switchID)
  ;                                            (get-switch-position switchID)))

    ;-----------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       switchID: symbol
    ;         Use: The ID of the switch.
    ;       pos: number
    ;         Use: The new position of the switch.
    ; Output: n/a 
    ; Use: Setting the switch position.
    ;-----------------------------------------------

;    (addFunction! railwaySym 'setSwitchPos (lambda (switchID pos)
 ;                                             (set-switch-position! switchID pos)))
    )

    ;----------------------------------------------------
    ; Function: getFunction
    ; Parameters: 
    ;       hasht: symbol
    ;         Use: The reference of the hashtable.
    ;       key: symbol
    ;         Use: The key used to hash the function.
    ; Output: n/a 
    ; Use: Get a function from the selected hashtable.
    ;----------------------------------------------------

    (define/public (getFunction hasht key)
     (if (and (symbol? hasht)
              (symbol? hasht))
          
          (cond ((eq? hasht railwayManSym) (if (hash-has-key? railwayManFunc key)
                                                (hash-ref railwayManFunc key)
                                                (error "Interface% getFunction: Table does not contain the key, recieved: " hasht key)))
                ((eq? hasht railwaySym) (if (hash-has-key? railwayFunc key)
                                              (hash-ref railwayFunc key)
                                              (error "Interface% getFunction: Table does not contain the key, recieved: " hasht key)))
                (else (error "Interface getFunction: The given table does not exist, recieved: " hasht)))
              
      (error "Interface% getFunction: Contract violation, expected two symbols, recieved: " hasht key)))

    ;-----------------------------------------------------------------------
    ; Function: callFunction
    ; Parameters: 
    ;         lst: list<symbol symbol any>
    ;           Use: The list containing information for a function call.
    ; Output: 
    ;     output: #void or any
    ;       Use: The result from the function call.
    ; Use: Call a function contained by one of the hashtables.
    ;----------------------------------------------------------------------

    (define/public (callFunction lst)
      (if (list? lst)
        (if (not (null? lst))
          (let ([obj (first lst)]
                [func (second lst)]
                [pm (cddr lst)])
                
                (if (and (symbol? obj)
                         (symbol? func))
                  (cond
                      ((empty? pm) ((getFunction obj func)))
                      ((= (length pm) 1) ((getFunction obj func) (first pm)))
                      ((= (length pm) 2) ((getFunction obj func) (first pm) (second pm)))                     
                      (else (error "Interface% callFunction: Parameters list is to long.")))
              
                (error "Interface% callFunction: Contract violation expected symbols as object and function, recieved: " obj func)))
        (error "Interface% callFunction: Contract violation expected a non empty list, recieved: " lst))
      (error "Interface% callFunction: Contract violation expected a list, recieved: " lst)))

    ))