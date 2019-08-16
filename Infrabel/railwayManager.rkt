#lang racket

(require graph)
(require "track.rkt")
(require "switch.rkt")
(require "detectionBlock.rkt")



(provide RailwayManager%)

(define RailwayManager%
  (class object%
    (super-new)
    
    ; The hashtables where the objects are saved.
    ; The keys are the id's and values, the objects.
    (define trackTable (make-hash))
    (define switchTable (make-hash))
    (define detectionblockTable (make-hash))
    (define activeRoutesTable (make-hash))


    ; Definitions of the object types, enables easyer type checking
    (define trackType 'object:Track%)
    (define switchType 'object:Switch%)
    (define detectionblockType 'object:Detectionblock%)
    (define serverType  'object:Server%)

    (field [railGraph    'uninitialised]
           [TCPserver    'uninitialised]
           [security     'uninitialised])


    ;-----------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;        boolean: boolean
    ;        Use: Determine if the object is initialised.
    ; Use: Determine if the object is initialised.
    ;------------------------------------------------------
    
    (define/public (initialised?)
      (and (not (eq? railGraph 'uninitialised))
           (not (eq? TCPserver 'uninitiailsed))))

    ;----------------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;      graph: graph
    ;        Use: The graph that contains a representation of the railway.
    ;     server: TCP server
    ;       Use: The server on which communication enters
    ;     secure: object:SecurityProtocol%
    ;       Use: The security protocol used to secure the railway. 
    ; Output: n/a
    ; Use: Initialise the object.
    ;----------------------------------------------------------------------
    
    (define/public (initialise! graph server)
      (if (graph? graph)
              (begin
                (setGraph! graph)
                (setServer! server))
    
          (error "RailwayManager% initialise!: Contract violation expected graph, recieved:" graph)))

    ;-----------------------------------------------------------------------
    ; Function: setServer!
    ; Parameters: 
    ;       server: object:Server%
    ;         Use: The TCP server that is used.
    ; Output: n/a 
    ; Use: Set the TCP server to enable communication outside the module.
    ;-----------------------------------------------------------------------

    (define/public (setServer! server)
      (if (eq? (object-name server) serverType)
          (set! TCPserver server)
          (error "RailwayManager% setServer!: Contract violation expected a server, recieved: " server)))

    ;----------------------------------------------------------
    ; Function: getServer
    ; Parameters: n/a 
    ; Output: 
    ;     TCPserver: object:Server%
    ;       Use: The server used for outgoing communication.
    ; Use: Get the server for outgoing communication.
    ;----------------------------------------------------------

    (define/public (getServer)
      (if (initialised?)
        TCPserver
        (error "RailwayManager% getServer: Object is not initialised.")))

    ;-------------------------------------------
    ; Function: setSecurity!
    ; Parameters: 
    ;     secure: object:SecurityProtocol%
    ;       Use: The security protocol used. 
    ; Output: n/a 
    ; Use: Initialise the security field. 
    ;-------------------------------------------

    (define/public (setSecurity! secure)
        (set! security secure))

    ;----------------------------------
    ; Function: startSecurity
    ; Parameters: n/a 
    ; Output: n/a 
    ; Use: Startup the security loop. 
    ;----------------------------------

    (define/public (startSecurity)
      (send security protocol))

    ;---------------------------------------------------------
    ; Function: activateRoute! 
    ; Parameters: 
    ;         data: list<symbol> 
    ;           Use: The data needed to activate a route
    ; Output: n/a 
    ; Use: Activate a train and a route
    ;---------------------------------------------------------

    (define/public (activateRoute! data)
      
      (let ((train (car data))
            (route (cadr data)))
        (if (not (hash-has-key? activeRoutesTable train))
            (hash-set! activeRoutesTable train route)
          (error "RailwayManager% activateRoute!: Given train is already active."))))
    
    ;------------------------------------------------------------------
    ; Function: deactivateRoute! 
    ; Parameters: 
    ;       train: symbol
    ;         Use: The id of the train that needs to be deactivated. 
    ; Output: n/a 
    ; Use: Deactivate a route. 
    ;------------------------------------------------------------------

    (define/public (deactivateRoute! train)
      (if (hash-has-key? activeRoutesTable train)
        (hash-remove! activeRoutesTable train)
        (error "RailwayManager% deactivateRoute!: Given train is not active.")))

    ;--------------------------------------------------------------
    ; Function: getActiveTrains
    ; Parameters: n/a 
    ; Output: 
    ;     activeTrains: list<symbol> 
    ;       Use: The trains that are active at the given moment. 
    ; Use: Get all the active trains of that moment. 
    ;---------------------------------------------------------------

    (define/public (getActiveTrains)
      (hash-keys activeRoutesTable))

    ;--------------------------------------------------------------------
    ; Function: getActiveRoute
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: An active trains who's route needs to be fetched. 
    ; Output: 
    ;       route: list<symbol> 
    ;         Use: The route of the active train. 
    ; Use: Get the route of an active train. 
    ;--------------------------------------------------------------------

    (define/public (getActiveRoute trainID)
      (if (hash-has-key? activeRoutesTable trainID)
          (hash-ref activeRoutesTable trainID)
          (error "RailwayManager% getActiveRoute: Given trainID does not belong to an acitve train, recieved " trainID)))

    ;--------------------------------------------------------------
    ; Function: setGraph!
    ; Parameters:
    ;          graph: graph
    ;          Use: The graph that represents the railway system.
    ; Output: n/a
    ; Use: Set the used graph.
    ;--------------------------------------------------------------

    (define/public (setGraph! graph)
      (if (graph? graph)
          (set! railGraph graph)
          (error "RailwayManager% setGraph!: Contract violation expected graph received" graph)))

    ;----------------------------------------------------------
    ; Function: getGraph
    ; Parameters: n/a
    ; Output:
    ;        graph: graph
    ;        Use: The graph that represents the railwaysystem
    ; Use: Retrieve the used graph
    ;----------------------------------------------------------
    
    (define/public (getGraph)
      (if (initialised?)
          railGraph
          (error "RailwayManager% getGraph: railway manager is not initialised, please initialise before use")))
      
    ;----------------------------------------------------------------------
    ; Function: isUnique?
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification that needs to be checked.
    ; Output:
    ;    boolean: boolean
    ;     Use: Boolean to determine whether or not a symbol is unique.
    ; Use: Determine whether or not a given symbol is unique.
    ;----------------------------------------------------------------------

    (define (isUnique? id)
      (if (symbol? id)
          (and (not (hash-has-key? trackTable id))
               (not (hash-has-key? switchTable id))
               (not (hash-has-key? detectionblockTable id)))
          (error "RailwayManager% isUnique?: Contract violation symbol expected received" id)))

    ;------------------------------------------------------------------------------
    ; Function: getObject
    ; Parameter:
    ;       id:symbol
    ;        Use: The identification of the object that needs to be fetched.
    ; Output:
    ;    railObject: object:Track% or object:Switch% or object:Detectionblock%
    ;       Use: The fetched railway object.
    ; Use: Get a railwayobject useing it's ID, no need to know the type up front.
    ;-------------------------------------------------------------------------------

    (define/public (getObject id)
      (if (symbol? id)
          (cond ((isSwitch? id) (getSwitch id))
                ((isTrack? id) (getTrack id))
                ((isDetectionblock? id) (getDetectionblock id))
                (else (error "RailwayManager% getObject: Given ID does not belong to a railobject" id)))
          (error "RailwayManager% getObject: Contract violation symbol expected received" id)))

    ;------------------------------------------------------------------------------------------
    ; Function: getRelatedObject
    ; Parameter:
    ;       id:symbol
    ;        Use: The identification of the object who's related object needs to be fetched.
    ; Output:
    ;       railObject: object:Track% object:Detectionblock%
    ;         Use: The related object that needs to be fetched.
    ; Use: Retrieve the related object of a railway object.
    ;------------------------------------------------------------------------------------------

    (define/public (getRelatedObject id)
      (if (isMember? id)
          (if (isTrack? id)
            (when (hasRelatedObject? id)
              (getDetectionblock(send (getTrack id) getDetectionblockID)))
              (if (isDetectionblock? id)
                (when (hasRelatedObject? id)
                  (getTrack(send (getDetectionblock id) getTrackID)))
                  (error "RailwayManager% getRelatedObject?: object has no related object" id)))  
          (error "RailwayManager% getRelatedObject: Given id does not belong to railway object" id)))

    ;----------------------------------------------------------------------------
    ; Function: getRelatedID
    ; Parameters: 
    ;       id: symbol
    ;        Use: The object who's related object's id needs to be fetched. 
    ; Output: 
    ;       id: symbol
    ;         Use: The relateds object's id. 
    ; Use: Fetch the id of a related object. 
    ;----------------------------------------------------------------------------

    (define/public (getRelatedID id)
      (if (isMember? id)
        (cond ((isDetectionblock? id)(send (getObject id) getTrackID))
              ((isTrack? id)(send (getObject id) getDetectionblockID)))

      (error "RailwayManager getRelatedID: Given ID does not belong to a member of the railway.")))

    ;------------------------------------------------------------------------------------
    ; Function: hasRelatedObject?
    ; Parameters:
    ;      id:symbol
    ;       Use: The id of the object of which the related object needs to be checked.
    ; Output:
    ;     boolean: boolean
    ;       Use: Determing if the object has a related object.
    ; Use: Determine if the object has a related object.
    ;------------------------------------------------------------------------------------
    
    (define/public (hasRelatedObject? id)
      (if (isMember? id)
          (if (isTrack? id)
              (send (getTrack id)  hasDetectionblock? )

              (if (isDetectionblock? id)
                  (send (getDetectionblock id) hasTrack?)

                  (error "RailwayManager% hasRelatedObject?: Object has no related object" id)))
          (error "RailwayManager% hasRelatedObject?: Object id does not belong to a railway object" id)))
 
    ;--------------------------------------------------------------
    ; Function: setStatus!
    ; Parameters: 
    ;         id: symbol
    ;          Use: The object which status' needs to be changed. 
    ;         status: symbol || #t     
    ;           Use: The occupation level of the object. 
    ; Output: n/a 
    ; Use: Change the occupation status of the object. 
    ;---------------------------------------------------------------

    (define/public (setStatus! id status)
    (if (or (symbol? status)
            (eq? status #t))
      (send (getObject id) setAvailable! id)
      (error "RailwayManager% setStat!: Expected a symbol or #t, recieved: " status)))

    ;----------------------------------------------------
    ; Function: getStatus 
    ; Parameters: 
    ;         id: symbol
    ;          Use: The object who's status is needed. 
    ; Output: 
    ;       status: symbol || boolean
    ;         Use: The status of the railway object. 
    ; Use: Get the status of the railway object. 
    ;----------------------------------------------------

    (define/public (getStatus id)
      (if (symbol? id)
          (send (getObject id) getAvailable)
      (error "RailwayManager getStatus: Expected a symbol, recieved: " id)))

    ;------------------------------------------------------------------
    ; Function: isTrack?
    ; Parameters:
    ;     id: symbol
    ;      Use: The id of the object that needs to be checked.
    ; Output:
    ;    boolean: boolean
    ;      Use: A boolean that determines if the object is a track.
    ; Use: Determine if an id belongs to a track object.
    ;------------------------------------------------------------------
    
    (define/public (isTrack? id)
      (if (symbol? id)
          (hash-has-key? trackTable id)
          (error "RailwayManager% isTrack?: contract violation, expected symbol received" id)))

    ;-------------------------------------------------------------------------------
    ; Function: createTrack!
    ; Parameters:
    ;      id: symbol
    ;       Use: The id to identify the object uniquely.
    ; Ouput: n/a
    ; Use: Create a Switch% object with the given ID and store it in the hashtable.
    ;-------------------------------------------------------------------------------

    (define/public (createTrack! id)
      (if (isUnique? id)
          (let ([track (make-object Track%)])
            (send track setID! id)
            (send track initialise!)
            (hash-set! trackTable id track))
          (error "RailwayManager% createRail: id is not unique, received" id)))

    ;--------------------------------------------------------------------------
    ; Function: getTrack
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the object that needs to be retrieved.
    ; Ouput:
    ;    track: object: Track%
    ;      Use: The track object that needed to be retrieved.
    ; Use: Retrieve a track object with the corresponding id.
    ;--------------------------------------------------------------------------
    
    (define/public (getTrack id)
      (if (isTrack? id)
          (hash-ref trackTable id)
          (error "RailwayManager% getTrack: given id is not a track member received" id)))

    ;--------------------------------------------------------
    ; Function: getAllTrackID
    ; Parameters: n/a
    ; Output:
    ;      list: list<symbol>
    ;        Use: The identifications of all the tracks.
    ; Use: Retrieve the identifications of all the tracks.
    ;--------------------------------------------------------
    
    (define/public (getAllTrackID)
      (if (hash-empty? trackTable)
          '()
          (hash-keys trackTable)))
         
    ;-----------------------------------------------------------------
    ; Function: deleteTrack!
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the to be deleted track object.
    ; Output: n/a
    ; Use: Delete a track object from the hashtable.
    ;-----------------------------------------------------------------

    (define/public (deleteTrack! id)
      (if (isTrack? id)
          (hash-remove! trackTable id)
          (error "RailwayManager% deleteTrack!: given id is not a track member received" id)))

    ;----------------------------------------------------------------------------------------
    ; Function: isSwitch?
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification of the object that needs to be checked.
    ; Output:
    ;    boolean: boolean
    ;     Use: A boolean that determines whether or not an object is in the switch table.
    ; Use: Check whether or not an objet is in the switch table.
    ;----------------------------------------------------------------------------------------

    (define/public (isSwitch? id)
      (if (symbol? id)
          (hash-has-key? switchTable id)
          (error "RailwayManager% isSwitch?: contract violation expected symbol recieved"id)))

    ;--------------------------------------------------------------------------------
    ; Function: createSwitch!
    ; Parameters:
    ;     id: symbol
    ;      Use: The id to uniquely identify the switch object.
    ; Output: n/a
    ; Use: Create a Switch% object with the given ID and store it in the hashtable.
    ;--------------------------------------------------------------------------------
    
    (define/public (createSwitch! id)
      (if (isUnique? id)
          (let ([switch (make-object Switch%)])
            (send switch setID! id)
            (send switch initialise!)
            (hash-set! switchTable id switch))
          (error "RailwayManager% createSwitch: id is not unique, received" id)))

    ;----------------------------------------------------------------------------
    ; Function: getSwitch
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the object that needs to be retrieved.
    ; Output:
    ;    switch: object:Switch%
    ;      Use: The switch object that needs to be retrieved.
    ; Use: Retrieve a Switch% object from the hashtable using it's id.
    ;----------------------------------------------------------------------------
    
    (define/public (getSwitch id)
      (if (isSwitch? id)
          (hash-ref switchTable id)
          (error "RailwayManager% getSwitch: id is not a member of the switchTable, recieved"id)))
    
    ;--------------------------------------------------------
    ; Function: getAllSwitchID
    ; Parameters: n/a
    ; Output:
    ;      list: list<symbol>
    ;        Use: The identifications of all the switches.
    ; Use: Retrieve the identifications of all the switches.
    ;--------------------------------------------------------
    
    (define/public (getAllSwitchID)
      (if (hash-empty? switchTable)
          '()
          (hash-keys switchTable)))
    
    ;-----------------------------------------------------------
    ; Function: deleteSwitch!
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the to be deleted switch.
    ; Output: n/a
    ; Use: Delete a switch useing it's id.
    ;-----------------------------------------------------------

    (define/public (deleteSwitch! id)
      (if (isSwitch? id)
          (hash-remove! switchTable id)
          (error "RailwayManager% deleteSwitch!: id is not a member of the switchtable, recieved" id)))


    ;--------------------------------------------------------------------------------
    ; Function: createDetectionblock!
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the new created detectionblock object.
    ; Output: n/a
    ; Use: Create a Detectionblock% object and store it in the detectionblockTable.
    ;--------------------------------------------------------------------------------

    (define/public (createDetectionblock! id)
      (if (isUnique? id)
          (let ([block (make-object Detectionblock%)])
            (send block setID! id)
            (send block initialise!)
            (hash-set! detectionblockTable id block))
          (error "RailwayManager% createDetectionBlock: id is not unique, received" id)))

    ;----------------------------------------------------------------------
    ; Function: isDetectionblock?
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the object that needs checking.
    ; Output:
    ;     boolean: boolean
    ;       Use: Determine whether or not the object is a detectionblock
    ;----------------------------------------------------------------------

    (define/public (isDetectionblock? id)
      (if (symbol? id)
          (hash-has-key? detectionblockTable id)
          (error "RailwayManager% isDetectionblock?: contract violation, expected symbol received" id)))

    ;---------------------------------------------------------------------
    ; Function: getDetectionblock
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the to be retrieved detectionblock.
    ; Output:
    ;    detectionblock: object:Detectionblock%
    ;       Use: The retrieved detectonblock.
    ; Use: Retrieve a detectionblock useing it's id.
    ;---------------------------------------------------------------------

    (define/public (getDetectionblock id)
      (if (isDetectionblock? id)
          (hash-ref detectionblockTable id)
          (error "RailwayManager% getDetectionblock: given id is not from a detectionblock recieved" id)))

    
    ;---------------------------------------------------------------
    ; Function: getAllDetectionblockID
    ; Parameters: n/a
    ; Output:
    ;      list: list<symbol>
    ;        Use: The identifications of all the detectionblocks.
    ; Use: Retrieve the identifications of all the detectionblocks.
    ;---------------------------------------------------------------
    
    (define/public (getAllDetectionblockID)
      (if (hash-empty? detectionblockTable)
          '()
          (hash-keys detectionblockTable)))
    
    ;-----------------------------------------------------------------------
    ; Function: deleteDetectionblock!
    ; Parameters:
    ;       id: symbol
    ;         Use: The id of the detectionblock that needs to be deleted.
    ; Output: n/a
    ; Use: Delete the detectionblock object from the hashtable.
    ;-----------------------------------------------------------------------

    (define/public (deleteDetectionblock! id)
      (if (isDetectionblock? id)
          (hash-remove! detectionblockTable id)
          (error "RailwayManager% deleteDetectionbolck!: id is not a member of the detectionblockTable recieved" id)))

    ;-----------------------------------------------------------------------
    ; Function: findObject
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the to be find object.
    ; Output:
    ;     object: object:Track% ; object:Detectionblock%; object:Switch%
    ;       Use: The to be found object.
    ; Use: Retrieve an object useing it's ID.
    ;-----------------------------------------------------------------------
    
    (define/public (findObject id)
      (cond ((isTrack? id)(getTrack id))
            ((isSwitch? id)(getSwitch id))
            ((isDetectionblock? id)(getDetectionblock id))
            (else
             (error "RailwayManager% findObject: id is not a member of one of the tables recieved" id))))

    ;---------------------------------------------------
    ; Function: clearAllTables!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Delete all the elements from the hashtables.
    ;---------------------------------------------------

    (define/public (clearAllTables!)
      (hash-clear! trackTable)
      (hash-clear! switchTable)
      (hash-clear! detectionblockTable))

    ;----------------------------------------------------------------
    ; Function: containsRailway?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;      Use: Determine if the railway manager contains a railway.
    ; Use: Check if it contains a railway or not.
    ;----------------------------------------------------------------

    (define/public (containsRailway?)
      (or (not (hash-empty? trackTable))
          (not (hash-empty? switchTable))
          (not (hash-empty? detectionblockTable))))

    ;----------------------------------------------------------------------------
    ; Function: isMember?
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the object that needs to be checked.
    ; Output:
    ;     boolean: boolean
    ;        Use: Determine if an object is part of the railway.
    ; Use: Check if an object is part of the railway.
    ;----------------------------------------------------------------------------
    
    (define/public (isMember? id)
      (or (isTrack? id)
          (isSwitch? id)
          (isDetectionblock? id)))

    ;---------------------------------------------------------------------------------
    ; Function: isConnected?
    ; Parameters:
    ;       id1: symbol
    ;        Use: The identification of a railway object.
    ;       id2: symbol
    ;        Use: The identification of a railway object.
    ; Output:
    ;       boolean: boolean
    ;        Use: Boolean to determine if two objects are connected.
    ; Use: Check if the object's who's id's are given are connected to each other.
    ;---------------------------------------------------------------------------------

    (define/public (isConnected? id1 id2)
      (if (and (symbol? id1)
               (symbol? id2))
          (if (and (isMember? id1)
                   (isMember? id2))
              (if (isDetectionblock? id1)
                  (begin (and (eq? (send (getDetectionblock id1) getTrackID) id2)
                              (eq? (send (getTrack id2) getDetectionblockID) id1)))
                  (if (isDetectionblock? id2)
                      (begin (and (eq? (send (getDetectionblock id2) getTrackID) id1)
                                  (eq? (send (getTrack id1) getDetectionblockID) id2)))
                      (begin (and (send (findObject id1) isConnected? id2)
                                  (send (findObject id2) isConnected? id1)))))
              #f)
          (error "RailwayManager% isConnected?: contract violation expected id's recieved" id1 id2)))

    ;-----------------------------------------------------------------------------------------------
    ; Function: connect!
    ; Parameters:
    ;       id1: symbol
    ;        Use: The identification of a railway object.
    ;       id2: symbol
    ;        Use: The identification of a railway object.
    ; Output: n/a
    ; Use: Connect two railway objects with each other and making sure the connections are valid.
    ;-----------------------------------------------------------------------------------------------

    (define/public (connect! id1 id2)
      (if (and (symbol? id1)
               (symbol? id2))
          (if (not (or (and (isDetectionblock? id1)  ;check if the not allowed construct is not the case
                            (isSwitch? id2))
                       (and (isSwitch? id1)
                            (isDetectionblock? id2))))
              
              (let ([obj1 (findObject id1)]
                    [obj2 (findObject id2)])
                (if (and (send obj1 initialised?)
                         (send obj2 initialised?))
                    (if (isDetectionblock? id1)
                        
                        (if (not(send obj1 isPlaced?))  ;The block is not placed on the track
                            (begin(send obj1 setTrackID! id2)
                                  (send obj2 sentDetectionblockID! id1))
                            (error "RailwayManager% connect!: cannot place an already placed detectionblock"))
                        
                        (if (isDetectionblock? id2)
                            (begin(send obj2 setTrackID! id1)
                                  (send obj1 setDetectionblockID! id2))
                            (if (and (send obj1 connectionAvailable?)
                                     (send obj2 connectionAvailable?))
                                (begin (send obj1 addConnectionID! id2)
                                       (send obj2 addConnectionID! id1))
                                
                                (error "RailwayManager% connect!: there is no connection available"))))
                    (error "RailwayManager% connect!: objects are not initialised please initialise before use")))
              (error "RailwayManager% connect!: cannot connects a switch and a detectionblock"))        
          (error "RailwayManager% connect!: contract violation, expected symbols recieved" id1 id2)))

    ;------------------------------------------------------------
    ; Function: disconnect!
    ; Parameters:
    ;       id1: symbol
    ;        Use: The identification of a railway object.
    ;       id2: symbol
    ;        Use: The identification of a railway object.
    ; Output: n/a
    ; Use: Disconnect two railway objects who are connected.
    ;------------------------------------------------------------
    
    (define/public (disconnect! id1 id2)
      (if (and (symbol? id1)
               (symbol? id2))
          (if (and (or (isDetectionblock? id1)  ; check if the id's belong to a detectionblock switch of track
                       (isSwitch? id1)
                       (isTrack? id1))
                   (or (isDetectionblock? id2)
                       (isSwitch? id2)
                       (isTrack? id2)))
              
              (let ([obj1 (findObject id1)]  ;If it is one of the types, retrieve the object
                    [obj2 (findObject id2)])
                (cond ((eq? (object-name obj1) detectionblockType)  ; One of the objects is a detection block, you know the other one is a track object
                       (if (isConnected? id1 id2)    ; check if the track and the detectionblock are connected in both ways                                      
                           (begin (send obj1 deleteTrackID!)
                                  (send obj2 deleteDetectionblock!))
                           (error "RailwayManager% disconnect: given objects are not connected" id1 id2)))
                      
                      ((eq? (object-name obj2) detectionblockType)
                       (if (isConnected? id1 id2)
                           (begin (send obj2 deleteTrackID!)
                                  (send obj1 deleteDetectionblock!))
                           (error "RailwayManager% disconnect: given objects are not connected" id1 id2)))
                                                                        
                      (else (if (isConnected? id1 id2) ; in this case, both objects are a combination of switches and tracks.     
                                (begin(send obj1 deleteConnection! id2)  ; check if the objects are connected to each other
                                      (send obj2 deleteConnection! id1))
                                (error "RailwayManager% disconnect: given objects are not connected" id1 id2)))))
                         
              (error "RailwayManager% disconnect!: given objects are not a part of the railway" id1 id2))
          (error "RailwayManager% disconnect!: contract violation, expected symbols recieved" id1 id2)))
      
    ))
