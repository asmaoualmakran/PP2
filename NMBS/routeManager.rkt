#lang racket

(require "routeCalculator.rkt")
(require "../TCP/client.rkt")
(require graph)

(provide RouteManager%)

;------------------------------------------------------------
; Class: RouteManager%
; Parameters: n/a
; Output: object:RouteManager%
; Use: Managing routes, create them, assing them to trains.
;------------------------------------------------------------

(define RouteManager%
  (class object%
    (super-new)

    (field [TCPclient 'uninitialised]
           [routeCalculator 'uninitialised]
           [graph 'uninitialised]  
           [trainManager 'uninitialised])

    ; Hashtables: Containing all the caclulated routes. And keep track of which ones are active
    
    (define routeTable (make-hash))
    (define activeRouteTable (make-hash))

    ; Variables: Used on determing types of certain objects.
    
    (define calcType 'object:RouteCalculator%)
    (define trainsType 'object:TrainManager%)
    (define clientType 'object:Client%)

    (define railwayManager 'railwayManager)

    ;----------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;      Use: Determine if the object is initialised.
    ; Use: Determine if the object is initialised.
    ;----------------------------------------------------
    
    (define/public (initialised?)
      (and (not (eq? TCPclient 'uninitialised))
           (not (eq? routeCalculator 'uninitialised))
           (not (eq? trainManager 'uninitialised))))

    ;------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;        client: object:Client%
    ;         Use: The TCP client used for the communication.
    ;        tmanager: object:TrainManager%
    ;           Use: The train manager used. 
    ;        calc: object:RouteCalculator%
    ;           Use: The route calculator used.
    ; Output: n/a
    ; Use: Initialise the routemanager with the needed objects.
    ;-------------------------------------------------------------
    
    (define/public (initialise! client tmanager routeCalc)
     (setClient! client)
     (setTrainManager! tmanager)
     (setRouteCalculator! routeCalc))

    ;---------------------------------------------------------
    ; Function: isUnique?
    ; Parameters:
    ;       id: symbol
    ;        Use: The symbol that needs to be checked.
    ; Output:
    ;       boolean: boolean
    ;         Use: Boolean to determine if the id is unique.
    ; Use: Determine if a id is unique.
    ;---------------------------------------------------------

    (define (isUnique? id)
      (if (symbol? id)
          (not (hash-has-key? routeTable id))
          (error "RouteManager% isUnique?: Contract violation symbol expected, received" id)))

    ;------------------------------------------------------
    ; Function: setTrainManager!
    ; Parameters:
    ;      manager: object:TrainManager%
    ;        Use: The train manager that needs to be set.
    ; Output: n/a
    ; Use: Assign a train manager.
    ;------------------------------------------------------- 

    (define/public (setTrainManager! manager)
      (if (eq? (object-name manager) trainsType)
          (set! trainManager manager)
          (error "RouteManager% setTrainManager!: Contract violation expected TrainManager%, recieved: " manager)))

    ;------------------------------------------
    ; Function: getTrainManager
    ; Parameters: n/a 
    ; Output: 
    ;     trainManager: object:TrainManager%
    ;       Use: The used train manager.
    ; Use: Get the used train manager.
    ;-------------------------------------------

    (define/public (getTrainManager)
      (if (initialised?)
          trainManager
          (error "RouteManager% getTrainManager: Object is not initialised, please initialise before use.")))

    ;--------------------------------------------------------------
    ; Function: setCliet! 
    ; Parameters: 
    ;         client: object:Client%
    ;           Use: The tcp client used for the communication.
    ; Output: n/a 
    ; Use: Set the used TCP client.
    ;-------------------------------------------------------------

    (define/public (setClient! client)
      (if (eq? (object-name client) clientType)
          (set! TCPclient client)
          (error "RouteManager% setClient!: Contract violation expected Client%, recieved: " client)))

    ;-----------------------------------
    ; Function: getClient
    ; Parameters: n/a 
    ; Output: 
    ;     TCPclient: object:Client%
    ;       Use: The used TCP client.
    ; Use: Get the used TCP client.
    ;-----------------------------------

    (define/public (getClient)
      (if (initialised?)
          TCPclient
          (error "RouteManager% getClient: Object is not initialised, please initialise before use.")))
    ;--------------------------------------------------------------
    ; Function: setRouteCalculator!
    ; Parameters:
    ;      calculator: object: RouteCalculator%
    ;         Use: The route calculator that needs to be assigned.
    ; Output: n/a
    ; Use: Assign a route calculator.
    ;--------------------------------------------------------------
    
    (define/public (setRouteCalculator! calculator)
      (if (eq? (object-name calculator) calcType)
          (set! routeCalculator calculator)
          (error "RouteManager% setRouteCalculator!: Contract violation expected RouteCalculator% recieved" calculator)))

    ;--------------------------------------------------
    ; Function: getRouteCalculator
    ; Parameters: n/a 
    ; Output: 
    ;     routeCalculator: object:RouteCalculator%
    ;       Use: The used route calculator.
    ; Use: Get the used route calculator
    ;--------------------------------------------------

    (define/public (getRouteCalculator)
      (if (initialised?)
          routeCalculator
          (error "RouteManager% getRouteCalculator: Object is not initialised, please initialise before use.")))
    ;----------------------------------------------------------------------
    ; Function: setGraph!
    ; Parameters:
    ;       lst: list <list>
    ;         Use: The graph creator and setter.
    ; Output: n/a
    ; Use: Assign a graph to the route manager using its list description.
    ;----------------------------------------------------------------------

    (define/public (setGraph! lst)
      (if (list? lst)
        (if (not (null? lst))
          (set! graph (unweighted-graph/directed lst))
        (error "RouteManager% setGraph!: Contract vioaltion, expected a non empty list, recieved: " lst))
      (error "RouteManager% setGraph!: Contract violation, expected a list, recieved: " lst)))

    ;------------------------------------------------------
    ; Function: getGraph
    ; Parameters: n/a 
    ; Output: 
    ;     graph: list<list>
    ;       Use: The edges contained by the graph.
    ; Use: Retrieve the graph used by the route manager.
    ;------------------------------------------------------

    (define/public (getGraph)
      (if (initialised?)
        (if (not (eq? graph 'uninitialised))
        (get-vertices graph)
        (error "RouteManager% getGraph: There is no graph available."))
      (error "RouteManager% getGraph: Object is not initialised, please initailse before use.")))
    
    ;---------------------------------------------------------------------
    ; Function: saveRoute!
    ; Parameters:
    ;       id: symbol
    ;        Use: The identifications that uniquely identifies a route.
    ;       route: list<symbol>
    ;        Use: The route that needs to be saved.
    ; Output: n/a
    ; Use: Save a route in the hashtable useing it's id as a key.
    ;---------------------------------------------------------------------
    
    (define/public (saveRoute! id route)
      (if (isUnique? id)
          (hash-set! routeTable id route)
          (error "RouteManager% saveRoute!: id is not unique, received" id)))

    ;------------------------------------------------------------------------
    ; Function: deleteRoute!
    ; Parameter:
    ;      id: symbol
    ;       Use: The identification of the route that needs to be deleted.
    ; Output: n/a
    ; Use: Delete an non active route from the route hashtable.
    ;------------------------------------------------------------------------
    
    (define/public (deleteRoute! id)
      (if (and(isMember? id)
              (symbol? id))
          (if (and(isRoute? id)
                  (not (isActiveRoute? id)))
              (hash-remove! routeTable id)
              (error "RouteManager% deleteRoute!: Cannot remove an active route" id))
          (error "RouteManager% deleteRoute!: Id does not belong to an route or is not a symbol received" id)))

    ;---------------------------------------------------------------------------
    ; Function: getRoute
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the route that needs to be fetched.
    ; Output:
    ;      route: list<symbol>
    ;        Use: The retrieved route.
    ; Use: Retrieve a route from the hashtable using it's id.
    ;---------------------------------------------------------------------------
    
    (define/public (getRoute id)
      (if (symbol? id)
          (if (isRoute? id)
              (hash-ref routeTable id)
              (error "RouteManager% getRoute: Given id does not belong to a route" id))
          (error "RouteManager% getRoute: Contract violation expected symbol recieved" id)))


    ;--------------------------------------------------------------
    ; Function: getAllRouteID
    ; Parameter: n/a
    ; Output:
    ;     routes: list<symbol>
    ;       Use: The list containing the id's of all the routes.
    ; Use: Retrieving the id's of all the routes.
    ;--------------------------------------------------------------
    
    (define/public (getAllRouteID)
      (if (hash-empty? routeTable)
          '()
          (hash-keys routeTable)))

    ;-----------------------------------------------------------------------------------
    ; Function: isMember?
    ; Parameter:
    ;      id: symbol
    ;       Use: The identification that may belong to a route or a active route.
    ; Output:
    ;      boolean: boolean
    ;        Use: Determine if the identification belongs to a route or active route.
    ; Use: Check if a identification belongs to a route or active route.
    ;-----------------------------------------------------------------------------------

    (define/public (isMember? id)
      (if (symbol? id)
          (or (isRoute? id)
              (isActiveRoute? id))
          (error "RouteManager% isMember?: Contract violation expected symbol recieved" id)))

    ;--------------------------------------------------------------------------
    ; Function: isRoute?
    ; Parameters:
    ;        id: symbol
    ;         Use: The identification of the route that needs to be checked.
    ; Output:
    ;        boolean: boolean
    ;         Use: Determine if the identification belongs to a route.
    ; Use: Check is the identification belongs to a route.
    ;--------------------------------------------------------------------------
    
    (define/public (isRoute? id)
      (hash-has-key? routeTable id))

    ;--------------------------------------------------------------------------
    ; Function: isActiveRoute?
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of a route.
    ; Output:
    ;       boolean: boolean
    ;        Use: Determine if the identification belongs to an active route.
    ; Use: Determine if the identification belong to an active route.
    ;--------------------------------------------------------------------------

    (define/public (isActiveRoute? id)
      (hash-has-key? activeRouteTable id))

    ;---------------------------------------------------------------------------------------------------
    ; Function: activateRoute!
    ; Parameters:
    ;        id: symbol
    ;         Use: The identification of a route that needs to be activated.
    ;        trainID: symbol
    ;         Use: The identification of the train that drives on the route.
    ; Output: n/a
    ; Use: Set a route to active and assigning it to a train, saving the combination in the hashtable.
    ;----------------------------------------------------------------------------------------------------

    (define/public (activateRoute! id trainID)
      (if (initialised?)
          (if (and (isRoute? id)
                   (send trainManager isTrain? trainID))
              (begin
                (send  (send trainManager getObject trainID) setTraject! (list->vector(getRoute id)))  ;activate the route and train, send the traject to the train
                (hash-set! activeRouteTable id (list id trainID)))
              (error "RouteManager% activateRoute!: Given id does not belong to a route" id))
          (error "RouteManager% activateRoute!: RouteManager% in not initialised, please initialise before use")))

    ;-------------------------------------------------------------------
    ; Function: getActiveTrain
    ; Parameters:
    ;         routeID: symbol
    ;           Use: The identification of an active route.
    ; Output:
    ;         trainID: symbol
    ;           Use: The train that drives the active route.
    ; Use: Retrieve the train that drives the specific active route.
    ;-------------------------------------------------------------------
    
    (define/private (getActiveTrainID routeID)
      (if (isActiveRoute? routeID)
          (cdr (hash-ref activeRouteTable routeID))
          (error "RouteManager% getActiveTrainID routeID given id does not belong to an active route")))

    ;--------------------------------------------------------------------------------
    ; Function: deactivateRoute!
    ; Parameters:
    ;          id: symbol
    ;           Use: The identification of the route that needs to be activated.
    ; Ouput: n/a
    ; Use: Deactivate an active route and removing the route from the active train.
    ;--------------------------------------------------------------------------------

    (define/public (deactivateRoute! id)
      (if (initialised?)
          (if (isActiveRoute? id)
              (begin
                (send (send trainManager getObject(getActiveTrainID id)) detelteTraject!)  ;Deactivate the train driving the traject.
          
                (hash-remove! activeRouteTable id))
              (error "RouteManager% deactivateRoute!: Given id does not belong to an active route." id))
          (error "RouteManager% deactivateRoute!: RouteManager% is not initialised please initialise before use.")))

    ;----------------------------------------------------------------------
    ; Function: calculateRoute
    ; Parameters:
    ;        start: symbol
    ;         Use: The detectionblock where the route needs to start.
    ;        end: symbol
    ;         Use: The detectionblock where the route needs to end.
    ; Output:
    ;        route: list<symbol>
    ;         Use: The constructed route between the start and end point.
    ; Use: Calculate a route between two detectionblock.
    ;----------------------------------------------------------------------
   
   (define/public (calculateRoute ID start end)
    (if (initialised?)
      (if (and (symbol? ID)
               (symbol? start)
               (symbol? end))
        (if (and (send TCPclient TCPcall (list railwayManager 'isDetectionblock? start))
                 (send TCPclient TCPcall (list railwayManager 'isDetectionblock? end)))
               
               (let ([startTrack (send TCPclient TCPcall (list railwayManager 'getTrackID start))]
                     [endTrack (send TCPclient TCPcall (list railwayManager 'getTrackID end))]
                     [route (list )])
               (display "fetched")
               (newline)
                (set! route (send routeCalculator calculateRoute startTrack endTrack graph))
                (display "route calculated: ")
                (display route)
                (newline)
                (saveRoute! ID route))
            (error "RouteManager% calculateRoute: Contract violation expected two detectionblock ids, recieved: " start end))
        (error "RouteManager% calculateRoute: Contract violation expected three symbols, recieved: " ID start end))
      (error "RouteManager% calculateRoute: Object is not initialised, please initialise before use.")))
  
    ))