#lang racket

(require "routeCalculator.rkt")
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

    (field [railwayManager 'none]
           [routeCalculator 'none]
           [railwayGraph 'none]  ; object containing info
           [graph 'none]
           [trainManager 'none])

    ; Hashtables: Containing all the caclulated routes. And keep track of which ones are active
    
    (define routeTable (make-hash))
    (define activeRouteTable (make-hash))

    ; Variables: Used on determing types of certain objects.
    
    (define railType 'object:RailwayManager%)
    (define calcType 'object:RouteCalculator%)
    (define graphType 'object:RailwayGraph%)
    (define trainsType 'object:TrainManager%)

    ;----------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;      Use: Determine if the object is initialised.
    ; Use: Determine if the object is initialised.
    ;----------------------------------------------------
    
    (define/public (initialised?)
      (and(not (eq? railwayManager 'none))
          (not (eq? routeCalculator 'none))
          (not (eq? railwayGraph 'none))
          (not (eq? graph 'none))
          (not (eq? trainManager 'none))))

    ;------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;        rmanager: object:RailwayManager%
    ;           Use: The railway manager used.
    ;        tmanager: object:TrainManager%
    ;           Use: The train manager used. 
    ;        calc: object:RouteCalculator%
    ;           Use: The route calculator used.
    ;        railGraph: object:RailwayGraph%
    ;           Use: The railway graph creator/manipulator used.
    ; Output: n/a
    ; Use: Initialise the routemanager with the needed objects.
    ;-------------------------------------------------------------
    
    ; (define/public (initialise! tmanager calc railGraph)
    ;  (if (and
    ;           (eq? (object-name tmanager) trainsType)
    ;           (eq? (object-name calc) calcType)
    ;           (eq? (object-name railGraph) graphType))
    ;      (begin (setRailManager! rmanager)
    ;             (setTrainManager! tmanager)
    ;             (setRouteCalculator! calc)
    ;             (setRailGraph! railGraph)                
    ;             (setGraph! (send railGraph generateGraph!)))
    ;      (error "RouteManager% initialise!: Contract violation expected RailwayManager%, RouteCalculator% and RailwayGraph% received" rmanager calc railGraph)))


    (define/public (initialise! tmanager routeCalc)
      (if (and  (eq? (object-name tmanager) trainsType)
                (eq? (object-name routeCalc) calcType))
          (if (send tmanager initialised?)
              'test
              (error "RouteManager% initialise!: Given train manager is not initialised, please use an initialised train manager:" tmanager))
          (error "RouteManager% initialise!: Contract violation expected TrainManager% and RailwayGraph% received:" tmanager routeCalc)))

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

    ;----------------------------------------------------------
    ; Function: setRailManager!
    ; Parameters:
    ;       manager: object:RailwayManager%
    ;          Use: The railway manager that needs to be set.
    ; Output: n/a
    ; Use: Set the railway manager.
    ;----------------------------------------------------------

    (define/public (setRailManager! manager)
      (if (eq? (object-name manager) railType)
          (set! railwayManager manager)
          (error "RouteManager% setRailManager!: Contract violation RailwayManager% expected recieved" manager)))

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
          (error "RouteManager% setTrainManager!: Contract violation expected TrainManager% recieved" manager)))

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

    ;-----------------------------------------------------------
    ; Function: setRailGraph!
    ; Parameters:
    ;       graph: object:RailwayGraph%
    ;         Use: The graph manipulator/creator.
    ; Output: n/a
    ; Use: Assign a graph manipulator/creator for the railway.
    ;-----------------------------------------------------------
    
    (define/public (setRailGraph! graph)
      (if (eq? (object-name graph) graphType)
          (set! railwayGraph graph)
          (error "RouteManager% setRailGraph!: Contract violation expected RailwayGraph% recieved" graph)))

    ;-----------------------------------------------------
    ; Function: setGraph!
    ; Parameter:
    ;       g: graph
    ;       Use: The graph that represents the railway.
    ; Output: n/a
    ; Use: Assign a graph that represents the railway.
    ;-----------------------------------------------------

    (define/public (setGraph! g)
      (if (graph? g)
          (set! graph g)
          (error "RouteManager% setGraph!: Contract violation expected graph recieved" g)))
    
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

    (define/public (calculateRoute start end)
      (if (initialised?)
          (if (and (send railwayManager isDetectionblock? start)
                   (send railwayManager isDetectionblock? end))

              (let ([startTrack (send (send railwayManager getDetectionblock start) getTrackID)]  ;The detectionblocks itself are not in the graph, the connecting rails are
                    [destinationTrack (send (send railwayManager getDetectionblock start) getTrackID)]
                    [route '()])
                (set! route (send routeCalculator calculateRoute startTrack destinationTrack railwayGraph))
            
                route) ;return the constructed route
              (error "RouteManager% calculateRoute: Contract violation two detectionblocks are expected, received" start end))
          (error "RouteManager% calculateRoute: Route manager is not initialised, please initialise before use")))
    ))