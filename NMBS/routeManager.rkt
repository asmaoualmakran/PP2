#lang racket

(require "routeCalculator.rkt")
(require graph)

(provide RouteManager%)

(define RouteManager%
  (class object%
    (super-new)

    (field [railwayManager 'none]
           [routeCalculator 'none]
           [railwayGraph 'none]  ; object containing info
           [graph 'none]
           [trainManager 'none])     
    
    (define routeTable (make-hash))
    (define activeRouteTable (make-hash))
    
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
    

    (define/public (initialise! rmanager tmanager calc railGraph)
      (if (and (eq? (object-name rmanager) railType)
               (eq? (object-name tmanager) trainsType)
               (eq? (object-name calc) calcType)
               (eq? (object-name railGraph) graphType))
          (begin (setRailManager! rmanager)
                 (setTrainManager! tmanager)
                 (setRouteCalculator! calc)
                 (setRailGraph! railGraph)                
                 (setGraph! (send railGraph generateGraph rmanager)))
          (error "RouteManager% initialise!: Contract violation ecpectes RailwayManager%, RouteCalculator% and RailwayGraph% received" rmanager calc railGraph)))

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


    (define/public (setTrainManager! manager)
      (if (eq? (object-name manager) trainsType)
          (set! trainManager manager)
          (error "RouteManager% setTrainManager!: Contract violation expected TrainManager% recieved" manager)))

    (define/public (setRouteCalculator! calculator)
      (if (eq? (object-name calculator) calcType)
          (set! routeCalculator calculator)
          (error "RouteManager% setRouteCalculator!: Contract violation expected RouteCalculator% recieved" calculator)))

    (define/public (setRailGraph! graph)
      (if (eq? (object-name graph) graphType)
          (set! railwayGraph graph)
          (error "RouteManager% setRailGraph!: Contract violation expected RailwayGraph% recieved" graph)))

    (define/public (setGraph! g)
      (if (graph? g)
          (set! graph g)
          (error "RouteManager% setGraph!: Contract violation expected graph recieved" g)))
    ;-------------------------------
    ; Function: saveRoute!
    ; Parameters:
    ;       id: symbol
    ;        Use:
    ;       route: list<symbol>
    ;        Use:
    ; Output: n/a
    ; Use: 
    
    (define/public (saveRoute! id route)
      (if (isUnique? id)
          (hash-set! routeTable id route)
          (error "RouteManager% saveRoute!: id is not unique, received" id)))
          

    (define/public (deleteRoute! id)
      (if (isMember? id)
          (if (isRoute? id)
              (hash-remove! routeTable id)
              (error "RouteManager% deleteRoute!: Cannot remove an active route" id))
          (error "RouteManager% deleteRoute!: Id does not belong to an route" id)))

    (define/public (getRoute id)
      (if (isMember? id)
          (if (isRoute? id)
              (hash-ref routeTable id)
              (hash-ref activeRouteTable id))
          (error "RouteManager% getRoute: Given id does not belong to a route" id)))

           
    (define/public (getAllRouteID)
      (if (hash-empty? routeTable)
          '()
          (hash-keys routeTable)))

    (define/public (isMember? id)
      (or (isRoute? id)
          (isActiveRoute? id)))

    (define/public (isRoute? id)
      (hash-has-key? routeTable id))

    (define/public (isActiveRoute? id)
      (hash-has-key? activeRouteTable id))

    (define/public (activateRoute! id trainID)
      (if (initialised?)
          (if (and (isRoute? id)
                   (send trainManager isTrain? trainID))
              (begin
                (send  (send trainManager getObject trainID) setTraject! (list->vector(getRoute id)))  ;activate the route and train, send the traject to the train
                (hash-set! activeRouteTable id (list id trainID)))
              (error "RouteManager% activateRoute!: Given id does not belong to a route" id))
          (error "RouteManager% activateRoute!: RouteManager% in not initialised, please initialise before use")))

    (define/private (getActiveTrain routeID)
      (if (isActiveRoute? routeID)
          (cdr (hash-ref activeRouteTable routeID))
          (error "RouteManager% getActiveTrain routeID given id does not belong to an active route")))
    

    (define/public (deactivateRoute! id)
      (if (initialised?)
          (if (isActiveRoute? id)
              (begin
                (send (send trainManager getObject(getActiveTrain id)) detelteTraject!)  ;Deactivate the train driving the traject.
          
                (hash-remove! activeRouteTable id))
              (error "RouteManager% deactivateRoute!: Given id does not belong to an active route." id))
          (error "RouteManager% deactivateRoute!: RouteManager% is not initialised please initialise before use.")))
    

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