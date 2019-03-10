#lang racket

(require "routeCalculator.rkt")
(require graph)

(provide RouteManager%)

(define RouteManager%
  (class object%
    (super-new)

    (field [railwayManager 'none]
           [routeCalculator 'none]
           [railGraph 'none]  ; object containing info
           [Graph 'none])     ; the actual graph (using capital letter, racket conflict)
    
    (define routeTable (make-hash))
    (define activeRouteTable (make-hash))
    
    (define railType 'object:RailwayManager%)
    (define calcType 'object:RouteCalculator%)
    (define graph 'object:RailwayGraph%)

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
          (not (eq? railGraph 'none))))
    

    (define/public (initialise!)
      'test)

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
      (if (not(initialised?))
          (if (eq? (object-name manager) railType)
              (set! railwayManager manager)
              (error "RouteManager% setRailManager!: Contract violation RailwayManager% expected recieved" manager))
          (error "RoutManager% setRailManager!: routeManager is already initialised")))


    (define/public (setRouteCalculator! calculator)
      'test)

    (define/public (setRailGraph! graph)
      'test)
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
      (if (isRoute? id)
          (hash-set! activeRouteTable id (list id trainID))
          (error "RouteManager% activateRoute!: Given id does not belong to a route" id)))
    

    (define/public (deactivateRoute! id)
      (if (isActiveRoute? id)
          (hash-remove! activeRouteTable id)
          (error "RouteManager% deactivateRoute!: Given id does not belong to an active route" id)))
    

    (define/public (calculateRoute start end)
      (if (initialised?)
          (if (and (send railwayManager isDetectionblock? start)
                   (send railwayManager isDetectionblock? end))

          (let ([startTrack (send (send railwayManager getDetectionblock start) getTrackID)]  ;The detectionblocks itself are not in the graph, the connecting rails are
                [destinationTrack (send (send railwayManager getDetectionblock start) getTrackID)]
                [route '()])
           (set! route (send routeCalculator calculateRoute startTrack destinationTrack railGraph))
            
            route) ;return the constructed route
          (error "RouteManager% calculateRoute: Contract violation two detectionblocks are expected, received" start end))
          (error "RouteManager% calculateRoute: Route manager is not initialised, please initialise before use")))
    ))