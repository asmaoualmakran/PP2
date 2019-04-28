#lang racket

(require graph)
(require dyoo-while-loop)
(provide RouteCalculator%)

;---------------------------------------------------
; Class: RouteCalculator%
; Parameters: n/a
; Output: object:RouteCalculator%
; Use: Class containing route calculation methods.
;---------------------------------------------------

(define RouteCalculator%
  (class object%
    (super-new)

    (field [trainManager 'uninitialised]
           [railManager  'uninitialised]
           [graph        'uninitialised])

    ; Variables to enable type checking
    
    (define railManagerType 'object:RailwayManager%)
    (define trainManagerType 'object:TrainManager%)

    ;------------------------------------------------------
    ; Function: initialised? 
    ; Parameters: n/a 
    ; Output:
    ;      boolean: boolean
    ;       Use: Determine if the object is initialised.
    ; Use: Determine if the object is initialised.
    ;------------------------------------------------------

    (define/public (initialised?)
      (and (not (eq? trainManager 'uninitialised))
           (not (eq? railManager 'uninitialised))
           (not (eq? graph 'uninitialised))))

    ;-------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;         trainMan: object:TrainManager%
    ;          Use: The train manager that is used.
    ; Output: n/a
    ; Use: Initialise the routeCalculator.
    ;-------------------------------------------------
    
    (define/public (initialise! trainMan)
      (if (eq? (object-name trainMan) trainManagerType)
          (if (send trainMan initialsed?)

              (begin
                (set! trainManager trainMan)
                (if (send (send trainManager getRailwayManager) initialised?)

                    (begin
                      (set! railManager (send trainManager getRailwayManager))
                      (set! graph (send railManager getGraph)))
   
                    (error "RouteCalculator% initialise!: Recieved railwayManager is not initialised, please use an initialised RailwayManager%, recieved"(send trainManager getRailwayManager))))
              (error "RouteCalculator% initialise!: Given TrainManager% is not initialised, please use an initialised TrainManager%, recieved:" trainMan))
          (error "RouteCalculator% initialise!: Contract violation expected a TrainManager%, recieved:" trainMan)))

    ;-------------------------------------------------------------
    ; Function: calculateRoute
    ; Parameters:
    ;    start: symbol
    ;      Use: The starting location of the path.
    ;    end: symbol
    ;     Use: The ending location of the path.
    ;    railGraph: graph
    ;     Use: The graph representing the railwaysystem.    
    ; Output:
    ;    list: list<symbol>
    ;      Use: List containing the path.
    ; Use: Calculate a path between two nodes in the graph
    ;--------------------------------------------------------------
    
    (define/public (calculateRoute start end)   

      (let ([route '()])
        (let-values ([(costs path) (dijkstra graph start)])
          (set! route (constructPath path start end)))
        route)
      (error "RailwayGraph% calculateRoute: Contract violation given parameters are not detecionblock,graph or manager"))

    ;--------------------------------------------------------------------
    ; Function: constructPath
    ; Parameters:
    ;      list: list<pair>
    ;        Use: List containing all the connecting nodes
    ;      start: symbol
    ;        Use: The starting node of the path
    ;      end: symbol
    ;        Use: The ending node of the path
    ; Output:
    ;      path: list<symbol>
    ;        Use: List containing the path between start and end node.
    ; Use: Construct a path between the start and end node.
    ;--------------------------------------------------------------------

    (define/public (constructPath list start end)
      (if (list? list)
          (let ([path '()])
            (for ([i list])
              (if (eq? (car i) end)
                  (set! path (add-between path #:before-first (car i) ))
                  '()))
            (if (not (eq? path '()))
                (while (not (eq? (car path) start))
                       (set! path (add-between path #:before-first (getPredec list (car path)))))
                (error "RailwayGraph% constructPath: Path is not initialised, rest can not be constructed"))
            path) ;returning the path
          (error "RailwayGraph% constructPath: Contract violation given list is not a list")))

    ;--------------------------------------------------------------
    ; Function: getPredec
    ; Parameters:
    ;    list: list<pair>
    ;      Use: The list where the elements need to be found.
    ;    elm: symbol
    ;     Use: The node who's predecesor needs to be found.
    ; Output:
    ;     predecessor: symbol
    ;       Use: The predecessor that needed to be found.
    ; Use: Find the node and return it's predecessor.
    ;--------------------------------------------------------------

    (define/private (getPredec list elm)
      (if (list? list)
          (for ([i list])
            (if (eq? elm (car i))
                (cdr i)
                (error "RailwayGraph% getPredec: Given element is not a member of the list")))
          (error "RailwayGraph% getPredec: Contract violaton given list is not a list")))

    ;----------------------------------------------------------------------------------------
    ; Function: uTurn
    ; Parameters:
    ;        list: list<pair>
    ;         Use: List containing the locations where u-turns are needed.
    ; Output:
    ;       list: list<list>
    ;        Use: List containing the u-turn routes.
    ; Use: Calculating the u-turns using the location where it's needed and the destination.
    ;-----------------------------------------------------------------------------------------

    (define/private (uTurn list)
      (if (list? list)
          (if (not (null? list))
              (let ([detectionBlocks (send railManager getAllDetectionblockID)])
                (for ([i list]
                      [d detectionBlocks])
                  (let ([switchID (car i)]      ; in the uTurn list, each element is a tuple containing the switch and the connection that needs to be followed.
                        [connID   (cdr i)])
                        (let([oppositeID (send (send railManager getSwitch switchID) getOppositeConnection)])
                        'body))))
              (error "RouteCalculator% uTurn: Contract violation expected an non empty list, recieved:" list))
          (error "ROuteCalculator% uTUrn: Contract violation expected a list, recieved:" list)))

    ;----------------------------------------------------------------------------------------
    ; Function: uTurnNeeded?
    ; Parameters:
    ;         route: symbol
    ;          Use: The route that is calculated in the first run.
    ; Output:
    ;     result: list<pair> of boolean
    ;       Use: The locations where u-turns are needed if no u turns needed #f is returned.
    ; Use: Determine the locations where u turns are needed and returning them.
    ;----------------------------------------------------------------------------------------
    
    (define/private (uTurnNeeded? route)
      (if (list? route)
          (if (not (null? route))
              (let ([uTurns '()])
                (for ([i route]
                      [idx (in-range (length route))])
                  (let ([object (send railManager i)])
                    (when (send railManager isSwitch? object)  ; when you have a switch you need to check if the path goes for y connection to y connection.
                      (let ([Con1 (list-ref route (- idx 1))] ; Switch is sandwitched between 2 connections.
                            [Con2 (list-ref route (+ idx 1))])
                        (when (and (member Con1 (send object getYConnection))
                                   (member Con2 (send object getYConnection)))
                          (append uTurns (cons i Con2)))))))
                (if (not (null? uTurns))
                    uTurns
                    #f))
              (error "RouteCalculator% uTurnNeeded?: Contract violation expected a non empty list, recieved:" route))
          (error "RouteCalculator% uTurnNeeded?: Contract violation expected a list, recieved:" route)))
    ))