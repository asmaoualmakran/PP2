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


    (define/private (uTurn)
      'test)

    (define/private (uTurnNeeded? route)
      (if (list? route)
          (if (not (null? route))
              (for ([i route])
                'test)
              (error "RouteCalculator% uTurnNeeded?: Contract violation expected a non empty list, recieved:" route))
          (error "RouteCalculator% uTurnNeeded?: Contract violation expected a list, recieved:" route)))
  
    ))