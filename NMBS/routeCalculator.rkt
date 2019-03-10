#lang racket

(require graph)
(require dyoo-while-loop)
(provide RouteCalculator%)

;---------------------------------------------------
; Class: RouteCalculator%
; Use: Class containing route calculation methods.
;---------------------------------------------------

(define RouteCalculator%
  (class object%
    (super-new)

    (define managerType 'object:RailwayManager%)

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
    
    (define/public (calculateRoute start end railGraph)   

          (let ([route '()])
            (let-values ([(costs path) (dijkstra railGraph start)])
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
  
    ))