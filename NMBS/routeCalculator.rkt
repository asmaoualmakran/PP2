#lang racket

(require graph)
(require data/heap)
(require typed-stack)
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
          (if (send trainMan initialised?)

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
    ;      Use: The starting detectionblock location of the path.
    ;    end: symbol
    ;     Use: The ending detectionblock location of the path.
    ;    railGraph: graph
    ;     Use: The graph representing the railwaysystem.
    ; Output:
    ;    list: list<symbol>
    ;      Use: List containing the path.
    ; Use: Calculate a path between two nodes in the graph
    ;--------------------------------------------------------------

    (define/public (calculateRoute start end)
      (if (and (send railManager isDetectionblock? start)
               (send railManager isDetectionblock? end))

          (let ([route (list )]
                [s (send (send railManager getObject start) getTrackID)]
                [e (send (send railManager getObject end) getTrackID)])

            (let-values ([(costs path) (dijkstra graph e)])  ;hashtable and a list returned als result

             (set! route  (constructPath (hash->list path) s e)))
             route)
          (error "RouteCalculator% calculateRoute: Start and ending node are not detectionblocks, route cannot be calculated: " start end)))

    ;------------------------------------------------------------------------------------------------------
    ; Function: constructPath
    ; Parameters: 
    ;        lst: list<cons>
    ;         Use: One point path calculation result.
    ;        start: symbol
    ;         Use: The start node of the path.
    ;        end: symbol
    ;         Use: The end node of the path.
    ; Output: 
    ;       list: list<symbol>
    ;        Use: A constructed path from lst.
    ; Use: Construct a path from start to end using a path that consists from a list containing conscells.
    ;-------------------------------------------------------------------------------------------------------

     (define/private (constructPath lst start end)
     
        (if (list? lst)
          (if (not (null? lst))
            (if (and (symbol? start)
                     (symbol? end))
                (let ([origin car]
                      [destination cdr]
                      [next start]
                      [path (list)])
                      
                      (while next    ;when you reach the origin that is also the end, the destination is then false
                        (for ([i lst])
                          (when (eq? next (origin i))
                            (set! path (append path (list next)))
                            (set! next (destination i)))))
                           path)
              (error "RoutCalculator% constructPath: Contract violation expected two symbols, recieved: " start end))
          (error "RouteCalculator% contructPath: Contract violation expected a non-empty list, recieved: " lst))
        (error "RouteCalculator% constructPath: Contract violation expected a list, recieved: " lst)))

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
                (error "RouteCalculator% getPredec: Given element is not a member of the list")))
          (error "RouteCalculator% getPredec: Contract violaton given list is not a list")))

    ;----------------------------------------------------------------------------------------------
    ; Function: <=?
    ; Parameters:
    ;        x: list<symbol>
    ;         Use: list containing a path
    ;        y: list<symbol>
    ;         Use: list containing a path
    ; Output:
    ;      list: list
    ;       Use: The list that is the shortest of the two
    ; Use: Determine which of the two lists is the shortest, procedure used for the heap in uTurn.
    ;----------------------------------------------------------------------------------------------

    (define (<=? x y)
      (if (and (list? x)
               (list? y))
          (< (length x)(length y))
          (error "RouteCalculator% <=?: Contract violation expected listst recieved:" x y)))


    ;----------------------------------------------------------------------------------------
    ; Function: uTurn
    ; Parameters:
    ;        list: list<pair>
    ;         Use: List containing the locations where u-turns are needed.
    ; Output:
    ;       results: stack<list>
    ;        Use: Stack containing the u-turn routes.
    ; Use: Calculating the u-turns using the location where it's needed and the destination.
    ;----------------------------------------------------------------------------------------

    (define/private (uTurn turnLoc)
      (if (list? turnLoc)
          (if (not (null? turnLoc))
              (if (andmap pair? turnLoc)
                  (let ([detectionBlocks (send railManager getAllDetectionblockID)]
                        [results (make-stack)])
                    (for ([i list])

                      (let ([subResults (make-heap <=?)]
                            [switchID (car i)]
                            [connID (cdr i)])

                        (let ([oppositeID (send(send railManager getSwitch switchID) getOppositeConnection connID)])

                        (for ([block detectionBlocks])
                          (let ([trackID (send (send railManager getDetectionblock block) getTrackID)])
                            (let ([currentPath (calculateRoute switchID trackID)])
                              (when (not (member currentPath oppositeID))
                                (heap-add! subResults (set! currentPath (append currentPath (calculateRoute trackID oppositeID))))
                          ))))
                          (push! results (heap-min results))
                          (set! subResults (make-heap <=?))

                          ))))
                  (error "RouteCalculator% uTurn: Contract violation expected a list of pairs, recieved:" turnLoc))
              (error "RouteCalculator% uTurn: Contract violation expected a non empty list, recieved:" turnLoc))
          (error "RouteCalculator% uTurn: Contract violation expected a list, recieved:" turnLoc)))


    (define/private (addUTurns route turnStack)
      'test)


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
                          (append uTurns (cons i Con2)))))))  ; pairs contain the switchID and where to needs to be turned ID (destination)
                (if (not (null? uTurns))
                    uTurns
                    #f))
              (error "RouteCalculator% uTurnNeeded?: Contract violation expected a non empty list, recieved:" route))
          (error "RouteCalculator% uTurnNeeded?: Contract violation expected a list, recieved:" route)))
    ))
