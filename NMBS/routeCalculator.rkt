#lang racket

(require graph)
(require data/heap)
(require typed-stack)
(require dyoo-while-loop)
(require "../TCP/client.rkt")

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

    (field [TCPclient 'uninitialised]
           [deadEnd   'S-16])

    ; Variables to enable type checking

    (define clientType 'object:Client%)

    (define railwayManager 'railwayManager)

    ;------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;      boolean: boolean
    ;       Use: Determine if the object is initialised.
    ; Use: Determine if the object is initialised.
    ;------------------------------------------------------

    (define/public (initialised?)
      (not (eq? TCPclient 'uninitialised)))

    ;-------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;         client: object:Client%
    ;          Use: The used TCP client.
    ; Output: n/a
    ; Use: Initialise the TCP client.
    ;-------------------------------------------------

      (define/public (initialise! client)
        (if (eq? (object-name client) clientType)
            (set! TCPclient client)
            (error "RouteCalculator% initialise!: Contract violation expected a Client% object, recieved: " client)))

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

      (define/public (calculateRoute start end graph)
        (let* ([route (calculate start end graph)]
               [turnLoc (uTurnNeeded? route)]
               [turns (when turnLoc (calculateUturn turnLoc graph))])  ; turns only calculated when there are u turns needed
          
          (display "Calculated route is ")          ;display used to show the calculated path
          (display route)
          (newline)
          (display "Uturns calculated are: ")
          (display turnLoc)
          (newline)
          (display "turns created are: ")
          (display (when (not (void? turns)) (stack->list turns)))
          (newline)
          (display "Turns added: ")
          (display (when (not (void? turns))(addUTurns route turns)))
          (newline)
          
          (if turnLoc              ; if there are u turns needed you add them to the route
            (addUTurns route turns)
            route)                  ; no u turns needed
        ))
    
    ;--------------------------------------------------------------------------
    ; Function: calculate 
    ; Parameters: 
    ;         start: symbol 
    ;           Use: The dectionblock where the route starts. 
    ;         end: symbol
    ;           Use: The dectionblock where the route ends. 
    ;         graph: graph
    ;           Use: The graph representing the railwaysystem. 
    ; Output: 
    ;     route: list<symbol>
    ;       Use: The calculated route. 
    ; Use: Calculating a route from a start to an endpoint useing dijkstra. 
    ;-------------------------------------------------------------------------

    (define/private (calculate start end graph)
      (if (and (symbol? start)
               (symbol? end))
        (if (and (has-vertex? graph start)
                 (has-vertex? graph end))
            (let ([route (list )])
                (let-values ([(costs path) (dijkstra graph end)])  ;hashtable and a list returned als result
                 (set! route  (constructPath (hash->list path) start end))) ;

             route) 
            (error "RouteCalculator% calculate: Given start and end vertex are not a member of the graph, recieved: " start end))
        (error "RouteCalculator% calculate: Contract violation, given start and end are not symbols, recieved: " start end)))

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
          (let ([turnLoc (list )])
            (for ([i route])
              (let ((isSwitch? (send TCPclient TCPcall (list railwayManager 'isSwitch? i))))
                (when isSwitch?
                  (let* ((yConnection (send TCPclient TCPcall (list railwayManager 'getYConnection i)))
                         (idx (index-of route i))
                         (prev (- idx 1))
                         (next (+ idx 1)))   ;issue occurs when prev en next are part of the same y connection

                      (when (and (>= prev 0)
                                 (<= next (- (length route) 1)))
                        (when (and (member (list-ref route prev) yConnection)
                                   (member (list-ref route next) yConnection))

                        (set! turnLoc (append turnLoc (list (cons i (list-ref route next)))))  ;when the next is part of the splitted connection --> add pair to turn location
                      ))))))

            (if (null? turnLoc)
                #f
                turnLoc))
          
        (error "RouteCalculator% uTurnNeeded?: Contract violation expected a non empty list, recieved: " route))
      (error "RouteCalculator% uTurnNeeded?: Contract violation expected a non empty list, recieved: " route)))

    
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


    ;--------------------------------------------------------------------------------
    ; Function: calculateUturn
    ; parameters: 
    ;       turnLoc: list<symbol>
    ;         Use: The locations where a u-turn is needed
    ;       graph: graph
    ;         Use: The graph representing the railwaysystem. 
    ; Output:
    ;      uturn: list<symbol>
    ;        Use: The routes for the u-turns
    ; Use: Calculating routes between two points, that then can be used as uturns. 
    ;---------------------------------------------------------------------------------

    (define/private (calculateUturn turnloc graph)
      (if (list? turnloc)
        (if (graph? graph)
          (if (not (null? turnloc))
          
            (let ([detectionblocks (send TCPclient TCPcall (list railwayManager 'getAllDetectionID))]  ;retrieve all detectionblocks 
                  [results (make-stack)])
              (for ([i turnloc])
                (let ([switchID (car i)]
                      [connID (cdr i)]  
                      [subResults (make-heap <=?)])
                  (for ([d detectionblocks])
                    (let* ([trackID (send TCPclient TCPcall (list railwayManager 'getTrackID d))]
                           [switch-track (calculate switchID trackID graph)]
                           [track-conn (calculate trackID connID graph)])

                      (when (and (not (member connID switch-track))
                                 (not (uTurnNeeded? switch-track))
                                 (not (uTurnNeeded? track-conn)))
                            (if (or (member deadEnd switch-track)
                                    (member deadEnd track-conn))
                                    #f
                            (heap-add! subResults (append switch-track (cdr track-conn)))) ; cdr is needed otherwise you have 2 times the connecting object       
                      )))
                (when (> (heap-count subResults) 0)
                  (push! results (heap-min subResults))
                  (set! subResults (make-heap <=?)))))  ;reset the heap

                results)
                
        
          (error "RouteCalculator% calculateUturn: Contract violation expected a non empty list, recieved: " turnloc))   
        (error "RouteCalculator% calculateUturn: Contract violation expected a graph, recieved: " graph))
      (error "RouteCalculator% calculateUturn: Contract violation expected a non empty list, recieved: " turnloc)))

    ;----------------------------------------------------------------------------------
    ; Function: addUTurns
    ; Parameters: 
    ;       route: list<symbol>
    ;         Use: The calculated route that needs extending.
    ;       turnStack: stack<list<symbol>>
    ;         Use: The points where turns need to be calculated between two points.
    ; Output:
    ;     resultRoute: list<symbol>
    ;       Use: The extended route.
    ; Use: Extending calculated routes with u-turns.
    ;------------------------------------------------------------------------------------

    (define/private (addUTurns route turnStack)
      (if (list? route)
        (if (not (null? route))
          (if (stack? turnStack)

            (let ([result (list )]
                  [stack turnStack])
                (for ([i route])
                  (when (not (stack-empty? stack))
                    (if (eq? i (car (top stack)))
                        (set! result (append result (top stack)))
                        (set! result (append result (list i)))
                    )))  
                result)
          (error "RouteCalculator% addUTurns: Contract violation expected a stack, recieved: " turnStack))
        (error "RouteCalculator% addUTurns: Contract violation expected a nonempty list, recieved: " route))
      (error "RouteCalculator% addUTurns: Contract violation expected a nonempty list, recieved: " route)))


    ))
