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

    (field [TCPclient 'uninitialised])

    ; Variables to enable type checking

    (define clientType 'object:Client%)

    (define railwayManager 
      (when (eq? (object-name TCPclient) clientType)
        (when (send TCPclient initialised?)
         (class-field-accessor TCPclient railwayManager))))

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

 ;   (define/public (calculateRoute start end graph)
 ;     (if (and (send railManager isDetectionblock? start)
 ;              (send railManager isDetectionblock? end))

  ;        (let ([route (list )]
  ;              [s (send (send railManager getObject start) getTrackID)]
  ;              [e (send (send railManager getObject end) getTrackID)])

  ;          (let-values ([(costs path) (dijkstra graph e)])  ;hashtable and a list returned als result

 ;            (set! route  (constructPath (hash->list path) s e)))
 ;            route)
 ;         (error "RouteCalculator% calculateRoute: Start and ending node are not detectionblocks, route cannot be calculated: " start end)))
    (define/public (calculateRoute start end graph)
      (let ([route (calculate start end graph)])
        (let ([turnloc (uTurnNeeded? route)])
          (when (turnloc)
          (let ([calcTurns (calculateUturn turnloc graph)])
            (set! route (addUTurns route calcTurns))))
        )
      route))

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

  ;  (define/private (uTurnNeeded? route)
  ;    (if (list? route)
  ;        (if (not (null? route))
  ;            (let ([uTurns '()])
  ;              (for ([i route]
  ;                    [idx (in-range (length route))])
  ;                (let ([object (send railManager i)])
  ;                  (when (send railManager isSwitch? object)  ; when you have a switch you need to check if the path goes for y connection to y connection.
  ;                    (let ([Con1 (list-ref route (- idx 1))] ; Switch is sandwitched between 2 connections.
  ;                          [Con2 (list-ref route (+ idx 1))])
  ;                      (when (and (member Con1 (send object getYConnection))
  ;                                 (member Con2 (send object getYConnection)))
  ;                        (append uTurns (cons i Con2)))))))  ; pairs contain the switchID and where to needs to be turned ID (destination)
  ;              (if (not (null? uTurns))
  ;                  uTurns
  ;                  #f))
  ;            (error "RouteCalculator% uTurnNeeded?: Contract violation expected a non empty list, recieved:" route))
  ;        (error "RouteCalculator% uTurnNeeded?: Contract violation expected a list, recieved:" route)))

    (define/private (uTurnNeeded? route)
      (if (list? route)
        (if (not (null? route))
          (let ([uTurnLoc '()])
            (for ([i route]
                  [idx (in-range (length route))])
                  
                (when (send TCPclient TCPcall (list railwayManager 'isSwitch?))  ; when you have a switch, a u turn may be needed
                  (let ([yConnection (send TCPclient TCPcall (list railwayManager 'getYConnection))] ; u turn is needed when the prev node and the next node are connected on the y part of the switch
                        [con1 (list-ref route (- idx 1))]  
                        [con2 (list-ref route (+ idx 1))])

                      (append uTurnLoc (list (cons i con2)))))) ; The two pairs need to stay together

          (if (not (null? uTurnLoc))
              uTurnLoc
              #f))

        (error "RouteCalculator% uTurnNeeded?: Contract violation expected a non empty list, recieved: " route))
      (error "RouteCalculator% uTurnNeeded?: Contract violation expected a list, recieved: " route)))
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

 ;   (define/private (uTurn turnLoc)
 ;     (if (list? turnLoc)
 ;         (if (not (null? turnLoc))
 ;             (if (andmap pair? turnLoc)
 ;                 (let ([detectionBlocks (send railManager getAllDetectionblockID)]
 ;                       [results (make-stack)])
 ;                   (for ([i list])
;
;                      (let ([subResults (make-heap <=?)]
;                            [switchID (car i)]
;                            [connID (cdr i)])
;
;                        (let ([oppositeID (send(send railManager getSwitch switchID) getOppositeConnection connID)])

;                        (for ([block detectionBlocks])
;                          (let ([trackID (send (send railManager getDetectionblock block) getTrackID)])
;                            (let ([currentPath (calculateRoute switchID trackID)])
;                              (when (and (not (member currentPath oppositeID)
;                                         (not (uTurnNeeded? currentPath))))
;                                (heap-add! subResults (set! currentPath (append currentPath (calculateRoute trackID oppositeID))))
;                          ))))
;                          (push! results (heap-min results))
;                          (set! subResults (make-heap <=?));
;
;                          ))))
;                  (error "RouteCalculator% uTurn: Contract violation expected a list of pairs, recieved:" turnLoc))
;              (error "RouteCalculator% uTurn: Contract violation expected a non empty list, recieved:" turnLoc))
;          (error "RouteCalculator% uTurn: Contract violation expected a list, recieved:" turnLoc)))

    (define/private (calculateUturn turnLoc graph)
      (if (list? turnLoc)
        (if (not (null? turnLoc))
          (if (andmap pair? turnLoc)

            (let ([detectionblocks (send TCPclient TCPcall (list railwayManager 'getAllDetectionID))]
                  [results (make-stack)])

              (for ([i turnLoc])
                (let ([switchID (car i)]
                      [connID (cdr i)]
                      [subResults (make-heap <=?)])

                  (let ([oppositeConn (send TCPclient TCPcall (list railwayManager 'getOppositeYConnection switchID connID))])    
                      (for ([block detectionblocks])
                      
                        (let ([trackID (send TCPclient TCPcall (list railwayManager 'getTrackID block))])
                          (let ([switch-track (calculate switchID trackID graph)]
                                [track-conn   (calculate trackID connID graph)])
                                
                                (when (and (not (member connID switch-track))
                                           (not (uTurnNeeded? switch-track))  ; prevent recursion in the path claculation
                                           (not (uTurnNeeded? track-conn)))
                                           
                                      (heap-add! (append switch-track track-conn))))))
                  
                  (push! results (heap-min subResults))
                  (set! subResults (make-heap <=?)))))
                  results) ; return the calculated u turns
          
          (error "RouteCalculator% calculateUturn: Contract violation expected a list of pairs, recieved: " turnLoc))
        (error "RouteCalculator% calculateUturn: Contract violation expected a non empty list, recieved: " turnLoc))
      (error "RouteCalculator% calculateUturn: Contract violation expected a list, recieved: " turnLoc)))

    ;------------------
    ; Function: addUTurns
    ; Parameters: 
    ;       route: list<symbol>
    ;         Use:
    ;       turnStack: stack<list<symbol>>
    ;         Use: 
    ; Output:
    ;     resultRoute: list<symbol>
    ;       Use: 
    ; Use:
    ;---------------------------------------

    (define/private (addUTurns route turnStack)
      (if (list? route)
        (if (not (null? route))
          (if (stack? turnStack)
          (let ([resultRoute route])

            (while (not (stack-empty? turnStack))
              (let ([turn (top turnStack)]
                    [idx    'none]
                    [left   'none]
                    [right  'none])

                (pop! turnStack)
                (set! idx (index-of resultRoute (car turn)))
                (if (= idx (length resultRoute))            ; if the idx = length of the list, the whole list is taken as "right"

                    (begin
                      (set! left (take resultRoute (- idx 1)))
                      (set! resultRoute (append left turn)))

                  (begin
                  (set! left (take resultRoute idx))             ; split the route in two, and take the current switch out of consideration
                  (set! right (take-right resultRoute (+ idx 1)))
                  (set! resultRoute (append left turn))    ; add the turn at the end of the first part of the route
                  (set! resultRoute (append resultRoute right))) ; add the last part of the route at the end of the result
                  )))
                  
                  resultRoute) ; return the extended route
          
          (error "RouteCalculator% addUTurns: Contract violation expected a stack, recieved: " turnStack))
        (error "RouteCalculator% addUTurns: Contract violation expected a non empty list, recieved: " route))
      (error "RouteCalculator% addUTurns: Contract violation expeted a list, recieved: " route)))


    ))
