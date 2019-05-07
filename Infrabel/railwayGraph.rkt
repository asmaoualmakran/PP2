#lang racket

(require graph)
(require math/matrix)

(provide RailwayGraph%)

;TODO enable non connections and dead ends.
;----------------------------------------------
; Class: RailwayGraph%
; Parameters: n/a
; Output: object:RailwayGraph%
; Use: Graph constructor and manipulator.
;----------------------------------------------

(define RailwayGraph%
  (class object%
    (super-new)

    (field
     [railwayManager 'none]
     [railGraph 'none])

    (define managerType 'object:RailwayManager%)

    ;----------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;         manager: object:RailwayManager%
    ;          Use: The railway manager that is used by the object.
    ; Output: n/a
    ; Use: Initialise the object.
    ;----------------------------------------------------------------
    
    (define/public (initialise! manager)
      (if (eq? (object-name manager) managerType)
          (begin (setRailManager! manager)
                 (generateGraph!))
          (error "RailwayGraph% initialse!: Contract violation expected railway manager recieved" manager)))

    ;-----------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;    boolean: boolean
    ;      Use: Determine if the object is initialised
    ; Use: Determine if the object is initialised
    ;-----------------------------------------------------

    (define/public (initialised?)
      (and (not (eq? railwayManager 'none))
           (not (eq? railGraph 'none))))

    ;---------------------------------------------------------
    ; Function: getGraph
    ; Parameters: n/a
    ; Output:
    ;     graph: graph
    ;       Use: The graph representing the railway system.
    ; Use: Getting the graph representing the railway.
    ;---------------------------------------------------------

    (define/public (getGraph)
      (if (initialised?)
          railGraph
          (error "RailwayGraph% getGraph: RaiwayGraph% is not initialised, please initialise before use")))

    ;--------------------------------------------------------------------
    ; Function: setRailManager!
    ; Parameters: 
    ;       manager: object:RailwayManager%
    ;         Use: The manager that controlls the railway.
    ; Output: n/a
    ; Use: Set the railwayManager field to the given railway manager.
    ;--------------------------------------------------------------------

    (define/public (setRailManager! manager)
      (if (eq? (object-name manager) managerType)
          (set! railwayManager manager)
          (error "RailwayGraph% setRailManager!: Contract violation, given manager is not of the type object:RailwayManager%.")))

    ;-----------------------------------------------------------
    ; Function: generateGraph!
    ; Parameters:
    ;     manager: object:RailwayManager%
    ;       Use: The manager that contains the railway objects
    ; Output: n/a
    ; Use: Generate the graph representing the railway system
    ;-----------------------------------------------------------

    (define/public (generateGraph!)
      (let ([railObjs (append (send railwayManager getAllTrackID) (send railwayManager getAllSwitchID))]
            [adjList '()])

        (for ([obj railObjs])
          (let ([object (send railwayManager getObject obj)]
                [convCon (convert obj)])

            (set! adjList (append adjList convCon))))
        
        (set! railGraph (unweighted-graph/directed adjList))
        ))

    ;---------------------------------------------------------------------------------
    ; Function: convert
    ; Parameters:
    ;        objID: symbol
    ;         Use: The object and it's connections that needs adding to the graph.
    ; Output:
    ;     list: list<symbol>
    ;      Use: A list containting the railway object's IDs and it's connection's IDs
    ; Use: Placing the object's id and the ids of the connecting objects in a list.
    ;----------------------------------------------------------------------------------

    (define/private (convert objID)
      (if (symbol? objID)
          (if (send railwayManager isMember? objID)
              (let ([object (send railwayManager getObject objID)])

                (let ([connections (send object getConnections)]
                      [result (list )])

                  (for ([i connections])

                    (when (not (eq? i 'none))
                      (set! result (append result (list (list objID i))))))
                  result))
              (error "RailwayGraph% convert: Given objID does not belong to a railway object, recieved: " objID))
          (error "RailwayGraph% convert: Contract violation expected a symbol, recieved: " objID)))
          

    ))