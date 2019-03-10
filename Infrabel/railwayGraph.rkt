#lang racket

(require graph)
(require math/matrix)

(provide RailwayGraph%)

(define RailwayGraph%
  (class object%
    (super-new)

    (field
     [railwayManager 'none]
     [railGraph 'none])

    (define managerType 'object:RailwayManager%)

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
    ; Function: genetrateGraph
    ; Parameters:
    ;     manager: object:RailwayManager%
    ;       Use: The manager that contains the railway objects
    ; Output: n/a
    ; Use: Generate the graph representing the railway system
    ;-----------------------------------------------------------
    
    (define/public (generateGraph manager)
      (let ([railObjs (append (send manager getAllTrackID) (send manager getAllSwitchID))]
            [adjList '()])

        (for ([obj railObjs])
          (add-between adjList #:after-last (convert obj manager))) ;building the adj list
        (set! railGraph ((unweighted-graph/directed adjList)))))

    ;---------------------------------------------------------------------------------
    ; Function: convert
    ; Parameters:
    ;      obj: object:Track% / object:Switch%
    ;       Use: The railway object.
    ; Output:
    ;     list: list<symbol>
    ;      Use: A list of symbols, identifications.
    ; Use: Placing the object's id and the ids of the connecting objects in a list.
    ;---------------------------------------------------------------------------------

    (define/private (convert obj manager)
      (if (eq? (object-name manager) managerType)
          (if (send manager isTrack? obj)
              (append (list obj) (send (send manager getTrack obj) getConnections))
              (if (send manager isSwitch? obj)
                  (append (list obj) (send (send manager getSwitch obj) getConnections))
                  (error "RailwayGraph% convert: Contract violation given obj is not a switch or a track.")))
          (error "RailwayGraph% convert: Contract violation given manager is not of the type RailwayManager%")))

    ))