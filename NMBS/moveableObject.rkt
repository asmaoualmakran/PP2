#lang racket

(require racket/class)

(provide MoveableObject%)

;-------------------------------------------------------
; Class: MoveableObject%
; Parameters: n/a
; Output: MoveableObject% : object
; Use: Super class for creating Locomotive and Railcar.
;-------------------------------------------------------

(define MoveableObject%
  (class object%
    (super-new)

    ; Successor and predecessor can be set to 'none if there is none 

    (field [ID 'uninitialised]
           [predecessorID 'uninitialised]
           [successorID 'uninitialised]
           [trainID 'uninitialised])

    ;------------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Ouput:
    ;    boolean
    ;      Use: Determine wheter the object is completely instatiated.
    ; Use: Determine wheter the object is completely instatiated.
    ;------------------------------------------------------------------

    (define/pubment (initialised?)
      (and (not (eq? ID 'uninitialised))
           (not (eq? predecessorID 'uninitialised))
           (not (eq? successorID 'uninitialised))
           (not (eq? trainID 'uninitialised))))

    ;-------------------------------------------------------
    ; Function: setID!
    ; Parameters:
    ;     symbol: symbol
    ;       Use: The identification of the movable object.
    ; Output: n/a
    ; Use: Setting the identification of a movable object.
    ;-------------------------------------------------------

    (define/public (setID! symbol)
      (if (and (eq? ID 'uninitialised)
               (symbol? symbol))
          (set! ID symbol)
          (error "MoveableObject% setID!: ID already initialised or contract violation, expected symbol received" symbol)))

    ;------------------------------------------------
    ; Funcion: getID
    ; Parameters: n/a
    ; Output:
    ;    ID: symbol
    ;     Use: The ID of the movable object.
    ; Use: Retreiving the ID of the movable object.
    ;------------------------------------------------

    (define/public (getID)
      (if (initialised?)
          ID
          (error "MoveableObject% getID: object not initialised, please initialise before use" ID)))

    ;----------------------------------------------------------------------------------
    ; Function: setPredecessorID!
    ; Parameters:
    ;      symbol: symbol
    ;        Use: The identification of the predecessor of the current movable object.
    ; Output: n/a
    ; Use: Setting the identification of the predecessing movable object.
    ;----------------------------------------------------------------------------------

    (define/public (setPredecessorID! symbol)
      (if (symbol? symbol)
          (set! predecessorID symbol)
          (error "MoveableObject% setPredecessorID!: contract violation expected symbol received" symbol)))

    ;----------------------------------------
    ; Function: deletePredecessorID!
    ; Parameters: n/a
    ; Ouput: n/a
    ; Use: Remove the predecessor's ID.
    ;----------------------------------------

    (define/public (deletePredecessorID!)
      (if (initialised?)
          (setPredecessorID! 'none)
          (error "MovableObject% deletePredecessorID!: object not initialised, please initialise before use")))

    ;---------------------------------------------------------------------------
    ; Function: getPredecessorID
    ; Parameters: n/a
    ; Output:
    ;    predecessorID: symbol
    ;        Use: The identification of the predecessing movable object.
    ; Use: Retrieving the identification of the predecessing movable object.
    ;---------------------------------------------------------------------------

    (define/public (getPredecessorID)
      (if (initialised?)
          predecessorID
          (error "MoveableObject% getPredecessorID: object not initialised, please initialise before use" ID)))

    ;------------------------------------------------------------------
    ; Function: setSuccessorID!
    ; Parameters:
    ;     symbol: symbol
    ;       Use: The identification of the successing movable object.
    ; Output: n/a
    ; Use: Setting the successing movable object's identification.
    ;------------------------------------------------------------------

    (define/public (setSuccessorID! symbol)
      (if (symbol? symbol)
          (set! successorID symbol)
          (error "MoveableObject% setSuccessorID!: contract violation expected symbol received" symbol)))

    ;----------------------------------
    ; Function: deleteSuccessorID!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Remove the successor's ID.
    ;----------------------------------

    (define/public (deleteSuccessorID!)
      (if (initialised?)
          (setSuccessorID! 'none)
          (error "MovableObject% deleteSuccessorID!: object not initialised, please initialise before use.")))

    ;---------------------------------------------------------------------
    ; Function: getSuccessorID
    ; Parameters: n/a
    ; Output:
    ;    successorID: symbol
    ;      Use: The identification of the successing movable object.
    ; Use: Retrieve the identification of the successing movable object.
    ;---------------------------------------------------------------------

    (define/public (getSuccessorID)
      (if (initialised?)
          successorID
          (error "MoveableObject% getSuccessorID: object not initialised, please initialise before use")))

    ;-----------------------------------------------------------------------------------------
    ; Function: setTrainID!
    ; Parameters:
    ;    symbol: symbol
    ;      Use: The identification of the train where the movable object is associated with.
    ; Output: n/a
    ; Use: Setting the identification associating train of the movable object.
    ;-----------------------------------------------------------------------------------------

    (define/public (setTrainID! symbol)
      (if (symbol? symbol)
          (set! trainID symbol)
          (error "MoveableObject% setTrainID!: contract violation, expected symbol received" symbol)))

    ;-------------------------------------------------------------
    ; Function: getTrainID
    ; Parameters: n/a
    ; Output:
    ;    trainID: symbol
    ;      Use: Identifiaction of the associating train.
    ; Use: Retrieve the identification of the assocciating train.
    ;-------------------------------------------------------------

    (define/public (getTrainID)
      (if (initialised?)
          trainID
          (error "MoveableObject% getTrainID: object is not initialised, please initialise before use" ID)))
   
    ))