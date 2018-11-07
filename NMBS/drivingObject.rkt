#lang racket

(require racket/class)

(provide DrivingObject%)

;-------------------------------------------------------
; Class: DrivingObject%
; Parameters: n/a
; Output: DrivingObject% : object
; Use: Super class for creating Locomotive and Railcar.
;-------------------------------------------------------

(define DrivingObject%
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

    (define/public (initialised?)
      (and (not (eq? ID 'uninitialised))
           (not (eq? predecessorID 'uninitialised))
           (not (eq? successorID 'uninitialised))
           (not (eq? trainID 'uninitialised))))

;-------------------------------------------------------
; Function: setID!
; Parameters:
;     symbol: symbol
;       Use: The identification of the driving object.
; Output: n/a
; Use: Setting the identification of a driving object.
;-------------------------------------------------------

    (define/public (setID! symbol)
      (if (and (eq? ID 'uninitialised)
               (symbol? symbol))
          (set! ID symbol)
          (error "DrivingObject% setID!: ID already initialised or contract violation, expected symbol received" symbol)))

;------------------------------------------------
; Funcion: getID
; Parameters: n/a
; Output:
;    ID: symbol
;     Use: The ID of the driving object.
; Use: Retreiving the ID of the driving object.
;------------------------------------------------

    (define/public (getID)
      (if (initialised?)
          ID
          (error "DrivingObject% getID: object not initialised, please initialise before use" ID)))

;----------------------------------------------------------------------------------
; Function: setPredecessorID!
; Parameters:
;      symbol: symbol
;        Use: The identification of the predecessor of the current driving object.
; Output: n/a
; Use: Setting the identification of the predecessing driving object.
;----------------------------------------------------------------------------------

    (define/public (setPredecessorID! symbol)
      (if (symbol? symbol)
          (set! predecessorID symbol)
          (error "DrivingObject% setPredecessorID!: contract violation expected symbol received" symbol)))

;---------------------------------------------------------------------------
; Function: getPredecessorID
; Parameters: n/a
; Output:
;    predecessorID: symbol
;        Use: The identification of the predecessing driving object.
; Use: Retrieving the identification of the predecessing driving object.
;---------------------------------------------------------------------------

    (define/public (getPredecessorID)
      (if (initialised?)
          predecessorID
          (error "DrivingObject% getPredecessorID: object not initialised, please initialise before use" ID)))

;------------------------------------------------------------------
; Function: setSuccessorID!
; Parameters:
;     symbol: symbol
;       Use: The identification of the successing driving object.
; Output: n/a
; Use: Setting the successing driving object's identification.
;------------------------------------------------------------------

    (define/public (setSuccessorID! symbol)
      (if (symbol? symbol)
          (set! successorID symbol)
          (error "DrivingObject% setSuccessorID!: contract violation expected symbol received" symbol)))

;---------------------------------------------------------------------
; Function: getSuccessorID
; Parameters: n/a
; Output:
;    successorID: symbol
;      Use: The identification of the successing driving object.
; Use: Retrieve the identification of the successing driving object.
;---------------------------------------------------------------------

    (define/public (getSuccessorID)
      (if (initialised?)
          successorID
          (error "DrivingObject% getSuccessorID: object not initialised, please initialise before use")))

;-----------------------------------------------------------------------------------------
; Function: setTrainID!
; Parameters:
;    symbol: symbol
;      Use: The identification of the train where the driving object is associated with.
; Output: n/a
; Use: Setting the identification associating train of the driving object.
;-----------------------------------------------------------------------------------------

    (define/public (setTrainID! symbol)
      (if (symbol? symbol)
          (set! trainID symbol)
          (error "DrivingObject% setTrainID!: contract violation, expected symbol received" symbol)))

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
          (error "DrivingObject% getTrainID: object is not initialised, please initialise before use" ID)))

;------------------------------------------------------------------------------------------------
; Function: couple!
; Parameters:
;    train: symbol
;      Use: The identification of the associating train. This can not be 'none
;    pre: symbol
;     Use: The identification of the predecessing driving object.
;    succ: symbol
;     Use: The identificationf of the successing driving object.
; Use: Connecting a driving object to a train, and potentionally a successor and predecessor.
;      if there isn't a successor and predecessor this can be set to the symbol 'none. 
;------------------------------------------------------------------------------------------------

    (define/public (couple! train pre succ)
      (if (and (and (symbol? train)(not (eq? train 'none)))   ; A driving object must be connected to a train, this can't be none
               (symbol? pre)
               (symbol? succ))
          (begin
            (set! trainID train)
            (set! predecessorID pre)
            (set! successorID succ))
          (error "DrivingObject% couple!: contract violation symbol expected received" train pre succ)))

   
    ))