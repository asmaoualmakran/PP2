#lang racket

(require racket/class)

(provide DrivingObject%)

(define DrivingObject%
  (class object%
    (super-new)

     ; Successor and predecessor can be set to 'none if there is none 

    (field [ID 'uninitialised]
           [predecessorID 'uninitialised]
           [successorID 'uninitialised]
           [trainID 'uninitialised])

    (define/public (initialised?)
      (and (not (eq? ID 'uninitialised))
           (not (eq? predecessorID 'uninitialised))
           (not (eq? successorID 'uninitialised))
           (not (eq? trainID 'uninitialised))))


    (define/public (setID! symbol)
      (if (and (eq? ID 'uninitialised)
               (symbol? symbol))
          (set! ID symbol)
          (error "DrivingObject% setID!: ID already initialised or contract violation, expected symbol received" symbol)))

    (define/public (getID)
      (if (initialised?)
          ID
          (error "DrivingObject% getID: object not initialised, please initialise before use" ID)))

    (define/public (setPredecessorID! symbol)
      (if (symbol? symbol)
          (set! predecessorID symbol)
          (error "DrivingObject% setPredecessorID!: contract violation expected symbol received" symbol)))

    (define/public (getPredecessorID)
      (if (initialised?)
          predecessorID
          (error "DrivingObject% getPredecessorID: object not initialised, please initialise before use" ID)))

    (define/public (getTrainID)
      (if (initialised?)
          trainID
          (error "DrivingObject% getTrainID: object is not initialised, please initialise before use" ID)))

    (define/public (setTrainID! symbol)
      (if (symbol? symbol)
          (set! trainID symbol)
          (error "DrivingObject% setTrainID!: contract violation, expected symbol received" symbol)))

    


    ))