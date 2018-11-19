#lang racket

(require racket/class)

(require "train.rkt")
(require "locomotive.rkt")
(require "railcar.rkt")

;----------------------------------------------------
; Class: TrainManager%
; Parameters: n/a
; Output: TrainManager% object
; Use: Creating an object that manages the trains.
;----------------------------------------------------

(define TrainManager%
  (class object%

    ;TODO make initialisers for each type of object.

    ; Hashtables for each type of objects, these are mutable, initial needed size is unknown.
    (define trainTable (make-hash))
    (define locomotiveTable (make-hash))
    (define railcarTable (make-hash))

    ; These are variables to enable easier checking of the object types.
    (define trainType 'object:Train%)
    (define locomotiveType 'object:Locomotive%)
    (define railcarType 'object:Railcar%)

    
    (define/public (createTrain id)   
      (if (hash-has-key? trainTable id)  ;check whether a id is already used.
          (error "TrainManager% createTrain: ID is already in use, received" id)
          (let ([train (new Train%)])   ; If it's not in use, the train can be added.
            (send train setID! id)
            (send train initBuild)
            (hash-set! trainTable id train))))

    (define/public (deleteTrain id)
      (if (hash-has-key? trainTable id)
          (hash-remove! trainTable id)
          (error "TrainManager% deleteTrain: train is not a member, can't be deleted, received." id)))

    (define/public (getTrain id)
      (if (hash-has-key? trainTable id)
          (hash-ref trainTable id)
          (error "TrainManager% getTrain: train is not a member." id)))

    (define/public (isTrain? id)
      (hash-has-key? trainTable id))

    (define/public (createLocomotive id)
      (if (hash-has-key? locomotiveTable id)
          (error "TrainManager% createLocomotive: ID is already in use, received" id)
          (let ([locomotive (new Locomotive%)])
            (send locomotive setID! id)
            (hash-set! locomotiveTable id locomotive))))

    (define/public (deleteLocomotive id)
      (if (hash-has-key? locomotiveTable id)
          (hash-remove! locomotiveTable id)
          (error "TrainManager% deleteLocomotive: locomotive is not a member, can't be deleted, received." id)))

    (define/public (getLocomotive id)
      (if (hash-has-key? locomotiveTable id)
          (hash-ref locomotiveTable id)
          (error "TrainManager% getLocomotive: locomotive is not a member received:" id)))

    (define/public (isLocomotive? id)
      (hash-has-key? locomotiveTable id))
    
    (define/public (createRailcar id)
      (if (hash-has-key? railcarTable id)
          (error "TrainManager% createRailcar: ID is already in use, received:"id)
          (let ([railcar (new Railcar%)])
            (send railcar setID! id)
            (hash-set! railcarTable id railcar))))

    (define/public (deleteRailcar id)
      (if (hash-has-key? railcarTable id)
          (hash-remove! railcarTable id)
          (error "TrainManager% deleteRailcar: ID is not a member can not be deleted, received:"id)))

    (define/public (getRailcar id)
      (if (hash-has-key? railcarTable id)
          (hash-ref railcarTable id)
          (error "TrainManager% getRailcar: railcar is not a member received:" id)))

    (define/public (isRailcar? id)
      (hash-has-key? railcarTable id))

    (define (findObject id)
      (cond ((hash-has-key? trainTable id)(hash-ref trainTable id))
            ((hash-has-key? locomotiveTable id)(hash-ref locomotiveTable id))
            ((hash-has-key? railcarTable id)(hash-ref railcarTable id))
            (else (error "TrainManager% findObject: id is unknown, object is not created, received"id))))

    (define/public (couple! train object)
      (if (and (isTrain? train)
               (or (isLocomotive? object)
                   (isRailcar? object)))
          (let ([trainID train]                ;When the given objects are valid
                [objID object])
            (let([trainObj (findObject trainID)] ;double lets needed to use the above variables
                 [obj (findObject objID)])
              (if (and (send trainObj initialised?) ;check if the objects are initialised
                       (send obj initialised?))
                  (if (null? (send trainObj getBuild)) ;check if the train build is empty if so, the added object must be a locomotive
                      (if (eq? (object-name obj) locomotiveType)
                          (begin (send trainObj addMember! objID)  ; if the object is a locomotive, connect
                                 (send obj setTrainID! trainID)
                                 (send trainObj setMasterLocomotiveID! objID))
                          (error "TrainManager% couple!: Train build is null, only a locomotive can be added.")) 
                      (begin (send obj setTrainID! trainID)  ; if the build is not null, everything can be connected. (set the train of the object)
                             (send obj setPrecessorID! (send trainObj getLastID)) ; set the predecessor of the to be added object to the last element of the train
                             (send (findObject (send trainObj getLastID)) setSuccesssorID! objID)  ; set the successor of the last element of the train tot the added object
                             (send trainObj addMember! objID) ; add the id of the object to the train.
                             ))  
                  (error "TrainManager% couple!: train or object are not initialised, please initialise before use."))
              ))
          (error "TrainManager% couple!: contract violation train or object don't have an existing id")))
     

    (define/public (decouple! train object)
      'test)

    


    ))