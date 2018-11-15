#lang racket

(require racket/class)

(require "train.rkt")
(require "locomotive.rkt")
(require "railcar.rkt")

(define TrainManager%
  (class object%

    ; Hashtables for each type of objects, these are mutable, initial needed size is unknown.
    (define trainTable (make-hash))
    (define locomotiveTable (make-hash))
    (define railcarTable (make-hash))

    (define/public (createTrain id)   ;TODO make initialisers
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
          

    (define/public (couple! train object)
      'test)

    (define/public (decouple! train object)
      'test)

    


    ))