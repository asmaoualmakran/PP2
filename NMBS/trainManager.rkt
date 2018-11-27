#lang racket

(require racket/class)

(require "train.rkt")
(require "locomotive.rkt")
(require "railcar.rkt")

(provide TrainManager%)

; NOTE: There needs to be checked if a train build is valid before use.

;----------------------------------------------------
; Class: TrainManager%
; Parameters: n/a
; Output: TrainManager% object
; Use: Creating an object that manages the trains.
;----------------------------------------------------

(define TrainManager%
  (class object%

    (super-new)

    ;TODO make initialisers for each type of object.
    ;TODO enable changing directions and setting speed of trains 
    

    ; Hashtables for each type of objects, these are mutable, initial needed size is unknown.
    ; The elements are hashed useing their id's as key, the values are the objects self.
    
    (define trainTable (make-hash))
    (define locomotiveTable (make-hash))
    (define railcarTable (make-hash))

    ; These are variables to enable easier checking of the object types.
    
    (define trainType 'object:Train%)
    (define locomotiveType 'object:Locomotive%)
    (define railcarType 'object:Railcar%)

                             

    ;-----------------------------------------------------------------------------
    ; Function: isUnique?
    ; Parameters:
    ;      id: symbol
    ;       Use: The id that needs to be checked on uniqueness.
    ; Output:
    ;     boolean: boolean
    ;       Use: Boolean to determine whether or not an id is already in use.
    ; Use: Check if a symbol is already in use.
    ;----------------------------------------------------------------------------
    
    (define/private (isUnique? id)
      (if (symbol? id)
          (and(not(hash-has-key? trainTable id))
              (not(hash-has-key? locomotiveTable id))
              (not(hash-has-key? railcarTable id)))
          (error "TrainManager% isUnique?: contract violation, expected symbol received" id)))

    ;------------------------------------------------------------------------------------------
    ; Function: createTrain!
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the train.
    ; Output: n/a
    ; Use: Create a train with an unique identification and safe it in the correct hashtable.
    ;------------------------------------------------------------------------------------------
    
    (define/public (createTrain! id)   
      (if (isUnique? id)  ;check whether a id is already used.
          (let ([train (make-object Train%)])   ; If it's not in use, the train can be added.
            (send train setID! id)
            (send train initBuild)
            (hash-set! trainTable id train))
          (error "TrainManager% createTrain: ID is already in use, received" id)))

    ;--------------------------------------------------------------------------------
    ; Function: initTrain
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification of the train that needs to be initialised.
    ; Use: Initialise the train.
    ;-------------------------------------------------------------------------------
    
    (define/public (initTrain id)  
      (if (isTrain? id)
          (let ([train (getTrain id)])
            (if (not(send train initialsed?))
                (begin
                  (send train initBuild)
                  (send train initActive)
                  (send train setTraject! 'none)
                  (send train setTrajectID! 'none)
                  (send train setCurrentPosition! 'none)
                  (send train setDirection! 'none)
                  (send train setSpeed! 0)
                  (send train currentNode! 'none)
                  (send train setMasterLocomotive! 'none)
                  (send train setRearLocomotive! 'none)
                  (send train setLastPosition! 'none)
                  (send train setNextNode! 'none))
            (error "TrainManager initTrain%: Train is already initialised."id)))
          (error "TrainManager% initTrain: id does not exist or is not from a train."id)))

    ;-------------------------------------------------------------
    ; Function: deleteTrain!
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification of the to be deleted train.
    ; Output: n/a
    ; Use: Delete an existing train from the hashtable.
    ;-------------------------------------------------------------

    (define/public (deleteTrain! id)
      (if (and(isTrain? id)     ;check if there is a train with this id
              (not (send(getTrain id) getActive)))  ; check whether the train is active
          (hash-remove! trainTable id)
          (error "TrainManager% deleteTrain: train is not a member, can't be deleted, received." id)))

    ;---------------------------------------------------------------------
    ; Function: getTrain
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the train.
    ; Output:
    ;    train: object:Train%
    ;      Use: The train object that needs to be retrieved.
    ; Use: Retrieving a train object from the hashtable useing it's id.
    ;---------------------------------------------------------------------

    (define/public (getTrain id)
      (if (isTrain? id)
          (hash-ref trainTable id)
          (error "TrainManager% getTrain: train is not a member." id)))

    ;--------------------------------------------------------------------------------
    ; Function: isTrain?
    ; Parameters:
    ;      id: symbol
    ;       Use: The identifiaction of train that needs to be checked.
    ; Output:
    ;     boolean: boolean
    ;        Use: Determine whether or not a given id is from an excisting train.
    ; Use: Determine whether or not the given id is from an excisting train.
    ;--------------------------------------------------------------------------------

    (define/public (isTrain? id)
      (if (symbol? id)
          (hash-has-key? trainTable id)
          (error "TrainManager% isTrain?: contract violation expected symbol received"id)))
    

    ;--------------------------------------------------------------------------------------
    ; Function: initDirection!
    ; Parameters:
    ;      id: symbol
    ;       Use: The train who's driving direction needs to be initialised.
    ; Output: n/a
    ; Use: Initialise the driving direction of the train, using it's master locomotive.
    ;--------------------------------------------------------------------------------------
    
    (define (initDirection! id)   ; the initialisation happens automaticallly when building the train.
      (if (isTrain? id)
          (let ([train (getTrain id)])
            (if (and(send train initialised?)
                    (eq? (send train getDirection)))
                (if (and(not (null? (send train getBuild)))
                        (isLocomotive? (send train getMasterLocomotive)))
                    (let ([locomotive (getLocomotive (send train getMasterLocomotive))])
                      (send train initDirection! (send locomotive getDirection)))
                    (error "TrainManager% initDirection!: train has no master locomotive, directions can not be initialised"))
                (error "TrainManager% initDirection!: train object is not initialised, please initialise before use")))
          (error "TrainManager% initDirection!: given object id is not a train")))

    ;-------------------------------------------------------------------------------------
    ; Function: switchDirection!
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the train who's direction needs to be changed.
    ; Output: n/a
    ; Use: Switch the direction of the train.
    ;-------------------------------------------------------------------------------------

    (define/public (switchDirection! id)  ;NOTE: this method needs extending, the master- and rear locomotives also need swapping!
      (if (isTrain? id)
          (let ([train (getTrain id)])
            (if (and (not (eq? 'none (send train getMasterLocomotive)))
                     (isLocomotive? (send train getMasterLocomotive)))
                (let ([locomotive (getLocomotive (send train getMasterLocomotive))])
                  (send train switchDirection!)
                  (send locomotive switchDirection!))
                (error "TrainManager% switchDirection!: given train has no excisting master locomotive")))
          (error "TrainManager% switchDirection!: given id does not belong to a excisting train")))
                
    ;-----------------------------------------------------------
    ; Function: createLocomotive!
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification of the locomotive.
    ; Output: n/a
    ; Use: Create a locomotive with an unique identification.
    ;-----------------------------------------------------------

    (define/public (createLocomotive! id)
      (if (isUnique? id)
          (let ([locomotive (new Locomotive%)])
            (send locomotive setID! id)
            (hash-set! locomotiveTable id locomotive))
          (error "TrainManager% createLocomotive: ID is already in use, received" id)))

    ; (define/public (initLocomotive! id

    ;------------------------------------------------------------------------------------------
    ; Funcion: deleteLocomotive!
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification of the to be removed locomotive
    ; Output: n/a
    ; Use: Delete a locomotive from the hashtable and decouple it from the corresponding train.
    ;-------------------------------------------------------------------------------------------

    (define/public (deleteLocomotive! id)
      (if (isLocomotive? id)
          (hash-remove! locomotiveTable id)
          (error "TrainManager% deleteLocomotive: locomotive is not a member, can't be deleted, received." id)))

    ;----------------------------------------------------------------------------------
    ; Function: getLocomotive
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the locomotive that needs to be retreived.
    ; Output:
    ;     locomotive: object:Locomotive%
    ;        Use: The locmotive object that needed to be retrieved.
    ; Use: Retrieve an excisting locomotive object from the hashtable.
    ;-----------------------------------------------------------------------------------
    
    (define/public (getLocomotive id)
      (if (isLocomotive? id)
          (hash-ref locomotiveTable id)
          (error "TrainManager% getLocomotive: locomotive is not a member received:" id)))

    ;---------------------------------------------------------------------------------------------
    ; Function: isLocomotive?
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the locomotive that needs to be checked.
    ; Output:
    ;   boolean: boolean
    ;      Use: Determine whether or not the locomotive excists in the hashtables.
    ; Use: Check whether or not there excists a locomotive with the corresponding identification.
    ;----------------------------------------------------------------------------------------------

    (define/public (isLocomotive? id)
      (if (symbol? id)
          (hash-has-key? locomotiveTable id)
          (error "TrainManager% isLocomotive: contract violation expected symbol received" id)))

    ;--------------------------------------------------------------------------------------------
    ; Function: createRailcar!
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the to be created railcar object.
    ; Output: n/a
    ; Use: Create a railcar with an unique identification and save it in the correct hashtable.
    ;--------------------------------------------------------------------------------------------
    
    (define/public (createRailcar! id)
      (if (isUnique? id)
          (let ([railcar (new Railcar%)])
            (send railcar setID! id)
            (hash-set! railcarTable id railcar))
          (error "TrainManager% createRailcar: ID is already in use, received:"id)))

    ;--------------------------------------------------------------
    ; Function: deleteRailcar!
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the to be deleted railcar.
    ; Output: n/a
    ; Use: Delete and excisting railcar from the hashtable.
    ;--------------------------------------------------------------

    (define/public (deleteRailcar! id)
      (if (isRailcar? id)
          (hash-remove! railcarTable id)
          (error "TrainManager% deleteRailcar: ID is not a member can not be deleted, received:"id)))

    ;-----------------------------------------------------------------
    ; Function: getRailcar
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the to be retrieved railcar.
    ; Output:
    ;    railcar: object:Railcar%
    ;      Use: The railcar object that needed to be retrieved.
    ; Use: Retrieve an excisting railcar object from the hashtable.
    ;-----------------------------------------------------------------
    
    (define/public (getRailcar id)
      (if (isRailcar? id)
          (hash-ref railcarTable id)
          (error "TrainManager% getRailcar: railcar is not a member received:" id)))

    ;----------------------------------------------------------------------------
    ; Function: isRailcar?
    ; Parameters:
    ;        id: symbol
    ;         Use: The identification of the railcar that needs to be checked.
    ; Output:
    ;    boolean: boolean
    ;     Use: Determine whether or not the railcar excists.
    ; Use: Determine wheter or not a given railcar excists in the hashtable.
    ;----------------------------------------------------------------------------

    (define/public (isRailcar? id)
      (if (symbol? id)
          (hash-has-key? railcarTable id)
          (error "TrainManager% isRailcar?: contract violation symbol expected, received" id)))

    ;----------------------------------------------------------------------------------
    ; Function: findObject
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the object that needs to be found.
    ; Output:
    ;    object: object:Train% || object:Locomotive% || object:railcar%
    ;      Use: The retrieved object if it excists.
    ; Use: Retrieve the object from the hashtables, if the type of object is unknown.
    ;---------------------------------------------------------------------------------

    (define (findObject id)
      (cond ((isTrain? id)(getTrain id))
            ((isLocomotive? id)(getLocomotive id))
            ((isRailcar? id)(getRailcar id))
            (else
             (error "TrainManager% findObject: id is unknown, object is not created, received"id))))

    ;-----------------------------------------------------------------------------------------------------------------
    ; Function: couple!
    ; Parameters:
    ;      train: symbol
    ;        Use: The train's id where a locomotive or railcar will be connected.
    ;      object: symbol
    ;        Use: The object's id that gets connected to the train. This will be either a railcar or a locomotive.
    ; Use: Conneting a railcar or a locomotive to a train.
    ;-----------------------------------------------------------------------------------------------------------------

    (define/public (couple! train object)   
      (if (and (isTrain? train)
               (or (isLocomotive? object)
                   (isRailcar? object)))
          (let ([trainID train]                ;When the given objects are valid
                [objID object])
            (let([trainObj (findObject trainID)] ;double lets needed to use the above variables
                 [obj (findObject objID)])
              (if (and (send trainObj initialised?) ;check if the objects are initialised
                       (send obj initialised?)
                       (eq? 'none (send obj getTrainID)))  ;The object can not be coupled to an other train before coupling.
                  (if (null? (send trainObj getBuild)) ;check if the train build is empty if so, the added object must be a locomotive
                      (if (eq? (object-name obj) locomotiveType)
                          (begin (send trainObj addMember! objID)  ; if the object is a locomotive, connect
                                 (send obj setTrainID! trainID)
                                 (send trainObj setMasterLocomotiveID! objID)
                                 (initDirection! trainID))
                          (error "TrainManager% couple!: Train build is null, only a locomotive can be added.")) 
                      (begin (send obj setTrainID! trainID)  ; if the build is not null, everything can be connected. (set the train of the object)
                             (send obj setPrecessorID! (send trainObj getLastID)) ; set the predecessor of the to be added object to the last element of the train
                             (send (findObject (send trainObj getLastID)) setSuccesssorID! objID)  ; set the successor of the last element of the train tot the added object
                             (send trainObj addMember! objID) ; add the id of the object to the train.
                             ))  
                  (error "TrainManager% couple!: train or object are not initialised, please initialise before use."))
              ))
          (error "TrainManager% couple!: contract violation train or object don't have an existing id")))

    ;----------------------------------------------------------------------------------------------------------------
    ; Function: decouple!
    ; Parameters:
    ;      train: symbol
    ;        Use: The train's id where a locomotive or railcar will be disconected.
    ;      object: symbol
    ;        Use: The object's id that gets disconected to the train. This will be either a railcar or a locomotive.
    ; Use: Disconect a locomotive or railcar from the train.
    ;-----------------------------------------------------------------------------------------------------------------

    (define/public (decouple! train object)
      (if (and (isTrain? train)
               (or (isLocomotive? object)
                   (isRailcar? object)))
          (let ([trainID train]        ;nesting let's to enable direct use of the variables
                [objID object])
            (let ([trainObj (findObject trainID)]
                  [obj (findObject objID)])
              (if (send trainObj isMember? objID)  ;check if the object is part of the train
                  (if (eq? objID (send trainObj getMasterLocomotive))  ; if the to be deleted object is a masterLocomotive, error is raised
                      (error "TrainManager% decouple!: object is a master locomotive it can't be deleted")
                      (begin (let ([succID (send obj getSuccessor)]    ;start deleting all references to the train
                                   [predID (send obj getPredecessor)])  ;getting the successor and predecessor of the to be deleted object
                               (let ([succObj (findObject succID)]      
                                     [predObj (findObject predID)])
                                 (send obj deleteTrainID!)
                                 (send succObj setPredecessorID predID)   ;Setting the successor id's and the predecessor id's of the objects to the correct one
                                 (send predID setSuccessorID succID)
                                 (send trainObj deleteMember! objID)
                                 (send obj deletePredecessorID!)
                                 (send obj deleteSuccessorID!)
                                 ))))
                  (error "TrainManager% decouple!: object is not a member of the train"objID))
              ))
          (error "TrainManager% decouple!: Contract violation expected train object id and locomotive or railcar id object expected received." train object)))

    ))