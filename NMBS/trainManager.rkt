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
    ;TODO copy of the railway object must be handled differently

    
    ; Hashtables for each type of objects, these are mutable, initial needed size is unknown.
    ; The elements are hashed useing their id's as key, the values are the objects self.

    (define trainTable (make-hash))
    (define locomotiveTable (make-hash))
    (define railcarTable (make-hash))

    ; These are variables to enable easier checking of the object types.
    
    (define trainType 'object:Train%)
    (define locomotiveType 'object:Locomotive%)
    (define railcarType 'object:Railcar%)
    (define clientType 'object:Client%)

    (field [NMBSclient 'uninitialised])

    ;----------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;       Use: Determine if the object is initialised.
    ; Use: Determine if the object is initialised.
    ;----------------------------------------------------

    (define/public (initialised?)
      (not (eq? NMBSclient 'uninitialised)))

    ;-------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;       cliet: object:Client%
    ;         Use: The TCP client that is going to be used.
    ; Outpunt: n/a
    ; Use: Initialse the object.
    ;-------------------------------------------------------------
    
    (define/public (initialise! client)
      (if (not (initialised?))
              (setClient! client)
          (error "TrainManager% initialise!: Object is already initialised, you cannot reinitialise the object")))

    ;-----------------------------------------------------------------------------------
    ; Function: setClient!
    ; Parameters:
    ;         client: object: client%
    ;           Use: The TCP client that is going to be used by the TrainManager%.
    ; Output: n/a
    ; Use: Set the TCP client of the train manager.
    ;-----------------------------------------------------------------------------------

    (define/public (setClient! client)
      (if (eq? (object-name client) clientType)
          (set! NMBSclient client)
          (error "TrainManger% setClient!: Contract violation expected a TCP client recieved" client)))

    ;------------------------------------------------------
    ; Function: getClient
    ; Parameters: n/a
    ; Output:
    ;     TCP client: object:CLient%
    ;       Use: The TCP client used.
    ; Use: The TCP client used by the train manager.
    ;------------------------------------------------------
    
    (define/public (getClient)
      (if (initialised?)
          NMBSclient
          (error "TrainManager% getClient: Object is not initialised, please initialise before use")))
    
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

    ;---------------------------------------------------------------------------------------------------
    ; Function: createTrain!
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the train.
    ;      pos: symbol
    ;       Use: The location where the train needs to be placed. This must be at a detectable location.
    ;      dir: symbol
    ;       Use: 
    ; Output: n/a
    ; Use: Create a train with an unique identification and safe it in the correct hashtable.
    ;----------------------------------------------------------------------------------------------------
    
    (define/public (createTrain! id pos dir)   
      (if (isUnique? id)  ;check whether a id is already used.
          (let ([train (make-object Train%)])   ; If it's not in use, the train can be added.
            (send train initialise! id pos dir)
            (hash-set! trainTable id train))
          (error "TrainManager% createTrain: ID is already in use, received" id)))

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

    ;---------------------------------------------------------------
    ; Function: getAllTrainID
    ; Parameters: n/a
    ; Output:
    ;     list: list<symbol>
    ;      Use: A list containing all the trains' identifications.
    ; Use: Retrieve the identifiactions of all the trains.
    ;---------------------------------------------------------------
    
    (define/public (getAllTrainID)
      (if (hash-empty? trainTable)
          '()
          (hash-keys trainTable)))

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

    ;-------------------------------------------------------------------------------
    ; Function: setTrainsSpeed!
    ; Parameters:
    ;      id: symbol
    ;       Use: The identifiaction of the train who's speed needs to be adjusted.
    ;      number: number
    ;       Use: The speed that it needs to be set to.
    ; Output: n/a
    ; Use: Change the speed of the train.
    ;--------------------------------------------------------------------------------
    
    (define/public (setTrainSpeed! id number)
      (if (and(symbol? id)
              (number? number))
          (if (isTrain? id)
              (send (getTrain id) setSpeed! number)
              (error "TrainManager% setTrainSpeed!: id does not belong to a train"))
          (error "TrainManager% setTrainSpeed!: contract violation expected number and symbol")))

    ;----------------------------------------------------------------------
    ; Function: getTrainSpeed
    ; Parameters: 
    ;      id: symbol
    ;       Use: The id of the train who's speed needs to be retrieved.
    ; Ouput:
    ;      speed: number
    ;        Use: The current speed of the train.
    ; Use: Retrieve the speed of the train.
    ;----------------------------------------------------------------------
          
    (define/public (getTrainSpeed id)
      (if (symbol? id)
          (if (isTrain? id)
              (send (getTrain id) getSpeed)
              (error "TrainManager% getTrainSpeed: id does not belong to a train"))
          (error "TrainManager% getTrainSpeed!: contract violation expected symbol recieved"id)))

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

    ;-----------------------------------------------------------------------------------
    ; Function: initLocomotive!
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the locomotive that needs to be initialised.
    ;       direction: symbol
    ;        Use: The driving direction of the locomotive.
    ; Use: Initialise the given locomotive.
    ;-----------------------------------------------------------------------------------

    (define/public (initLocomotive! id direction)
      (if (isLocomotive? id)
          (let ([locomotive (getLocomotive id)])
            (if (not(send locomotive initialised?))
                (begin
                  (send locomotive setPredecessorID! 'none)
                  (send locomotive setSuccessorID! 'none)
                  (send locomotive setTrainID! 'none)
                  (send locomotive setDirection! direction))
                (error "TrainManager% initLocomotive!: object is already initialised, cannot be reinitialised")))
          (error "TrainManager% initLocomotive!: provided ID does not belong to an excisting locomotive" id)))         

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

    ;-------------------------------------------------------------------
    ; Function: getAllLocomotiveID
    ; Parameters: n/a
    ; Output:
    ;    list: list<symbol>
    ;     Use: A list containing all the locomotives' identifications.
    ; Use: Retrieve every locomotive's identification.
    ;-------------------------------------------------------------------
    
    (define/public (getAllLocomotiveID)
      (if (hash-empty? locomotiveTable)
          '()
          (hash-keys locomotiveTable)))

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

    ;-------------------------------------------------------------------------
    ; Function: initRailcar!
    ; Parameters:
    ;        id: symbol
    ;         Use: The identification of the to be initialised railcar.
    ;        capacity: number
    ;         Use: The capacity of the railcar.
    ; Output: n/a
    ; Use: Initialise the given id's railcar.
    ;-------------------------------------------------------------------------

    (define/public (initRailcar! id capacity)
      (if (isRailcar? id)
          (let ([railcar (getRailcar id)])
            (if (not (send railcar initialsed?))
                (begin (send railcar setPredecessorID! 'none)
                       (send railcar setSuccessorID! 'none)
                       (send railcar setTrainID! 'none)
                       (send railcar setCapacity! capacity))
                (error "TrainManager% initRailcar!: object is already initialised, it cannot be reinitialised")))
          (error "TrainManager% initRailcar!: identification does not belong to a excisting railcar" id)))

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

    ;------------------------------------------------------------------
    ; Function: getAllRailcarID
    ; Parameters: n/a
    ; Output:
    ;    list: list<symbol>
    ;     Use: A list containing all the railcars' identifications.
    ; Use: Retrieve all the railcars' identifications.
    ;------------------------------------------------------------------
    
    (define/public (getAllRailcarID)
      (if (hash-empty? railcarTable)
          '()
          (hash-keys railcarTable)))

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