#lang racket

(require racket/class)

(require "locomotive.rkt")
(require "railcar.rkt")


(provide Train%)

(define Train%
  (class object%

    (super-new)

    (field

     [ID 'uninitialised]
     [trainBuild 'uninitialised]
     [traject 'uninitialised]
     [trajectID 'uninitialised]
     [masterLocomotiveID 'uninitialised]
     [rearLocomotiveID 'uninitialised]
     [length 'uninitialised]
     [currentPosition 'uninitialised]
     [lastPosition 'uninitialised]
     [direction 'uninitialised]
     [speed 'uninitialised]
     [currentNode 'uninitialised]
     [nextNode 'uninitialised])
    ;NOTE: node<>position, position is the physical location of the train this can only be a detectionblock.
    ;                      node is the logical position in the traject.

    (define locomotiveType 'object:Locomotive%)
    (define railcarType 'object:Railcar%)

    
    (define/public (initialised?)
      (and (not(eq? ID 'uninitialised))
           (not(eq? trainBuild 'uninitialised))
           (not(eq? traject 'uninitialised))
           (not(eq? trajectID 'uninitialised))
           (not(eq? masterLocomotiveID 'uninitialised))
           (not(eq? rearLocomotiveID 'uninitialised))
           (not(eq? length 'uninitialised))
           (not(eq? currentPosition 'uninitialised))
           (not(eq? lastPosition 'uninitialised))
           (not(eq? direction 'uninitialised))
           (not(eq? speed 'uninitialised))
           (not(eq? currentNode 'uninitialised))
           (not(eq? nextNode 'uninitialised))))


    ;TODO
    (define/public (initBuild)
      (if (or (initialised?)
              (eq? null trainBuild))
          (error "Train% initBuild: build is alreadt initialised")
          (set! trainBuild (list))))

    ;--------------------------------------------
    ; Function: setID!
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the train.
    ; Output: n/a
    ; Use: Set the identification of the train.
    ;--------------------------------------------

    (define/public (setID! id)
      (if (and (symbol? id)
               (eq? ID 'uninitialised))
          (set! ID id)
          (error "Train% setID!: ID already initialised or contract violation expected symbol received" id ID)))

    ;--------------------------------------------------
    ; Function: getID
    ; Parameters: n/a
    ; Output:
    ;     ID: symbol
    ;      Use: The identification of the train.
    ; Use: Retrieve the identification of the train.
    ;--------------------------------------------------

    (define/public (getID)
      (if (initialised?)
          ID
          (error "Train% getID: object not initialised please initialise before use.")))

    ;------------------------------------------------------------
    ; Function: valid?define/private
    ; Parameters: n/a
    ; Output:
    ;   boolean: boolean
    ;    Use: Whether or not a train construction is valid.
    ; Use: Determine wheter or not a train contruction is valid.
    ;------------------------------------------------------------

    (define/public (valid?)
      (and(initialised?)
          (member masterLocomotiveID trainBuild)
          (or (eq? rearLocomotiveID 'none)
              (member rearLocomotiveID trainBuild))))

    ;-----------------------------------------------------------------------------
    ; Function: setMasterLocomotiveID!
    ; Parameters:
    ;     object: Locomotive% object
    ;       Use: The locomotive that is set to master locomotive.
    ; Output: n/a
    ; Use: Setting a locomotive that is part of the train to master locomotive.
    ;-----------------------------------------------------------------------------

    (define/public (setMasterLocomotiveID! object)
      (if (object? object)
          (if (eq? (object-name object) locomotiveType)   ; check the type of the object
              (if (member (send object getID) trainBuild) ; check if the object is a member of the train
                  (set! masterLocomotiveID (send object getID)) 
                  (error "Train% setMasterLocomotive!: trainBuild does not contain the locomotive, please add before assigning"))
              (error "Train% setMasterLocomotive!: contract violation object has not the type Locomotive% received" object))
          (error "Train setMasterLocomotive!: contract violation expected Train% object received" object)))

    ;--------------------------------------------------------------
    ; Function: getMasterLocomotiveID
    ; Parameters: n/a
    ; Output:
    ;    masterLocomotiveID: symbol
    ;     Use: The identification of the master locomotive.
    ; Use: Retrieve the identification of the master locomotive.
    ;--------------------------------------------------------------
    
    (define/public (getMasterLocomotiveID)
      (if (initialised?)
          masterLocomotiveID
          (error "Train% getMasterLocomotiveID: object is not initialised please initialise before use")))

    ;--------------------------------------------------------------
    ; Function: setRearLocomotive!
    ; Parameters:
    ;      object: object
    ;        Use: The locomotive object that needs to be set.
    ; Output: n/a
    ; Use: Setting a locomotive as rear locomotive.
    ;--------------------------------------------------------------

    (define/public (setRearLocomotive! object)
      (if (object? object)
          (if (eq? (object-name object) locomotiveType)   ; check the type of the object
              (if (member (send object getID) trainBuild) ; check if the object is a member of the train
                  (set! rearLocomotiveID (send object getID)) 
                  (error "Train% setRearLocomotiveID!: trainBuild does not contain the locomotive, please add before assigning"))
              (error "Train% setRearLocomotiveID!: contract violation object has not the type Locomotive% received" object))
          (error "Train setRearLocomotiveID!: contract violation expected Train% object received" object)))

    ;--------------------------------------------------------------
    ; Function: getRearLocomotiveID
    ; Parameters: n/a
    ; Output:
    ;    rearLocomotiveID: symbol
    ;       Use: The identification of the rear locomotive.
    ; Use: Retrieving the identification of the rear locomotive.
    ;--------------------------------------------------------------

    (define/public (getRearLocomotiveID)
      (if (initialised?)
          rearLocomotiveID
          (error "Train% getRearLocomotiveID: object not initialised please initialise before use")))

    ;---------------------------------------------------------------------------------------------------
    ; Fucntion: deleteMember!
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the to be deleted object.
    ; Output: n/a
    ; Use: Delete a identification of an object from the train build. The references are not adjusted.
    ;---------------------------------------------------------------------------------------------------

    (define/public (deleteMember! id)
      (if (initialised?)
          (if (member id trainBuild)
              (set! trainBuild (remove id trainBuild))
              (error "Train% deleteMember!: object is not member of the train, it can not be deleted"))
          (error "Train% deleteMember!: object is not initialised, please initialise before use.")))

    ;TODO

    (define/public (addMember! id)
      (if (initialised?)
          (set! trainBuild (append trainBuild id))
          (error "Train% addMember!: object is not initialised, please initialise before use.")))

    ;TODO
    (define/public (getBuild)
      (if (initialised?)
          trainBuild
          (error "Train% getBuild: object is not initialised please initialise before use.")))
      
    ;---------------------------------------------------------------
    ; Function: setTraject!
    ; Parameters:
    ;     traj: vector<nodes>
    ;       Use: The vector that holds the nodes of the tracject.
    ; Output: n/a
    ; Use: Setting the traject of the train.
    ;---------------------------------------------------------------

    (define/public (setTraject! traj)
      (if (vector? traj)
          (set! traject traj)
          (error "Train% setTraject: contract violation, expected vector received" traj)))

    ;------------------------------------------
    ; Function: deleteTraject!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Deleting the traject of the train.
    ;------------------------------------------

    (define/public (deleteTraject!)
      (if (initialised?)
          (set! traject 'none)
          (error "Train% deleteTraject!: object is not initialised please initialise before use")))

    ;---------------------------------------------
    ; Function: getTraject
    ; Parameters: n/a
    ; Output:
    ;    traject: vector<nodes>
    ;      Use: The traject of the train.
    ; Use: Retrieving the traject of the train.
    ;---------------------------------------------

    (define/public (getTraject)
      (if (initialised?)
          (traject)
          (error "Train% getTraject: object is not initialised please initialise before use")))

    ;------------------------------------------------
    ; Function: setTrajectID!
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the traject.
    ; Output: n/a
    ; Use: Setting the trajectID.
    ;------------------------------------------------
    
    (define/public (setTrajectID! id)
      (if (symbol? id)
          (set! trajectID id)
          (error "Train% setTraject!: contract violation expected symbol received" id)))

    ;----------------------------------------
    ; Function: deleteTrajectID!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Deleteing the train's trajectID.
    ;----------------------------------------

    (define/public (deleteTrajectID!)
      (if (initialised?)
          (set! trajectID 'none)
          (error "Train% deleteTrajectID!: object is not initialised, please initialise before use.")))

    ;------------------------------------------------------
    ; Function: getTrajectID
    ; Parameters: n/a
    ; Output:
    ;    trajectID: symbol
    ;       Use: The traject identification.
    ; Use: Retrieving the identification of the traject.
    ;-----------------------------------------------------

    (define/public (getTrajectID)
      (if (initialised?)
          trajectID
          (error "Train% getTrajectID: object not initialised please initialise before use.")))

    ;----------------------------------------------------------------
    ; Function: setCurrentPosition!
    ; Parameters:
    ;    pos: symbol
    ;     Use: A postion in the traject.
    ; Output: n/a
    ; Use: Setting the currentPosition and update the lastPosition.
    ;----------------------------------------------------------------

    (define/public (setCurrentPosition! pos)
      (if (initialised?)
          (begin  (set! lastPosition currentPosition)
                  (set! currentPosition pos))
          (error "Train% setCurrentPosition!: object not initialised please initialise before use.")))
      
    ;---------------------------------------------------------
    ; Function: setNextNode!
    ; Parameters:
    ;       node: symbol
    ;        Use: The next node that the train will pass.
    ; Output: n/a
    ; Use: Setting the nextNode and update the currentNode.
    ;---------------------------------------------------------
    
    (define/public (setNextNode! node)
      (if (initialised?)
          (begin (set! currentNode nextNode)
                 (set! nextNode node))
          (error "Train% setCurrentNode!: object is not initialised please initialise before use.")))

    ;---------------------------------------------------------------------------------
    ; Function: initPosition
    ; Parameters:
    ;     pos: symbol
    ;      Use: The starting position of the train, this is part of the traject.
    ;     node: symbol
    ;      Use: THe starting node of the train.
    ; Output: n/a
    ; Use: Initialiseing the position and the node of the train.
    ;---------------------------------------------------------------------------------
    
    (define/public (initPosition pos node)
      (if (and (symbol? pos)       ;Test if the parameter's types are correct 
               (symbol? node))
          (if (not (eq? 'uninitialised traject))  ;Check whether the traject is set. 
              (if (vector-member pos traject)     ;Check if the position is part of the traject.
                  (begin (set! currentPosition pos)
                         (set! lastPosition 'none))
                  (error "Train% initPosition: position is not a member of the traject"))
              (error "Train% initPosition: traject is not initialised, please initialise before use."))
          (error "Train% initPosition: contract violation, expected two symbols, received" pos node)))

    (define/public (initDirection! newdirection)
      (if (symbol? newdirection)
          (if (or (eq? newdirection 'left)
                  (eq? newdirection 'right))
              (set! direction newdirection)
              (error "Train% setDirection!: expected symbols 'left or 'right" newdirection))
          (error "Train% setDirection!: contract violation expected number received" newdirection)))

    (define/public (getDirection)
      (if (initialised?)
          direction
          (error "Train% getDirection: object not initialised, please initialise before use")))

    (define/public (switchDirection!)
      (if (initialised?)
          (if (eq? direction 'left)
              (set! direction 'right)
              (set! direction 'left))
          (error "Train% switchDirection!: object is not initialised, please initialise before use")))

    (define/public (setSpeed! number)
      (if (and(number? number)
              (<= 0 number))
          (set! speed number)
          (error "Train% setSpeed!: contract violation positive number expected received" number)))

    (define/public (getSpeed)
      (if (initialised?)
          speed
          (error "Train% getSpeed: object is not initialised, please initialise before use")))

;    (define/public (couple! object)
;      (if (null? trainBuild)               ;the build is empty
;          (if (eq? (object-name object) locomotiveType)   ;object can only be added if it is a locomotive
;              (set! trainBuild (append (send object getID)))  
;              (error "Train% couple!: only a locomotive can be head of the train"))
;          (if (or (eq? (object-name object) locomotiveType)
;                  (eq? (object-name object) railcarType))
;              (set! trainBuild (append (send object getID)))
;              (error "Train% couple!: contract violation, expected locomotive or railcar object received" object))))

;               (define/public (decouple! object)
;                 'test)

               ))