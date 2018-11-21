#lang racket

(require racket/class)

(require "locomotive.rkt")
(require "railcar.rkt")


(provide Train%)

;---------------------------------------------------
; Class: Train%
; Parameters: n/a
; Output: Train% object
; Use: Creating an object that represents a train.
;---------------------------------------------------

(define Train%
  (class object%

    (super-new)

    (field

     [ID 'uninitialised]
     [trainBuild         'uninitialised]
     [traject            'uninitialised]
     [trajectID          'uninitialised]
     [active             'uninitialised]
     [masterLocomotiveID 'uninitialised]
     [rearLocomotiveID   'uninitialised]
     [length             'uninitialised]
     [currentPosition    'uninitialised]
     [lastPosition       'uninitialised]
     [direction          'uninitialised]
     [speed              'uninitialised]
     [currentNode        'uninitialised]
     [nextNode           'uninitialised])
    ;NOTE: node<>position, position is the physical location of the train this can only be a detectionblock.
    ;                      node is the logical position in the traject.

    ; These are variables to enable easier checking of the object types.
    (define locomotiveType 'object:Locomotive%)
    (define railcarType 'object:Railcar%)

    ;------------------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;    boolean: boolean
    ;      Use: A boolean determine whether a object is initialised or not.
    ; Use: Determine whether a object is initialised or not.
    ;------------------------------------------------------------------------
    
    (define/public (initialised?)
      (and (not(eq? ID 'uninitialised))
           (not(eq? trainBuild 'uninitialised))
           (not(eq? traject 'uninitialised))
           (not(eq? trajectID 'uninitialised))
           (not(eq? active 'unitialised))
           (not(eq? masterLocomotiveID 'uninitialised))
           (not(eq? rearLocomotiveID 'uninitialised))
           (not(eq? length 'uninitialised))
           (not(eq? currentPosition 'uninitialised))
           (not(eq? lastPosition 'uninitialised))
           (not(eq? direction 'uninitialised))
           (not(eq? speed 'uninitialised))
           (not(eq? currentNode 'uninitialised))
           (not(eq? nextNode 'uninitialised))))


    ;-----------------------------------------------------
    ; Funtion: initBuild
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Initialise the trainBuild to the empty list.
    ;-----------------------------------------------------
    
    (define/public (initBuild)
      (if (or (initialised?)
              (eq? null trainBuild))
          (error "Train% initBuild: build is alreadt initialised")
          (set! trainBuild (list))))

    ;--------------------------------------
    ; Function: initActive
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Initialise the active field.
    ;--------------------------------------

    (define/public (initActive)
      (set! active false))

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

    ;-----------------------------------------------------------------------
    ; Function: getActive
    ; Parameters: n/a
    ; Output:
    ;    active: boolean
    ;      Use: Determine whether or not a train is active.
    ; Use: Retrieve the boolean determine whether or not a train is active.
    ;-----------------------------------------------------------------------

    (define/public (getActive)
      (if (initialised?)
          active
          (error "Train% getActive: object is not initialised please initialise before use.")))

    ;---------------------------------
    ; Function: switchActive
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Switch the active status.
    ;---------------------------------
    

    (define/public (switchActive)
      (if (initialised?)
          (set! active (not active))
          (error "Train% switchActive: object is not initialised please initialise before use.")))

    ;------------------------------------------------------------
    ; Function: valid?
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
    ; Function: deleteMember!
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the to be deleted object.
    ; Output: n/a
    ; Use: Delete an identification of an object from the train build. The references are not adjusted.
    ;---------------------------------------------------------------------------------------------------

    (define/public (deleteMember! id)
      (if (initialised?)
          (if (member id trainBuild)
              (set! trainBuild (remove id trainBuild))
              (error "Train% deleteMember!: object is not member of the train, it can not be deleted"))
          (error "Train% deleteMember!: object is not initialised, please initialise before use.")))

    ;--------------------------------------------------------------
    ; Function: addMember!
    ; Parameters:
    ;     id: symbol
    ;       Use: The identification of the to be added object.
    ; Ouput: n/a
    ; Use: Add an identification of an object to the train build.
    ;--------------------------------------------------------------

    (define/public (addMember! id)
      (if (initialised?)
          (set! trainBuild (append trainBuild id))
          (error "Train% addMember!: object is not initialised, please initialise before use.")))

    ;----------------------------------------------------------------------
    ; Function: isMember?
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the member that needs to be checked.
    ; Use: Check whether or not an object is a member of the train.
    ;----------------------------------------------------------------------

    (define/public (isMember? id)
      (if (and (symbol? id)
               (initialised?))
      (member id trainBuild)
      (error "Train% isMember?: object is not initialised or contract violation, symbol expected received:"id)))

    ;--------------------------------------------------------------------
    ; Function: getBuild
    ; Parameters: n/a
    ; Output:
    ;    build: list
    ;      Use: The list containing the id's of the connected objects.
    ; Use: Retrieving the list of id's that make part of the train.
    ;--------------------------------------------------------------------
    
    (define/public (getBuild)
      (if (initialised?)
          trainBuild
          (error "Train% getBuild: object is not initialised please initialise before use.")))

    ;--------------------------------------------------------------
    ; Function: getLastID
    ; Parameters: n/a
    ; Output:
    ;     id: symbol
    ;      Use: The id of the last object conneted to the train.
    ; Use: Retrieve the id of the last connected object.
    ;--------------------------------------------------------------
    
    (define/public (getLastID)
      (let get ([lst trainBuild])
        (if (null? (cdr lst))
            (car lst)
            (get (cdr lst)))))
        
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

    ;--------------------------------------------------------
    ; Function: initDirection!
    ; Parameters:
    ;       newdirection: symbol
    ;          Use: The direction of the train to be set.
    ; Output: n/a
    ; Use: Setting the initial direction of the train.
    ;--------------------------------------------------------
    
    (define/public (initDirection! newdirection)
      (if (and(symbol? newdirection)
              (not (initialised?))) 
          (if (or (eq? newdirection 'left)
                  (eq? newdirection 'right))
              (set! direction newdirection)
              (error "Train% setDirection!: expected symbols 'left or 'right" newdirection))
          (error "Train% setDirection!: contract violation direction already initialised or wrong parameter, expected symbol received" newdirection)))

    ;-------------------------------------------------------------
    ; Function: getDirection
    ; Parameters: n/a
    ; Output:
    ;   direction: symbol
    ;     Use: The symbol that provides the train's direction.
    ; Use: Retrieving the train's direction.
    ;-------------------------------------------------------------
    
    (define/public (getDirection)
      (if (initialised?)
          direction
          (error "Train% getDirection: object not initialised, please initialise before use")))

    ;--------------------------------------------------------------------
    ; Function: switchDirection!
    ; Parameters: n/a
    ; Output:
    ;    direction: symbol
    ;      Use: The direction of the train.
    ; Use: Switching the train's direction, to the opposite direction.
    ;--------------------------------------------------------------------

    (define/public (switchDirection!)
      (if (initialised?)
          (if (eq? direction 'left)
              (set! direction 'right)
              (set! direction 'left))
          (error "Train% switchDirection!: object is not initialised, please initialise before use")))

    ;-----------------------------------------
    ; Function: setSpeed!
    ; Parameters:
    ;    number: number
    ;      Use: The speed of the train.
    ; Output: n/a
    ; Use: Setting the speed of the train.
    ;-----------------------------------------
    
    (define/public (setSpeed! number)
      (if (and(number? number)
              (<= 0 number))
          (set! speed number)
          (error "Train% setSpeed!: contract violation positive number expected received" number)))

    ;------------------------------------------
    ; Function: getSpeed
    ; Parameters: n/a
    ; Output:
    ;    speed: number
    ;      Use: Retrieve the train's speed.
    ;------------------------------------------
    
    (define/public (getSpeed)
      (if (initialised?)
          speed
          (error "Train% getSpeed: object is not initialised, please initialise before use")))

               ))