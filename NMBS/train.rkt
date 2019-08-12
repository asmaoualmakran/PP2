  #lang racket

  (require racket/class)


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
      [traject            'uninitialised]
      [trajectID          'uninitialised]
      [active             'uninitialised]
      [position           'uninitialised]
      [direction          'uninitialised]
      [speed              'uninitialised])


      ;------------------------------------------------------------------------
      ; Function: initialised?
      ; Parameters: n/a
      ; Output:
      ;    boolean: boolean
      ;      Use: A boolean determine whether a object is initialised or not.
      ; Use: Determine whether a object is initialised or not.
      ;------------------------------------------------------------------------
      
      (define/public (initialised?)
        (and (not (eq? active 'uninitialised))
            (not (eq? position 'uninitialised))
            (not (eq? direction 'uninitialised))
            (not (eq? speed 'uninitialised))
            (not (eq? ID 'uninitialised))))

      ;-------------------------------------------------------------------------------------
      ; Function: initialise! 
      ; Parameters:
      ;       position: symbol
      ;         Use: The location where the train is located
      ;       direction: symbol
      ;         Use: The direction the train is driving in, this is it's previous location.
      ; Output: n/a 
      ; Use: Initialise the train object, with it's needed information.
      ;--------------------------------------------------------------------------------------

      (define/public (initialise! ID position direction)
          (if (not (initialised?))
            (if (and (symbol? position)
                    (symbol? direction)
                    (symbol? ID))
              
                (begin 
                (setID! ID)
                (set! active #f)
                (setSpeed! 0)
                (setDirection! direction)
                (setPosition! position))

                (error "Train% initialise!: Contract violation expected three symbols, recieved: " ID position direction))
          (error "Train% initialise!: Object is already initialised.")))

      ;---------------------------------------------------------
      ; Function: setPosition!
      ; Parameters: 
      ;       pos: symbol
      ;         Use: The position where the train is located.
      ; Output: n/a 
      ; Use: Change the position of the train.
      ;---------------------------------------------------------

      (define/public (setPosition! pos)
        (if (symbol? position)
            (set! position pos)
            (error "Train% setPosition!: Contract violation expected a symbol, recieved: " pos)))

      ;--------------------------------------------
      ; Function: getPosition
      ; Parameters: n/a 
      ; Output: 
      ;      position: symbol
      ;        Use: The location of the train.
      ; Use: Get the location of the train.
      ;--------------------------------------------

      (define/public (getPosition)
        (if (initialised?)
            position
            (error "Train% getPosition: Object is not initialised.")))

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
      ; Function: switchActive!
      ; Parameters: n/a
      ; Output: n/a
      ; Use: Switch the active status.
      ;---------------------------------
      
      (define/public (switchActive!)
        (if (initialised?)
            (set! active (not active))
            (error "Train% switchActive: object is not initialised please initialise before use.")))
            
      ;------------------------------------------------------------------
      ; Function: setTraject!
      ; Parameters:
      ;     traj: list<symbol>
      ;       Use: The list that holds the traject
      ; Output: n/a
      ; Use: Setting the traject of the train and the correct direction.
      ;------------------------------------------------------------------

      (define/public (setTraject! traj)
        (if (list? traj)
            (if (not (getActive))
                (begin
                  (set! traject traj)
                  (switchActive!)
                  (setDirection! (cadr traj)))
                (error "Train% setTraject: Train is already activated, cannot drive another traject."))
            (error "Train% setTraject: contract violation, expected vector received" traj)))

      ;-------------------------------------------------------------------------
      ; Function: deleteTraject!
      ; Parameters: n/a
      ; Output: n/a
      ; Use: Deleting the traject of the train, and make the train non active.
      ;-------------------------------------------------------------------------

      (define/public (deleteTraject!)
        (if (initialised?)
          (when (getActive)
          
            (switchActive!)
            (setTraject! 'none))
          (error "Train% deleteTraject!: Object is not initialised please initialise before use.")))
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

      ;-----------------------------------------------
      ; Function: setDirection!
      ; Parameters: 
      ;       dir: symbol
      ;         Use: The new direction of the train.
      ; Output: n/a 
      ; Use: Change the direction of the train.
      ;------------------------------------------------

      (define/public (setDirection! dir)
          (if (symbol? dir)
            (set! direction dir)
          (error "Train% setDirection!: Contract violation expected a symbol, recieved: " dir)))

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


      ;-----------------------------------------
      ; Function: setSpeed!
      ; Parameters:
      ;    number: number
      ;      Use: The speed of the train.
      ; Output: n/a
      ; Use: Setting the speed of the train.
      ;-----------------------------------------
      
      (define/public (setSpeed! number)
        (if (number? number)
            (set! speed number)
            (error "Train% setSpeed!: contract violation number expected received" number)))

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


      ; Code that enables expansion to use with railcar and locomotives. 
      ; Enables composeing a train existing from raicars and locomotives.
      ; The folowing fields need to be added to the object: 
      ; trainBuild, length, rearLocomotiveID, masterLocomotive

      ;-----------------------------------------------------
      ; Funtion: initBuild
      ; Parameters: n/a
      ; Output: n/a
      ; Use: Initialise the trainBuild to the empty list.
      ;-----------------------------------------------------
      
      ; (define/public (initBuild)
      ;   (if (or (initialised?)
      ;           (eq? null trainBuild))
      ;       (error "Train% initBuild: build is alreadt initialised")
      ;       (begin (set! trainBuild (list))
      ;              (setLength! 0))))

      ;------------------------------------------------------------
      ; Function: valid?
      ; Parameters: n/a
      ; Output:
      ;   boolean: boolean
      ;    Use: Whether or not a train construction is valid.
      ; Use: Determine wheter or not a train contruction is valid.
      ;------------------------------------------------------------

      ; (define/public (valid?)
      ;   (and (initialised?)
      ;       (member masterLocomotiveID trainBuild)
      ;       (or (eq? rearLocomotiveID 'none)
      ;           (member rearLocomotiveID trainBuild))))

      ;-----------------------------------------------------------------------------
      ; Function: setMasterLocomotiveID!
      ; Parameters:
      ;     object: Locomotive% object
      ;       Use: The locomotive that is set to master locomotive.
      ; Output: n/a
      ; Use: Setting a locomotive that is part of the train to master locomotive.
      ;-----------------------------------------------------------------------------

      ; (define/public (setMasterLocomotiveID! id)
      ;   (if (symbol? id)
      ;       (if (not(member id trainBuild))
      ;           (set! masterLocomotiveID id)
      ;           (error "Train% setMasterLocomotiveID!: locomotive is already part of the train"))
      ;       (error "Train% setMasterLocomotiveID!: contract violation expected symbol received")))
                
      ;--------------------------------------------------------------
      ; Function: getMasterLocomotiveID
      ; Parameters: n/a
      ; Output:
      ;    masterLocomotiveID: symbol
      ;     Use: The identification of the master locomotive.
      ; Use: Retrieve the identification of the master locomotive.
      ;--------------------------------------------------------------
      
      ; (define/public (getMasterLocomotiveID)
      ;   (if (initialised?)
      ;       masterLocomotiveID
      ;       (error "Train% getMasterLocomotiveID: object is not initialised please initialise before use")))

      ;--------------------------------------------------------------
      ; Function: setRearLocomotive!
      ; Parameters:
      ;      object: object
      ;        Use: The locomotive object that needs to be set.
      ; Output: n/a
      ; Use: Setting a locomotive as rear locomotive.
      ;--------------------------------------------------------------

      ; (define/public (setRearLocomotiveID! id)
      ;   (if (symbol? id)
      ;       (if (not (member id trainBuild))
      ;           (set! rearLocomotiveID id)
      ;           (error "Train% setRearLocomotiveID!: locomotive is already part of the train buid."))
      ;       (error "Train setRearLocomotiveID!: contract violation expected symbol recieved" id)))

      ;--------------------------------------------------------------
      ; Function: getRearLocomotiveID
      ; Parameters: n/a
      ; Output:
      ;    rearLocomotiveID: symbol
      ;       Use: The identification of the rear locomotive.
      ; Use: Retrieving the identification of the rear locomotive.
      ;--------------------------------------------------------------

    ; (define/public (getRearLocomotiveID)
    ;   (if (initialised?)
    ;       rearLocomotiveID
    ;       (error "Train% getRearLocomotiveID: object not initialised please initialise before use")))

      ;---------------------------------------------------------------------------------------------------
      ; Function: deleteMember!
      ; Parameters:
      ;    id: symbol
      ;     Use: The identification of the to be deleted object.
      ; Output: n/a
      ; Use: Delete an identification of an object from the train build. The references are not adjusted.
      ;---------------------------------------------------------------------------------------------------

    ;  (define/public (deleteMember! id)
    ;    (if (initialised?)
    ;        (if (member id trainBuild) 
    ;            (begin(set! trainBuild (remove id trainBuild))
    ;              ;    (setLength! (- 1 length)) field length is needed to make this work
    ;;                 )
    ;           (error "Train% deleteMember!: object is not member of the train, it can not be deleted"))
    ;       (error "Train% deleteMember!: object is not initialised, please initialise before use.")))

      ;--------------------------------------------------------------
      ; Function: addMember!
      ; Parameters:
      ;     id: symbol
      ;       Use: The identification of the to be added object.
      ; Ouput: n/a
      ; Use: Add an identification of an object to the train build.
      ;--------------------------------------------------------------

      ; (define/public (addMember! id)
      ;   (if (initialised?)
      ;       (begin(set! trainBuild (append trainBuild id))
      ;           ;  (setLength! (- length 1)) field length is needed to make this work
      ;             )
      ;       (error "Train% addMember!: object is not initialised, please initialise before use.")))

      ;----------------------------------------------------------------------
      ; Function: isMember?
      ; Parameters:
      ;    id: symbol
      ;     Use: The identification of the member that needs to be checked.
      ; Use: Check whether or not an object is a member of the train.
      ;----------------------------------------------------------------------

    ; (define/public (isMember? id)
    ;   (if (and (symbol? id)
    ;            (initialised?))
    ;       (member id trainBuild)
    ;       (error "Train% isMember?: object is not initialised or contract violation, symbol expected received:"id)))

      ;--------------------------------------------------------------------
      ; Function: getBuild
      ; Parameters: n/a
      ; Output:
      ;    build: list
      ;      Use: The list containing the id's of the connected objects.
      ; Use: Retrieving the list of id's that make part of the train.
      ;--------------------------------------------------------------------
      
  ;   (define/public (getBuild)
  ;     (if (initialised?)
  ;         trainBuild
  ;         (error "Train% getBuild: object is not initialised please initialise before use.")))

      ;--------------------------------------------------------------
      ; Function: getLastID
      ; Parameters: n/a
      ; Output:
      ;     id: symbol
      ;      Use: The id of the last object conneted to the train.
      ; Use: Retrieve the id of the last connected object.
      ;--------------------------------------------------------------
      
    ; (define/public (getLastID)  ;field trainBuild is needed
    ;   (let get ([lst trainBuild])
    ;     (if (null? (cdr lst))
    ;         (car lst)
    ;         (get (cdr lst)))))

      ;---------------------------------------------------
      ; Function: setlength!
      ; Parameters:
      ;    number: number
      ;      Use: The length where it needs to be set to.
      ; Use: adjust the train's length.
      ;----------------------------------------------------
      
    ;  (define/private (setLength! number)
    ;    (set! length number)) field length is needed to make this work

      ))