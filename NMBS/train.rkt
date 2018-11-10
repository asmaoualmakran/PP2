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

    (define locomotiveType 'obeject:Locomotive%)
    (define railcarType 'object:Railcar%)

    
    (define/public (initialised?)
      (and (not(eq? ID 'uninitialised))
           (not(eq? trainBuild 'uninitialised))
           (not(eq? traject 'uninitialised))
           (not(eq? masterLocomotiveID 'uninitialised))
           (not(eq? rearLocomotiveID 'uninitialised))
           (not(eq? length 'uninitialised))
           (not(eq? currentPosition 'uninitialised))
           (not(eq? lastPosition 'uninitialised))
           (not(eq? direction 'uninitialised))
           (not(eq? speed 'uninitialised))
           (not(eq? currentNode 'uninitialised))
           (not(eq? nextNode 'uninitialised))))


    (define/public (setID! id)
      (if (and (symbol? id)
               (eq? ID 'uninitialised))
          (set! ID id)
          (error "Train% setID!: ID already initialised or contract violation expected symbol received" id ID)))

    (define/public (getID)
      (if (initialised?)
          ID
          (error "Train% getID: object not initialised please initialise before use.")))

    (define/public (valid?)
      (and(initialised?)
          (member masterLocomotiveID trainBuild)
          (member rearLocomotiveID trainBuild)))

    (define/public (setMasterLocomotiveID! object)
      (if (object? object)
      (if (eq? (object-name object) locomotiveType)   ; check the type of the object
          (if (member (send object getID) trainBuild) ; check if the object is a member of the train
              (set! masterLocomotiveID (send object getID)) 
              (error "Train% setMasterLocomotive!: trainBuild does not contain the locomotive, please add before assigning"))
          (error "Train% setMasterLocomotive!: contract violation object has not the type Locomotive% received" object))
      (error "Train setMasterLocomotive!: contract violation expected Train% object received" object)))

    (define/public (getMasterLocomotiveID)
      (if (initialised?)
          masterLocomotiveID
          (error "Train% getMasterLocomotiveID: object is not initialised please initialise before use")))

    (define/public (setRearLocomotive! object)
      (if (object? object)
      (if (eq? (object-name object) locomotiveType)   ; check the type of the object
          (if (member (send object getID) trainBuild) ; check if the object is a member of the train
              (set! rearLocomotiveID (send object getID)) 
              (error "Train% setRearLocomotiveID!: trainBuild does not contain the locomotive, please add before assigning"))
          (error "Train% setRearLocomotiveID!: contract violation object has not the type Locomotive% received" object))
      (error "Train setRearLocomotiveID!: contract violation expected Train% object received" object)))
      

    (define/public (getRearLocomotiveID)
      (if (initialised?)
          rearLocomotiveID
          (error "Train% getRearLocomotiveID: object not initialised please initialise before use")))


;    (define/public (initLocomotive)
      

 ;   (define/public (initRailcar)
      

 ;   (define/public (create)
      

    (define/public (couple! object)
      'test)

    (define/public (decouple! object)
      'test)

    (define/public (setTraject! traj)
      (if (vector? traj)
          (set! traject traj)
          (error "Train% setTraject: contract violation, expected vector received" traj)))

    (define/public (removeTraject!)
      (if (initialised?)
          (set! traject 'none)
          (error "Train% removeTraject!: object is not initialised please initialise before use")))

    (define/public (getTraject)
      (if (initialised?)
          (traject)
          (error "Train% getTraject: object is not initialised please initialise before use")))

    (define/public (setCurrentPosition! pos)
      'test)

    (define/public (setCurrentNode! node)
      'test)

    (define/public (setDirection!)
      'test)

    (define/public (getDirection)
      'test)

    (define/public (setSpeed! number)
      'test)

    (define/public (getSpeed)
      'test)
    
    
              
    
    

 
    


    

    ))