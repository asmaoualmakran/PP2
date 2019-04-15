#lang racket

(require "railwayManager.rkt")

;-----------------------------------------------------------
; Class: RailwayController%
; Parameters: n/a
; Output: object:RailwayController%
; Use: Controll the railway objects and check their status.
;-----------------------------------------------------------

(define RailwayController%
  (class object%
    (super-new)

    (define managerType 'object:RailwayManager%)

    (field [railwayManager 'none])

    ;---------------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;      Use: Boolean to determine if the object is initialised or not.
    ; Use: Determine if the object is initialised or not.
    ;---------------------------------------------------------------------

    (define/public (initialised?)
      (not (eq? railwayManager 'none)))

    ;-----------------------------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;         manager: object:RailwayManager
    ;          Use: The railway manager that is used to represent the railway system.
    ; Output: n/a
    ; Use: Initialise the object.
    ;-----------------------------------------------------------------------------------
    
    (define/public (initialise! manager)
      (if (not (initialised?))
          (if (eq? (object-name manager) managerType)
              (set! railwayManager manager)
              (error "RailwayController% initialise!: Contract violation, expected a object with type RailwayManager%, recieved:" manager))
          (error "RailwayController% initialise!: Object is already initialised, it cannot be reinitialised.")))

    ;-------------------------------------------------------------------------------------
    ; Function: getSwitchStance
    ; Parameters:
    ;        switchID: symbol
    ;           Use: The identification of the switch who's stance needs to be checked
    ; Output:
    ;       stance: symbol
    ;          Use: The identification of the track where the switch is directed to.
    ; Use: Check in which direction the switch is, and which tracks it's connecting.
    ;-------------------------------------------------------------------------------------

    (define/public (getSwitchStance switchID)
      (if (initialised?)
          (if (send railwayManager isSwitch? switchID)
              (send (send railwayManager getSwitch switchID) getState)
              (error "RailwayController% getSwitchStance: RailwayManager% does not have a switch with ID:" switchID))
          (error "RailwayController% getSwitchStance: Object is not initialised, please initialise before use.")))

    ;-----------------------------------------------------------------------------------
    ; Function: switch!
    ; Parameters:
    ;        switchID: symbol
    ;         Use: The identification of the switch who's stance needs to be changed.
    ; Output: n/a
    ; Use: Change the stance of the switch.
    ;-----------------------------------------------------------------------------------
    
    (define/public (swtich! switchID)
      (if (initialised?)
          (if (send railwayManager isSwitch?)
              'MakeTheSWITCHswitch
              (error "RailwayController% switch!: Given switchId does not belong to a switch, recieved:" switchID))
          (error "RailwayController% switch!: Object is not initialised, please initialise before use.")))

    (define/public (getTrain detectionID)
      'test)

    (define/public (givePermission trainID)
      'test)

    (define/public (setSpeed! trainID)
      'test)

    (define/public (getSpeed trainID)
      'test)

    (define/public (getTrainDir trainID)
      'test)

    (define/public (correctDir? trainID route)
      'test)
    
    
    (define/public (hasPermission? trainID detectionID)
      'test)

    (define/public (updateTrainLoc! detectionID)
      'test)
    

    ))