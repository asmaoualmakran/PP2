#lang racket

(require "railwayManager.rkt")

(provide RailwayController%)
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
    (define securityType 'object:SecurityProtocol%)

    (field [railwayManager 'uninitialised])

    ;---------------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;      Use: Boolean to determine if the object is initialised or not.
    ; Use: Determine if the object is initialised or not.
    ;---------------------------------------------------------------------

    (define/public (initialised?)
      (not (eq? railwayManager 'uninitialised)))

    ;-----------------------------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;         manager: object:RailwayManager
    ;          Use: The railway manager that is used to represent the railway system.
    ;         secure: object:SecurityProtocol%
    ;           Use: The security protocol to be used
    ; Output: n/a
    ; Use: Initialise the object.
    ;-----------------------------------------------------------------------------------
    
    (define/public (initialise! manager)
      (if (not (initialised?))
          (if (eq? (object-name manager) managerType)
              (set! railwayManager manager)
              (error "RailwayController% initialise!: Contract violation, expected a object with type RailwayManager%, recieved:" manager))
          (error "RailwayController% initialise!: Object is already initialised, it cannot be reinitialised.")))

    ;-------------------------------------------------------------------------
    ; Function: setStance! 
    ; Parameters: 
    ;       switchID: symbol
    ;         Use: The id of the switch who's stance needs to be changed. 
    ;       id: symbol
    ;         Use: The id to where the stance needs to be changed. 
    ; Output: n/a 
    ; Use: Change the stance of the switch. 
    ;-------------------------------------------------------------------------

    (define/public (setStance! switchID id)
    (if (and (symbol? id)
              (symbol? switchID))
      (if (send railwayManager isSwitch? switchID) 
            (send (send railwayManager getObject switchID) setState! id)
          (error "RailwayController% setStance!: Given switchID does not belong to a switch, recieved " switchID))
          
        (error "RailwayController% setStance!: Given switchID is not a symbol, recieved" switchID)))

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

    ;-------------------------------------------------------------------------------------
    ; Function: memberYConnection? 
    ; Parameters: 
    ;       swicthID: symbol
    ;         Use: The switch who's connection needs to be checked. 
    ;       id: symbol
    ;         Use: The to be checked connection of the switch. 
    ; Output: 
    ;       boolean: boolean
    ;         Use: Determine if the given connection is connected to the splitted part. 
    ; Use: Determine if the given connection is connected to the splitted part. 
    ;-------------------------------------------------------------------------------------

   (define/public (memberYConnection? switchID id)
    (if (send railwayManager isSwitch? switchID)
      (let* ((switch (send railwayManager getSwitch switchID))
             (yConnection (send switch getYConnection)))
          (member id yConnection))
    (error "RailwayController% memberYConnection: Given id does not belong to a switch")))
   
   ;------------------------------------------------------------
   ; Function: connection
   ; Parameters: 
   ;        switchID: symbol
   ;          Use: The switch who's connections arw needed. 
   ; Output: 
   ;        connections: list<symbol>
   ;          Use: Get all the connections of the switch. 
   ;------------------------------------------------------------

   (define/public (connection switchID )
    (send (send railwayManager getSwitch switchID) getConnections))

    ;------------------------------------------------------------------------------------------
    ; Function: getTrack
    ; Parameters: 
    ;         id: symbol
    ;          Use: The id of the detection block who's track needs to be fetched.
    ; Output: 
    ;       detectionblock: object:Detectionblock%
    ; Use: Get the id of the detection block related to the track.
    ;------------------------------------------------------------------------------------------ 

    (define/public (getTrack id)
      (if (initialised?)
        (send railwayManager getRelatedID id)
      (error "RailwayController% getTrack: object is not initialised, please initialse before use.")))

    ;-----------------------------------------------------------------------------------------
    ; Function: getBlock
    ; Parameters: 
    ;         id: symbol
    ;          Use: The id of the detection block who's track id needs to be fetched.
    ; Output: 
    ;       detectionblock: object:Detectionblock%
    ; Use: Get the id of the track related to the detection block.
    ;------------------------------------------------------------------------------------------ 
   
    (define/public (getBlock id)
      (if (initialised?)
        (send railwayManager getRelatedID id)
      (error "RailwayController% getBlock: Object is not initialised, please initialse before use.")))

    ;----------------------------------------------------------------------
    ; Function: isDetectionblock?
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the object that needs checking.
    ; Output:
    ;     boolean: boolean
    ;       Use: Determine whether or not the object is a detectionblock
    ;----------------------------------------------------------------------

    (define/public (isDetection? id)
      (if (initialised?)
        (send railwayManager isDetectionblock? id)
      (error "RailwayController% isDetection?: Object is not initialised, please initialise before use.")))

    ;----------------------------------------------------------
    ; Function: isSwitch? 
    ; Parameters: 
    ;         id: symbol
    ;          Use: The ID of the to be checked object. 
    ; Output: 
    ;       boolean: boolean
    ;          Use: Determine if the id belongs to a switch. 
    ; Use: Determine if the given id belongs to a switch. 
    ;----------------------------------------------------------

    (define/public (isSwitch? id)
      (if (initialised?)
        (send railwayManager isSwitch? id)
        (error "RailwayController% isSwitch?: Object is not initialised, please initialise before use.")))
   
    ;----------------------------------------------------------------------
    ; Function: isTrack? 
    ; Parameters: 
    ;         id: symbol
    ;           Use: The object id that needs to be checked. 
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if the object id belongs to a track object. 
    ; Use: Determine if a given object id belongs to a track object. 
    ;----------------------------------------------------------------------

    (define/public (isTrack? id)
      (if (initialised?)
        (send railwayManager isTrack? id)
        (error "RailwayController% isTrack?: Object is not initialised, please initialise before use.")))

    ;--------------------------------------------------------------
    ; Function: hasDetectionblock? 
    ; Parameters: 
    ;         id: symbol
    ;           Use: The identification of the track
    ; Output: 
    ;       boolean: boolean 
    ;         Use: Determine if the track has a detectionblock. 
    ; Use: Determine if a track has a detectionblock. 
    ;--------------------------------------------------------------

    (define/public (hasDetectionblock? id)
      (if (isTrack? id)
        (send (send railwayManager getTrack id) hasDetectionblock?)
      (error "RailwayController: expected a track id recieved: " id)))

    ;-------------------------------------------------------------
    ; Function: getActiveRoute
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: The train who's route needs to be fetched. 
    ; Output: 
    ;     route: list<symbol> 
    ;       Use: The route that is driven by the given train. 
    ; Use: Getting an active route. 
    ;--------------------------------------------------------------

    (define/public (getActiveRoute trainID)
      (if (initialised?)
        (send railwayManager getActiveRoute trainID)
      (error "RailwayController% getActiveRoute: Object is not initialised, please initialse before use.")))

    ;----------------------------------------------------------
    ; Function: getActiveTrains
    ; Parameters: n/a 
    ; Output: 
    ;     trains: list<symbol>
    ;       Use: The trains that are active on that moment. 
    ; Use: Get the trains that are active on that moment. 
    ;----------------------------------------------------------

    (define/public (getActiveTrains)
      (if (initialised?)
        (send railwayManager getActiveTrains)
      (error "RailwayController% getActiveTrains: Object is not initialised, please initialise before use.")))

    ;------------------------------------------------------------------------
    ; Function: deactivateRoute!
    ; Parameters: n/a 
    ; Output: n/a 
    ; Use: Remove a route and a train from the active list and hash-table. 
    ;------------------------------------------------------------------------

    (define/public (deactivateRoute! trainID)
      (send railwayManager deactivateRoute! trainID))
  
    ;---------------------------------------------------------
    ; Function: reserve! 
    ; Parameters: 
    ;         objID: symbol
    ;           Use: The object that needs to be reserved. 
    ;         trainID: symbol
    ;           Use: The train that's reserving the object. 
    ; Output: n/a 
    ; Use: Reserve a section for a given train. 
    ;---------------------------------------------------------

    (define/public (reserve! objID trainID)
      (if (and (symbol? trainID)
               (symbol? objID))
          (when (eq? #t (getReservation objID))           ; you cannot reserve a reserved object
            (send railwayManager setStatus! objID trainID))
      (error "RailwayController% reserve!: Contract violation expected two symbols, recieved: " objID trainID)))

    ;----------------------------------------------------------
    ; Function: release! 
    ; Parameters: 
    ;         objID: symbol
    ;           Use: The object that needs to be released.
    ;         trainID: symbol
    ;           Use: The train reserving the object.  
    ; Output: n/a 
    ; Use: Remove a reservation. 
    ;----------------------------------------------------------

    (define/public (release! objID trainID)
      (if (and (symbol? objID)
               (symbol? trainID))
          (when (eq? trainID (getReservation objID))
                (send railwayManager setStatus! objID #t))
      (error "RailwayController% release!: Contract violation expected a symbol, recieved: " objID)))

    ;------------------------------------------------------------------------------------------------
    ; Function: getReservation
    ; Parameters: 
    ;         objID: symbol
    ;           Use: The object who's reservation needs to be get. 
    ; Output: 
    ;       reservation: symbol || booelan
    ;           Use: The reservation status of an object. 
    ; Use: Getting the reservation of the object, symbol is returned when reserved, #t when free.
    ;------------------------------------------------------------------------------------------------

    (define/public (getReservation objID)
      (if (symbol? objID)
        (send railwayManager getStatus objID)
      (error "RailwayController% getReservation: Contract violation expected a symbol, recieved: " objID)))

    ))