#lang racket

(require racket/class)
(require typed-stack)

(provide SecurityProtocol%)

;--------------------------------------
; Class: SecurityProtocol%
; Parameters: n/a
; Output: object:SecurityProtocol%
; Use: Secure a given railway.
;--------------------------------------

(define SecurityProtocol%
  (class object%
    (super-new)

    ; Variables to enable checking types easyier. 

    (define blockType 'object:DetectionBlock%)
    (define switchType 'object:Switch%)
    (define trackType 'object:Track%)
    (define railManType 'object:RailwayManager%)

    ; The railway manager that's used (railway that needs to be secured)
    
    (field [railManager 'none])

    ;---------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;    boolean: boolean
    ;      Use: Boolean to determine if the object is initialised.
    ; Use: Check if the object is initialised.
    ;---------------------------------------------------------------
    
    (define/public (initialised?)
      (not (eq? railManager 'none)))

    ;------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;     manager: object:RailwayManager%
    ;       Use: The railway manager that needs to be assigned.
    ; Output: n/a
    ; Use: Assign a railway manager to the security protocol.
    ;------------------------------------------------------------

    (define/public (initialise! manager)
      (if (eq? (object-name manager) railManType)
          (set! railManager manager)
          (error "SecurityProtocol% initialise!: Contract violation expected RailwayManager% recieved" manager)))

    ;-----------------------------------------------------------------------------------
    ; Function: reserve!
    ; Parameters:
    ;     railObj: Detectionblock% or Switch% or Track%
    ;       Use: The railway object that needs to be reserved
    ;     trainID: symbol
    ;       Use: The identification of the train that wants to reserve the location.
    ; Output: n/a
    ; Use: Reserve the railway object for a specific train.
    ;-----------------------------------------------------------------------------------
    
    (define/private (reserve! railObj trainID)
      (if (initialised?)
      (if (send railManager isMember? (send railObj getID))
          (if (or(not(symbol? (send railObj getAvailable)))  ;If it is a symbol, it must be set on #f (not available)
                 (send railObj getAvailble))                 ;If the boolean is #t  the obj is available

              (if (and (not (send railManager isSwitch? (send railObj getID)))
                       (send railManager hasRelatedObject? (send railObj getID))) ;if it does not have a related object, no extra fetching needed

                  (begin
                    (send (send railManager getRelatedObject (send railObj getID)) setAvailable! trainID) ;if it is a detectionblock
                    (send railObj setAvailable! trainID))

                  (send railObj setAvailable! trainID))
              
              (error "SecurityProtocol% reserve!: Railway object is already reserved, cannot reserve the section"))
          (error "SecurityProtocol% reserve!: Contract violation, given railway object is not a switch, detectionblock or track"))
      (error "SecurityProtocol% reserve!: SecurityProtocol% is not intialised please initialise before use")))

    ;----------------------------------------------------------------------------------------
    ; Function: reserveSection!
    ; Parameters:
    ;      startBlock: object: DetectionBlock%
    ;         Use: The detectionblock where the traject starts.
    ;      endBlock: object: DetectionBlock%
    ;         Use: The detectionblock where the traject ends.
    ;      route: list<symbol>
    ;        Use: The list of identifications of the objects that need to be reserved.
    ;      trainID: symbol
    ;        Use: The identification of the train that needs the objects reserved.
    ; Output: n/a
    ; Use: Reserve a sequense of railway objects for a specific train with a specific route.
    ;----------------------------------------------------------------------------------------

    (define/public (reserveSection! startBlock endBlock route trainID)
      (if (initialised?)
      (if (and (eq? (object-name startBlock) blockType)
               (eq? (object-name endBlock) blockType)
               (list? route)
               (symbol? trainID)
               (eq? (object-name railManager) railManType))
          
          (if (not (null? list))
              (let ([resStack (empty-stack)] ; create an empty stack
                    [railObj 'none]) ; variable to save the railobject 
                (for ([i route])     ; In this part build the stack to enable correct reservation
                  (set! railObj (send railManager getObject i))
                  #:break (or (symbol? (send railObj getAvailable))   ; When a non available railobject is reached, end the loop.
                              (not (send railObj getAvailable)))

                  (push! resStack railObj))
                
                (if (eq? (send endBlock getID) (send (top resStack) getID)) ; compare the identification of the last pushed object with the end detectionblock
                    (print "route is completely reserved")
                    
                    (begin(for ([i resStack])
                            #:break  (and (eq? (object-name (send railManager getObject i)) trackType) ; in this case a detectionblock has been found.
                                          (send (send railManager getObject i) hasDetectionblock?))
                            (pop! resStack)) ; if it is not a detection block remove it form the stack
                          
                          (for ([e resStack])    ; The available elements are in the stack (between two detectionblocks)
                            (reserve!(send railManager getObject e)))
                          )))  ; clear the stack afther te operations
              
              (error "SecurityProtocol% reserveSection: Cannot reserve section, route is empty"))
          (error "SecurityProtocol% reserveSection!: Contract violation, expected detectionblock detecionblock list symbol RailwayManager% received" startBlock endBlock route trainID railManager))
      (error "SecurityProtocol% reserveSection!: SecurityProtocol% not initialised, please initialise before use")))

    ;---------------------------------------------------------------------
    ; Function: release!
    ; Parameters:
    ;     railObj: object:Detectionblock% object:switch object:track
    ;        Use: The object where the reservation needs to be removed.
    ; Output: n/a
    ; Use: Remove the reservation from the given object.
    ;---------------------------------------------------------------------
    
    (define/private (release! railObj)
      (if (initialised?)
      (let ([id (send railObj getID)])
        (if (or (send railManager isDetectionblock? id)
                (send railManager isTrack? id))
            
            (if (send railManager hasRelatedObject? id)
                (begin (send railObj setAvailable! #t)
                       (send (send railManager getRelatedObject id) setAvailable! #t))
                (send railObj setAvailable! #t))
            
            (if (send railManager isSwitch? id)
                (send railObj setAvailable! #t)
                (error "SecurityProtocol% release!: Given id does not belong to a railway object."))))
      (error "SecurityProtocol% release!: SecurityProtocol% not initialised, please initialise before use"))) 

    ;-----------------------------------------------------------------------------------------------
    ; Function: releaseSection!
    ; Parameters:
    ;      trainID: symbol
    ;         Use: The identification of the train that has reserved sections on the railroad.
    ;      route: list<symbol>
    ;         Use: The route or section of the route that the train has pased.
    ; Output: n/a
    ; Use: Remove the reservations set for the train.
    ;-----------------------------------------------------------------------------------------------

    (define/public (releaseSection! trainID route)
      (if (initialised?)
      (for ([i route])
        (let ([current (send railManager getObject i)])
          (if (eq? trainID(send current getAvailable))
              (release! current railManager)
              'couldNotRelease)))
      (error "SecurityProtocol% releaseSection!: SecurityProtocol% is not initialised, please initialise before use")))

    ;-------------------------------------------------------------------------------------
    ; Function: hashPermission?
    ; Parameters:
    ;      trainID: symbol
    ;        Use: The identification of the train who's permission needs to be checked.
    ;      detectionblockID: symbol
    ;        Use: The detctionblock where the permission needs to be checked.
    ; Output:
    ;      boolean: boolean
    ;        Use: Determine if the train has permission to pass the detectionblock.
    ; Use: Check if the train has permission to pass the detectionblock.
    ;--------------------------------------------------------------------------------------

    (define/public (hasPermission? trainID detectionblockID)
      (if (initialised?)
          (if (and (symbol? trainID)
                   (symbol? detectionblockID))
              (if (send railManager isDetectionblock? detectionblockID)
                  (eq? trainID (send (send railManager getObject detectionblockID) getAvailable))
                  (error "SecurityProtocol% hasPremission?: detectionblockID does not belong to a detectionblock recieved" detectionblockID))
              (error "SecurityProtocol% hasPremission?: Contract violaton, expected symbol symbol recieved" trainID detectionblockID))
          (error "SecurityProtocol% hasPremission?: SecurityProtocol% is not initialised, please initialse before use")))
                            

    ))