#lang racket

(require racket/class)
(require typed-stack)
(require "switch.rkt")
(require "track.rkt")
(require "detectionBlock.rkt")
(require "infrastructure.rkt")

(provide SecurityProtocol%)

(define SecurityProtocol%
  (class object%
    (super-new)
    

    (define blockType 'object:DetectionBlock%)
    (define switchType 'object:Switch%)
    (define trackType 'object:Track%)
    (define railManType 'object:RailwayManager%)


  

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
    
    (define/private (reserve! railObj trainID railManager)
      (if (send railManager isMember? (send railObj getID))
          (if (or(not(symbol? (send railObj getAvailable)))  ;If it is a symbol, it must be set on #f (not available)
                 (send railObj getAvailble))                 ;If the boolean is #t  the obj is available

              (if (and (not (send railManager isSwitch? (send railObj getID)))
                       (send railManager hasRelatedObject? (send railObj getID))) ;if it does not have a related object, no extra fetching needed

                  (begin(send (send railManager getRelatedObject (send railObj getID)) setAvailable! trainID) ;if it is a detectionblock
                  (send railObj setAvailable! trainID))

                  (send railObj setAvailable! trainID))
              
              (error "SecurityProtocol% reserve!: Railway object is already reserved, cannot reserve the section"))
          (error "SecurityProtocol% reserve!: Contract violation, given railway object is not a switch, detectionblock or track")))

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
    ;      railManager: RailwayManager%
    ;        Use: The manager that constains all the railway objects.
    ; Output: n/a
    ; Use: Reserve a sequense of railway objects for a specific train with a specific route.
    ;----------------------------------------------------------------------------------------

    (define/public (reserveSection! startBlock endBlock route trainID railManager)
      (if (and (eq? (object-name startBlock) blockType)
               (eq? (object-name endBlock) blockType)
               (list? route)
               (symbol? trainID)
               (eq? (object-name railManager) railManType))
          
          (if (not (null? list))
              (let ([resStack (empty-stack)] ; create an empty stack
                    [railObj 'none]) ; variable to save the railobject 
                (for ([i route])     ;In this part build the stack to enable correct reservation
                  (set! railObj (send railManager getObject i))
                  #:break (or (symbol? (send railObj getAvailable))   ;When a non available railobject is reached, end the loop.
                              (not (send railObj getAvailable)))

                  (push! resStack railObj))
                
                (if (eq? (send endBlock getID) (send (top resStack) getID)) ;compare the identification of the last pushed object with the end detectionblock
                    (print "route is completely reserverd")
                    
                    (begin(for ([i resStack])
                            #:break  (and (eq? (object-name (send railManager getObject i)) trackType) ;in this case a detectionblock has been found.
                                          (send (send railManager getObject i) hasDetectionblock?))
                            (pop! resStack)) ;if it is not a detection block remove it form the stack
                          
                          (for ([e resStack])    ;The available elements are in the stack (between two detectionblocks)
                            (reserve!(send railManager getObject e)))
                          )))  ;clear the stack afther te operations
              
              (error "SecurityProtocol% reserveSection: Cannot reserve section, route is empty"))
          (error "SecurityProtocol% reserveSection!: Contract violation, expected detectionblock detecionblock list symbol RailwayManager% received" startBlock endBlock route trainID railManager)))

   

    ;---------------------------------------------------------------------
    ; Function: release!
    ; Parameters:
    ;     railObj: object:Detectionblock% object:switch object:track
    ;        Use: The object where the reservation needs to be removed.
    ; Output: n/a
    ; Use: Remove the reservation from the given object.
    ;---------------------------------------------------------------------
    
    (define/private (release! railObj)
      (send railObj setAvailable! #t))

    ;-----------------------------------------------------------------------------------------------
    ; Function: releaseSection!
    ; Parameters:
    ;      trainID: symbol
    ;         Use: The identification of the train that has reserved sections on the railroad.
    ;      route: list<symbol>
    ;         Use: The route or section of the route that the train has pased.
    ;      railmanager: object: RailwayManager%
    ;         Use: The railwaymanager that contains all the railway objects.
    ; Output: n/a
    ; Use: Remove the reservations set for the train.
    ;-----------------------------------------------------------------------------------------------

    ;TODO: make sure when you have a track, the detecionblock is also released

    (define/public (releaseSection! trainID route railmanager)
      (for ([i route])
        (let ([object (send railmanager getObject i)])
          (cond ((eq? (send object getAvailable ) trainID) (send object setAvailable! #t)) ;cond used instead of if, when condition is false nothing must happen.
                ))))
                   

    ))