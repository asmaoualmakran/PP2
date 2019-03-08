#lang racket

(require racket/class)
(require typed-stack)
(require "switch.rkt")
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
    
    (define/private (reserve! railObj trainID)
      (if (or (eq? (object-name railObj) blockType)
              (eq? (object-name railObj) switchType)
              (eq? (object-name railObj) trackType))
          (if (or(not(symbol? (send railObj getAvailable)))  ;If it is a symbol, it must be set on #f (not available)
                 (send railObj getAvailble))                 ;If the boolean is #t  the obj is available             
              (send railObj setAvailable! trainID)
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
                      #:break (eq? (object-name (top resStack)) blockType) ; when finding a detectionblock on the stack, stop the iteration
                      (pop! resStack))
                    (pop-all! resStack))))  ;clear the stack afther te operations
              (error "SecurityProtocol% reserveSection: Cannot reserve section, route is empty"))
          (error "SecurityProtocol% reserveSection!: Contract violation, expected detectionblock detecionblock list symbol RailwayManager% received" startBlock endBlock route trainID railManager)))

   

    (define/private (release! railObj)
      (send railObj setAvailable! #t))
    

    ))