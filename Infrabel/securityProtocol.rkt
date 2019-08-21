#lang racket

(require racket/class)
(require typed-stack)

(require "railwayController.rkt")
(require "railwaySystem.rkt")


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
    (define railConType 'object:RailwayController%)
   (define railSysType 'object:RailwaySystem%)

   
    
    (field  [railwayController  'uninitialised]
            [railwaySystem      'uninitialised]
            [drivingSpeed       '100]
            [stoppingSpeed      '0]
            [reservedSections (make-hash)])

    ;---------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;    boolean: boolean
    ;      Use: Boolean to determine if the object is initialised.
    ; Use: Check if the object is initialised.
    ;---------------------------------------------------------------
    
    (define/public (initialised?)
           (not (eq? railwayController 'none)))

    ;------------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;     
    ; Output: n/a
    ; Use: Assign a railway manager to the security protocol.
    ;------------------------------------------------------------

    (define/public (initialise! railsys railcon)
      (if (and (eq? (object-name railsys) railSysType)
               (eq? (object-name railcon) railConType))
        (begin (set! railwaySystem railsys)
               (set! railwayController railcon))
          (error "SecurityProtocol% initialise!: Contract violation expected RailwaySystem%" )))

    ;-------------------------------------------------------------------------------------
    ; Function: takeSegment
    ; Parameters: 
    ;         symbol: loc 
    ;           Use: The starting location for the reservation. 
    ;         route: list<symbol> 
    ;           Use: The route where the segment needs to be taken. 
    ; Output: 
    ;       segment: list<symbol> 
    ;          Use: The calculated segment. 
    ; Use: Take a segment, starting from loc until the next detectionblock in the route. 
    ;-------------------------------------------------------------------------------------

    (define/private (takeSegment loc route)
      (if (and (send railwayController isDetection? loc)
               (list? route)
               (not (null? route)))
      (let* ((track (send railwayController getTrack loc))
             (idx (index-of route track))
             (segment (list )))
          (for ([i (in-range idx (length route))])
                #:final (send railwayController isDetection? (list-ref route i))
                (set! segment (append segment (list (list-ref route i)))))
                (display "segment is ")
                (display segment)
                (newline)
                segment)
                
      (error "SecurityProtocol% takeSegment: Contract violation location is not a detectionblock or the list is empty")))

    ;-----------------------------------------------------
    ; Function: getNext
    ; Parameters: 
    ;         loc: symbol
    ;           Use: A location in the route. 
    ;         route: list<symbol> 
    ;           Use: A calculated route. 
    ; Output: 
    ;       next: symbol
    ;         Use: The location next to loc
    ; Use: Get the object next to loc in the route. 
    ;-----------------------------------------------------

    (define/private (getNext loc route)
      (if (member loc route)
        (let* ((idx (index-of route loc))
               (next (list-ref route (+ idx 1))))
        next)
        (error "SecurityProtocol% getNext: Given location does not belong to the route, recieved " route))) 
    
    ;-----------------------------------------------------------------------------
    ; Function: getSimulatorStance
    ; Parameters: 
    ;       switchID: symbol
    ;         Use: The switch who's stance needs to be checked. 
    ;  Output: 
    ;       stance: number
    ;         Use: The stance of the switch expressed in a number. 
    ;  Use: The switch stance to be used to change the stance in the simulator. 
    ;-----------------------------------------------------------------------------

    (define/private (getSimulatorStance switchID)
      (if (send railwayController isSwitch? switchID)
          (cadr(send railwayController getSwitchStance switchID))
      (error "SecurityProtocol% getHardwareStance: Given id does not belong to a switch, recieved" switchID)))    

    ;----------------------------------------------------------------------------------------
    ; Function: endLocation? 
    ; Parameters: 
    ;         trainID: symbol
    ;           Use: The train where needs to be checked if it reached it's destination. 
    ; Output: 
    ;       boolean: boolean
    ;         Use: Determine if the train reached it's end location. 
    ; Use: Determine if the train reached it's end location. 
    ;---------------------------------------------------------------------------------------

    (define/private (endLocation? trainID)
      (let* ((route (send railwayController getActiveRoute trainID))
             (lastDB (send railwayController getBlock (last route)))
             (location #f))

              (set! location (send railwaySystem getTrainLocation trainID))

             (eq? lastDB location)))

    ;-----------------------------------------------------------------------
    ; Function: endSegment? 
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: The train who's end of segment needs to be checked. 
    ;       segment: list<symbol> 
    ;         Use: The segment reserved by the train. 
    ; Output: 
    ;      boolean: boolean
    ;         Use: Determine if the end of the segment is reached. 
    ; Use: Determine if the end of the segment is reached. 
    ;-----------------------------------------------------------------------

    (define/private (endSegment? trainID segment)
      (let ((lastDB (send railwayController  getBlock (last segment))))
      
        (eq? lastDB (send railwaySystem getTrainLocation trainID))))

    ;---------------------------------------------------------------------------------------
    ; Function: endTraject! 
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: The train who's traject needs to be ended. 
    ; Output: n/a 
    ; Use: End a train's traject and remove the train from the active trains and trajects. 
    ;---------------------------------------------------------------------------------------

    (define/private (endTraject! trainID)
      (when (endLocation? trainID)
            (send railwaySystem setTrainSpeed! trainID stoppingSpeed)
            (send railwayController deactivateRoute! trainID)))

    ;----------------------------------------------------------------------
    ; Function: reserveSection!
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: The train wherefor the section needs to be reserved. 
    ;       section: list<symbol>
    ;         Use: Section to be reserved for the train.
    ; Output: n/a 
    ; Use: Reserve a section for a train. 
    ;----------------------------------------------------------------------

    (define/private (reserveSection! trainID section)
      (if (symbol? trainID)
        (if (and (list? section)
                  (not (null? section)))

          (for ([i section])

             (if (send railwayController isTrack? i)
                (cond ((send railwayController hasDetectionblock? i)  
                                                                      (send railwayController reserve! i trainID)
                                                                     (send railwayController reserve! (send railwayController getBlock i) trainID))
                (else  (send railwayController reserve! i trainID)))
             (send railwayController reserve! i trainID)) )
        (error "SecurityProtocol% reserveSection!: Contract violation expected a non empty list, recieved: " section))
      (error "SecurityProtocol% reserveSection!: Contract violation expected a symbol, recieved: " trainID))
      
              (display "section reserved ")
              (display section)
              (display " ")
              (display (car section))
              (display "-")
              (display (send railwayController getReservation (car section))) 
              (newline))

    ;----------------------------------------------------------------------------------
    ; Function: releaseSection! 
    ; Parameters: 
    ;         trainID: symbol
    ;           Use: The id of the train that wants to release it's reseved section. 
    ;         section: symbol
    ;           Use: The section to be released. 
    ; Output: n/a 
    ; Use: Release the section reserved by the given train. 
    ;----------------------------------------------------------------------------------
    
    (define/private (releaseSection! trainID section)
      (if (symbol? trainID)
        (if (and (list? section)
                 (not (null? section)))

        (for ([i section])
              (cond ((send railwayController isTrack? i )(if (send railwayController hasDetectionblock? i)
                                      (begin 
                                        (send railwayController release! i trainID)
                                        (send railwayController release! (send railwayController getBlock i) trainID))
                                      (send railwayController release! i trainID)))
              (else (send railwayController reserve! i trainID))))
      
        (error "SecurityProtocol% releaseSection!: Contract violation expected a non empty list, recieved: " section))
      (error "SecurtiyProtocol% releaseSection!: Contract violation expected a symbol, recieved: " trainID))
                      (display "train ")
        (display trainID)
        (newline)
              (display "section released ")
              (display section)
              (display " ")
              (display (car section))
              (display "-")
              (display (send railwayController getReservation (car section)))
              (newline) )

    ;-----------------------------------------------------------------------
    ; Function: setSwitchesSection!
    ; Parameters: 
    ;       section: list<symbol>
    ;         Use: The section who's switch stance needs to be altered. 
    ; Output: n/a 
    ; Use: Get the correct placement of the switches for a given section. 
    ;-----------------------------------------------------------------------

    (define/private (setSwitchesSection! section)
      (for ([i section])
          (when (send railwayController isSwitch? i)                           
            (when (send railwayController memberYConnection? i (getNext i section))
                  (send railwayController setStance! i (getNext i section))
                  (send railwaySystem setSwitchPosition! i (getSimulatorStance i))))))

    ;-------------------------------------------------------------
    ; Function: sectionAvailable? 
    ; Parameters: 
    ;       section: list<symbol> 
    ;         Use: The section that needs to be checked. 
    ; Output: 
    ;      boolean: boolean
    ;       Use: Determine if the section is available. 
    ; Use: Determine is a section is avilable for reservation. 
    ;-------------------------------------------------------------

    (define/private (sectionAvailable? section)

      (memf (lambda (elm)
              (eq? #t elm)) (map (lambda (elm)
                                    (send railwayController getReservation elm ))section)))

    ;------------------------------------
    ; Function: protocol
    ; Parameters: n/a 
    ; Output: n/a 
    ; Use: The security protocol used. 
    ;------------------------------------
  
    (define/public (protocol)
      (when (initialised?)
        
        (let ([activeTrains (list )])

          (set! activeTrains (send railwayController getActiveTrains))
       
          (when (not (null? activeTrains))

            (for ([t activeTrains])

              (let  ([trainLoc (send railwaySystem getTrainLocation t)]
                     [reserved (when (hash-has-key? reservedSections t)(hash-ref reservedSections t))]
                     [route    (send railwayController getActiveRoute t)]
                     [segment  (list ) ])

              (when (hash-has-key? reservedSections t)                     ;there is already an reserved segment
 
                (cond 
                      ((endLocation? t) (send railwaySystem setTrainSpeed! t stoppingSpeed)
                                        (endTraject! t)                       ; the train has reached it's end location
                                        (releaseSection! t reserved)

                                        (send railwayController reserve! trainLoc t)

                                        (hash-remove! reservedSections t))

                     ((endSegment? t reserved) (set! segment (takeSegment trainLoc route))
                                          (if (sectionAvailable? segment)
                                              (begin 
                                                (reserveSection! t segment)
                                                (setSwitchesSection! segment)
                                                (releaseSection! t reserved)
                                                (hash-set! reservedSections t segment))
                                              (begin
                                                (send railwaySystem setTrainSpeed! t stoppingSpeed)
                                                (releaseSection! t reserved)

                                                (send railwayController reserve! trainLoc t))))))

              (unless (hash-has-key? reservedSections t) ; there is no reserved segments for the train

                (set! segment (takeSegment trainLoc route))
 
                (when (sectionAvailable? segment)

                          (reserveSection! t segment)
                          (setSwitchesSection! segment)
                          (hash-set! reservedSections t segment)
                          (send railwaySystem setTrainSpeed! t drivingSpeed)
                          )))))
                  
            (protocol)))
        (protocol))


        




    
                  

    ))