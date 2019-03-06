#lang racket

(require racket/class)
(require "switch.rkt")
(require "detectionBlock.rkt")
(require "infrastructure.rkt")

(provide SecurityProtocol%)

(define SecurityProtocol%
  (class object%
    (super-new)
    

    (define blockObj 'object:DetectionBlock%)
    (define switchObj 'object:Switch%)
    (define trackObj 'object:Track%)

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
      (if (or (eq? (object-name railObj) blockObj)
              (eq? (object-name railObj) switchObj)
              (eq? (object-name railObj) trackObj))
          (if (or(not(symbol? (send railObj getAvailable)))  ;If it is a symbol, it must be set on #f (not available)
                 (send railObj getAvailble))                 ;If the boolean is #t  the obj is available             
              (send railObj setAvailable! trainID)
              (error "SecurityProtocol% reserve!: Railway object is already reserved, cannot reserve the section"))
          (error "SecurityProtocol% reserve!: Contract violation, given railway object is not a switch, detectionblock or track")))

    ;----------------------------------------------------------------------------------------
    ; Function: reserveSection!
    ; Parameters:
    ;     startBlockObj: Detectionblock%
    ;       Use: The detectionblock where the reservations start.
    ;     endBlockObj: Detectionblock%
    ;       Use: The detectionblock where the reservations end.
    ;     route: list<symbol>
    ;       Use: The route that needs to be followed
    ;     trainID: symbol
    ;       Use: The identifiaction of the train of which the reservation is needed for.
    ;     railway: RailwayManager%
    ;       Use: The railway manager that contains all the railway objects.
    ; Output: n/a
    ; Use: Reserve successive railway objects for a given train on a given route.
    ;----------------------------------------------------------------------------------------
    

    (define/public (reserveSection! startBlockObj endBlockObj route trainID railway)
      
      (if (list? route)
          (if (not(empty? route))
              (if (and (memq startBlockObj route)
                       (memq endBlockObj route))
         
                  (let ([connection 'none]
                        [interNode 'none])
                    (send startBlockObj setAvailable! trainID)  ;Block the two endpoints
                    (send endBlockObj setAvailable! trainID)
                    (for ([node route]
                          #:break (or (symbol? (send node getAvailable))
                                      (not (send node getAvailable))))  ;if the availablity contains #f or a symbol, it needs to break
                      (set! interNode node)        ;save the node you passed in the intration
                      (send (send railway getObject node) setAvailable! trainID))
                    interNode
                    (if (eq? interNode endBlockObj) ;afther breaking or completing the loop, retrun the intermeadiate result
                        (print "Route is completely reserved")
                        (print "Warning route is not completely reserved, last node was" interNode)))
                  (error "SecurityProtocol% reserveSection!: Given route does not contain the start block and or end block"))
              (error "SecurityProtocol% reserveSection!: Given route is an empty list"))       
          (error "SecurityProtocol% reserveSection!: Given route is not a list")))

    (define/public (release! railObj)
      (send railObj setAvailable! #t)
      )
    

    ))