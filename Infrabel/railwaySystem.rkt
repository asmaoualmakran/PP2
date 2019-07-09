#lang racket 

(require racket/class)
(require (prefix-in Z21: "../libs/interface/interface.rkt")) ; The Z21 interface
(require "../libs/simulator/gui_simulator/interface.rkt")

(provide RailwaySystem%)

;------------------------------------------------------------------------------------------------
; Class: RailwaySystem%
; Parameters: n/a 
; Output: object:RailwaySystem%
; Use: Abstraction for the simulator and the Z21 library enables easy switching between the two.
;------------------------------------------------------------------------------------------------

(define RailwaySystem%
  (class object%
   (super-new)

  (field [status 'uninitialised])

  (define/public (initialised?)
    (not (eq? status 'uninitialised)))
  
  (define/public (setStatus! stat)
    (if (or (eq? stat 'simulator)
            (eq? stat 'Z21))
        
        (set! status stat)
        (error "RailwaySystem% setStatus!: Recieved an unknown status, expected simulator or Z21, recieved: " stat)))

  (define/public (startSystem railway)
    (if (initialised?)
    
      (if (eq? status 'simulator)   ; test for which railway is selected
        (begin
        (cond ((eq? railway 'Hardware) (setup-hardware))
            
              ((eq? railway 'Straight) (setup-straight))
              ((eq? railway 'Straight-with-switch) (setup-straight-with-switch))
                                                   
              ((eq? railway 'Loop) (setup-loop))
          (else "RailwaySystem% startSystem: Recieved an unknown railway setup, recieved: " railway))
        (start))      

      (Z21:start-simulator))
    (error "RailwaySysytem% startSystem: Object is not initalised, please initialise before use."))
  )

  (define/public (stopSystem)
    (if (initialised?)
      (if (eq? status 'simulator)
        (stop)
        (Z21:stop-simulator))
    (error "RailwaySystem% stopSystem: Object is not initialised, please initialise before use."))
  ) 

  (define/public (getTrainLocation id)
    'test
  )

  (define/public (addTrain! id prevSeg currSeg)
      'test
  )

  (define/public (delteTrain! id)  ;does not exist in Z21
      'test
  )

  (define/public (setTrainSpeed! id speed)
      'test
  )

  (define/public (getSwitchPosition id)
      'test
  )

  (define/public (setSwitchPosition! id)
      'test
  )



))