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

  ;-------------------------------------------------------
  ; Function: initialised?
  ; Parameters: n/a
  ; Output: 
  ;     boolean: boolean
  ;       Use: Determine if the object is initialised.
  ; Use: Determine if the object is initialised. 
  ;-------------------------------------------------------

  (define/public (initialised?)
    (not (eq? status 'uninitialised)))
  
  ;----------------------------------------------------------
  ; Function: setStatus! 
  ; Parameters: 
  ;       stat: symbol
  ;         Use: The status of which simulator is used. 
  ; Output: n/a 
  ; Use: Set the status of which simulator is used. 
  ;----------------------------------------------------------

  (define/public (setStatus! stat)
    (if (or (eq? stat 'simulator)
            (eq? stat 'Z21))
        
        (set! status stat)
        (error "RailwaySystem% setStatus!: Recieved an unknown status, expected simulator or Z21, recieved: " stat)))

  ;---------------------------------------------------------------------
  ; Function: railway
  ; Parameters: n/a 
  ; Output: n/a 
  ; Use: Startup the simulator with the correct railway and simulator.
  ;---------------------------------------------------------------------

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

  ;--------------------------------
  ; Function: stopSystem
  ; Parameters: n/a 
  ; Output: n/a 
  ; Use: Stop the simulator
  ;--------------------------------

  (define/public (stopSystem)
    (if (initialised?)
      (if (eq? status 'simulator)
        (stop)
        (Z21:stop-simulator))
    (error "RailwaySystem% stopSystem: Object is not initialised, please initialise before use."))) 

  (define/public (getTrainLocation id)
    'test
  )

  ;-----------------------------------------------------------------------------
  ; Function: addTrain!
  ; Parameters: 
  ;       id: symbol
  ;         Use: The id used for the new created train. 
  ;       prevSeg: symbol
  ;         Use: The segment lying behind the train, determining its direction. 
  ;       currSeg: symbol
  ;         Use: The location of the train, this must be a detectionblock. 
  ; Ouput: n/a 
  ; Use: Create a new train.
  ;-----------------------------------------------------------------------------

  (define/public (addTrain! id prevSeg currSeg)
      (add-loco id prevSeg currSeg))

  ;---------------------------------------------------------------
  ; Function: deleteTrain!
  ; Parameters: 
  ;         id: symbol
  ;           Use: The id of the train that needs to be deleted.
  ; Output: n/a 
  ; Use: Delete a excisting train. 
  ;---------------------------------------------------------------

  (define/public (deleteTrain! id)  ;does not exist in Z21
      (remove-loco id))

  (define/public (setTrainSpeed! id speed)
      (if (eq? status 'simulator)
          (set-loco-speed! id speed)
          (Z21:set-loco-speed! id speed)))

  (define/public (getSwitchPosition id)
      'test
  )

  (define/public (setSwitchPosition! id)
      'test
  )



))