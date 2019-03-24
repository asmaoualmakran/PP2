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
      'test
      )

    (define/public (swtich! switchID)
      'test)

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