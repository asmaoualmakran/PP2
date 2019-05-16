#lang racket

(require "../TCP/client.rkt")
(require "../TCP/server.rkt")

(require rackUnit)


(let ([server (new Server%)]
      [client (new Client%)])

      (send server openConnection!)

      (let ([listener (send server getListener)]
            [state    (send server listenerActive?)]
            [portState (send server connectionActive?)])

    ;------------------------------------------------
    ; Initialisation tests
    ; Check if the server is correctly initialised.
    ;------------------------------------------------
      
    (test-case
    "Initialisation tests"
    (check-true (tcp-listener? listener))
    (check-true state)
    (check-false portState))

    (send client connect!)
    (send server connect!)

    ;------------------------------------------------------
    ; Connection tests
    ; Check if the server and client connected correctly
    ;------------------------------------------------------

    (test-case 
    "Connection tests"
    (check-true (send server inputPortOpen?))
    (check-true (send server outputPortOpen?))
    (check-true (send server connectionActive?))
    (check-true (port? (send server getOutputPort)))
    (check-true (port? (send server getInputPort)))
    (check-true (send client inputPortOpen?))
    (check-true (send client outputPortOpen?))
    (check-true (send client connectionActive?))
    (check-true (send server connectionActive?)))

    (send client closeConnection!)
    (send server closeConnection!)
    (set! state (send server listenerActive?))
    (set! portState (send server connectionActive?))

    ;------------------------------------------------------
    ; Disconnection tests
    ; Check if the server and client disconnect correctly
    ;------------------------------------------------------

    (test-case
    "Disconnection tests"
      (check-exn exn:fail? 
                  (lambda () (send server getListener)))
      (check-false state)
      (check-false portState)
      (check-false (send server inputPortOpen?))
      (check-false (send server outputPortOpen?))
      (check-false (send server connectionActive?))
      (check-false (send client inputPortOpen?))
      (check-false (send client outputPortOpen?))
      (check-false (send client connectionActive?))))

    ;--------------------------------------------
    ; Failure tests
    ; Check if the code fails in these tests
    ;--------------------------------------------

    (test-case
    "Fail tests"
    (check-false (send client connectionActive?))
    (check-false (send server connectionActive?))
    (check-exn exn:fail? 
                (lambda () (send client writeOutput 5)))
    (check-exn exn:fail? 
                (lambda () (send server writeOutput 5))))

    (send server openConnection!)
    (send client connect!)
    (send server connect!)

    (test-case 
    "Fail serialize tests"
    (check-exn exn:fail?
                (lambda () (send client writeOutput client)))
    (check-exn exn:fail?
              (lambda () (send server writeOutput client))))      
)
