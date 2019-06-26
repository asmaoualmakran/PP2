#lang racket

(require "NMBS/nmbs.rkt")
(require "Infrabel/infrabel.rkt")
(require dyoo-while-loop)


(startNMBS)
;(send GUI startGUI)
(startInfrabel)
(send server openConnection!)

(send client connect!)
(send server connect!)

;(define (startServerThread)
;  (send server startServer))

;(thread (send GUI keepAlive))
;(send server startServer)
;(send server recieveTCP)
(define (keepAlive)
  (while (boolean? #t)
         (display 'alive)
         (newline)))

;(define serverThread (void))

;(define start (lambda ()
;                (set! serverThread (thread (send server startServer)))))

;;(define threadPool (make-thread-group ))
(display "start thread\n")

(thread (lambda () (send server recieveTCP)))
(thread (lambda () (sleep 1) (send client TCPcall (list 'railwayManager 'getAllSwitchID))))
(send GUI startGUI)