#lang racket

(require "NMBS/nmbs.rkt")
(require "Infrabel/infrabel.rkt")
(require dyoo-while-loop)


(startNMBS)
(startInfrabel)

(send server openConnection!)

(send client connect!)
(send server connect!)

(thread (lambda () (send server recieveTCP)))
(thread (lambda () (sleep 1) (send client TCPcall (list 'railwayManager 'getAllSwitchID))))
(send GUI startGUI)