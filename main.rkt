#lang racket

(require "NMBS/nmbs.rkt")
(require "Infrabel/infrabel.rkt")
(require "GUI/gui.rkt")
(require "communication/communicationManager.rkt")


(define nmbs-infrabel (new CommunicationManager%))
(send nmbs-infrabel connect! trainManager)
(send nmbs-infrabel connect! railwayManager)

(define gui-nmbs (new CommunicationManager%))

(define gui (new GUI%))

(send gui-nmbs connect! trainManager)
(send gui-nmbs connect! railwayManager)