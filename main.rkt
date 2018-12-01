#lang racket

(require "NMBS/nmbs.rkt")
(require "Infrabel/infrabel.rkt")
(require "GUI/gui.rkt")
(require "communication/communicationManager.rkt")


(define nmbs-infrabel (new CommunicationManager%))
(send nmbs-infrabel connect! trainManager)
(send nmbs-infrabel connect! railwayManager)

(send trainManager connectInfrabel! railwayManager)

(send railwayManager connectNMBS! trainManager)

(define gui-nmbs (new CommunicationManager%))


(define gui (new GUI%))

(send gui-nmbs connect! gui)
(send gui-nmbs connect! trainManager)

(send gui connectNMBS! trainManager)
(send trainManager connectGUI! gui)

(send trainManager loadRailway! railwayManager)
(send railwayManager createTrack! 'T1)
(send railwayManager createTrack! 'T2)
(send railwayManager createSwitch! 'S1)
(send railwayManager createDetectionblock! 'B1)


(send gui updateTracks)
(send gui updateSwitches)
(send gui updateDetectionblocks)


