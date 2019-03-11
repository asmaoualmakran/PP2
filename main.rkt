#lang racket

(require "NMBS/nmbs.rkt")
(require "Infrabel/infrabel.rkt")
(require "GUI/gui.rkt")





(define gui (new GUI%))

(startInfrabel)
(startNmbs railwayManager railwayGraph)



(send trainManager loadRailway! railwayManager)
(send railwayManager createTrack! 'T1)
(send railwayManager createTrack! 'T2)
(send railwayManager createSwitch! 'S1)
(send railwayManager createDetectionblock! 'B1)


(send gui updateTracks)
(send gui updateSwitches)
(send gui updateDetectionblocks)


