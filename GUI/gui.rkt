#lang racket
(require racket/gui)

(provide GUI%)

(define GUI%
  (class object%
    (super-new)

    (define frame (new frame% [label "PP2"]))

    (define panel (new vertical-panel% [parent frame]))

    (new button% [parent panel]
         [label "start simulator"]
         [callback (lambda (button event)
                     (display 'ok))])

    (new button% [parent panel]
         [label "Stop simulator"]
         [callback (lambda (button event)
                     (display 'ok))])

    (new button% [parent panel]
         [label "Add train"])
   ;      (new text-field% [parent panel]
    ;          [label " include train parameters"]))

 
    (define trains (new text-field% [parent panel]
         [label "List of trains"]))
       ;  [callback (lambda (button event)
        ;             (display 'ok))]))

    (define tracks (new text-field% [parent panel]
                        [label "List of tracks"]))

    (define switches (new text-field% [parent panel]
                          [label "List of switches"]))

    (define detections (new text-field% [parent panel]
                            [label "List of detectionblocks"]))
    

    (send trains set-value "test") ; dit gebruiken om de lijst met treinen te updatten

    ; Zien hoe je correct de zaken door gaat geven
    ; Zorgen dat je een update functie hebt die de nieuwe staat
    ; gaat doorsturen.

    
    (send frame show #t)
    

    ))