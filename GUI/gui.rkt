#lang racket
(require racket/gui)
(require "../simulator/gui_simulator/interface.rkt")

(provide GUI%)

(define GUI%
  (class object%
    (super-new)

    (define frame (new frame% [label "PP2"]))

    (define mainPanel (new vertical-panel% [parent frame]))

    ;Track selector panes

    (define track-selector (new horizontal-panel% [parent mainPanel]))
    (define track-selector-left (new vertical-panel% [parent track-selector]))
    (define track-selector-right (new vertical-panel% [parent track-selector]))

    ;Train selectors panes

    (define train-selector (new horizontal-panel% [parent mainPanel]))
    (define train-selector-left (new vertical-panel% [parent train-selector]))
    (define train-selector-right (new vertical-panel% [parent train-selector]))

    ;Train creator panes

    (define train-creator (new horizontal-panel% [parent mainPanel]))
    (define train-creator-left (new vertical-panel% [parent train-creator ]))
    (define train-creator-right (new vertical-panel% [parent train-creator ]))
    


    ;track-selector items
    
    (define trackDropdown (new choice%
                               (label "Tracks")
                               (parent track-selector-left)
                               (choices (list "straight" "with switches")))) ;choices needs to be modular 

    (define startButton (new button% [parent track-selector-right]
                             [label "start simulator"]
                             [callback (lambda (button event)
                                         (start))]))

    (define stopButton(new button% [parent track-selector-right]
                           [label "stop simulator"]
                           [callback (lambda (button event)
                                       (stop))]))

    

    ;train-selector items

    (define trainDropdown (new choice%
                               (label "Trains")
                               (parent train-selector-left)
                               (choices (list "train1" "train2")))) ; choices need to be loaded from manager


    (define setSpeedButton (new button% [parent train-selector-right]
                                [label "set speed"]
                                [callback (lambda (button event)
                                            (display "speed set")
                                            (newline))]))

    (define getSpeedButton (new button% [parent train-selector-right]
                                [label "get speed"]
                                [callback (lambda (button event)
                                            (display "speed get")
                                            (newline))]))
                                      


    ;train creator items

    (define (popup name)
      (define frame1 (new frame% [label name]))
      (define main (new vertical-panel% [parent frame]))
      (new button% [parent frame1]
                                [label "new train"]
                                [callback (lambda (button event)               
                                            (display "new train")
                                            (newline))])
      (send frame1 show #t))
      

    (define newTrainButton (new button% [parent train-creator-left]
                                [label "new train"]
                                [callback (lambda (button event)
                                            (popup "new train")
                                            (display "new train")
                                            (newline))]))

    (define newLocoButton (new button% [parent train-creator-left]
                               [label "new locomotive"]
                               [callback (lambda (button event)
                                           (display "new locomotive")
                                           (newline))]))

    (define newRailcarButton (new button% [parent train-creator-left]
                                  [label "new railcar"]
                                  [callback (lambda (button event)
                                              (display "new railcar")
                                              (newline))]))
    



    


  
 

    (send frame show #t)

    ))

(new GUI%)