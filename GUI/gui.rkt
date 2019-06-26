#lang racket
(require racket/gui)
(require dyoo-while-loop)
(require "../TCP/client.rkt")
(require "../NMBS/routeManager.rkt")

(provide GUI%)

(define GUI%
  (class object%
    (super-new)

    (field [TCPclient 'uninitialised]
           [routeManager 'uninitialised])

    (define clientType 'object:Client%)
    (define routeType 'object:RouteManager%)

    (define/public (initialised?)
      (and (not (eq? TCPclient 'uninitialised))
           (not (eq? routeManager 'uninitialised))))

  ;  (define/public (initialise! client routeMan)
  ;    (if (and (eq? (object-name client) clientType)
  ;              (eq? (object-name routeMan) routeType))
  ;        (begin (set! TCPclient client)
  ;              (set! routeManager routeMan))
  ;        (error "GUI% initialise!: Contract violation expected a TCP client recieved and route manager: " client routeMan)))

    (define/public (initialise! client routeMan)
      (set! TCPclient client)
      (set! routeManager routeMan))

     (define railwayManager 'railwayManager)
     (define railway 'railway)
   ; (define railwayManager 
   ;   (when (eq? (object-name TCPclient) clientType)
   ;     (when (send TCPclient initialised?)
   ;      (class-field-accessor TCPclient railwayManager))))

   ; (define railway
   ;    (when (eq? (object-name TCPclient) clientType)
   ;     (when (send TCPclient initialised?)
   ;      (class-field-accessor TCPclient railway))))

    (define/public (startGUI)
      (send frame show #t))
    
    (define/public (keepAlive)
      (while (send frame is-enabled)
      #t
      ))

    (define frame (new frame% [label "PP2"]))


    (define mainPanel (new vertical-panel% [parent frame]))

    ;Track selector panes

    (define track-selector (new horizontal-panel% [parent mainPanel]))
    (define track-selector-left (new vertical-panel% [parent track-selector]))
    (define track-selector-right (new vertical-panel% [parent track-selector]))

    ;Traject selector pane

    (define traject-selector (new horizontal-panel% [parent mainPanel]))
    (define traject-selector-left (new vertical-panel% [parent traject-selector]))
    (define traject-selector-right (new vertical-panel% [parent traject-selector]))

    ;Traject creator pane

    (define traject-creator (new horizontal-panel% [parent mainPanel]))
    (define traject-creator-left (new vertical-panel% [parent traject-creator]))
    (define traject-creator-right (new vertical-panel% [parent traject-creator]))

    ;Train selectors panes

    (define train-selector (new horizontal-panel% [parent mainPanel]))
    (define train-selector-left (new vertical-panel% [parent train-selector]))
    (define train-selector-right (new vertical-panel% [parent train-selector]))

    ;Train creator panes

    (define train-creator (new horizontal-panel% [parent mainPanel]))
    (define train-creator-left (new vertical-panel% [parent train-creator ]))
    (define train-creator-right (new vertical-panel% [parent train-creator ]))

    ;Traject setter

    (define traject-setter (new horizontal-panel% [parent mainPanel]))
    (define traject-setter-left (new vertical-panel% [parent traject-setter ]))
    (define traject-setter-right (new vertical-panel% [parent traject-setter ]))
    


    ;track-selector items
    
   

    (define trackDropdown (new choice%
                               (label "Tracks")
                               (parent track-selector-left)
                               (choices (list "Hardware")))) ;choices needs to be modular 

    (define startButton (new button% [parent track-selector-right]
                             [label "start simulator"]
                             [callback (lambda (button event)
                                         (when (= (send trackDropdown get-selection) 0)
                                         (send TCPclient TCPcall (list railwayManager 'startRailway "../railwaySetup/test" ))
                                         (display "call made")
                                         (newline)
                                         ))
                                         ]))

    (define stopButton (new button% [parent track-selector-right]
                           [label "stop simulator"]
                           [callback (lambda (button event)
                                       (send TCPclient TCPcall (list railway 'stop-simulator)))]))

    ;Traject selector items

    (define trajectDopdown (new choice%
                                (label "Trajects")
                                (parent traject-selector-left)
                                (choices (loadTrajects))))
     
    (define/private (loadTrajects)
      (if (initialised?)
          (for-each symbol->string (send routeManager getAllRouteID))
          (list " ")))

    (define trajectSelect (new button% [parent traject-selector-right]
                               [label "select traject"]
                               [callback (lambda (button event)
                                           (display "traject selected"))]))

    ;Traject creator items
    
    (define trajectCreate (new button% [parent traject-creator-left]
                               [label "create traject"]
                               [callback (lambda (button event)
                                           (define popframe (new frame% [label"Create Traject"]))
                                           (define main (new vertical-panel% [parent popframe]))
                                           (define top (new horizontal-panel% [parent main]))
                                           (define middle (new horizontal-panel% [parent main]))
                                           (define left (new vertical-panel% [parent middle]))
                                           (define right (new vertical-panel% [parent middle]))
                                           (define bottom (new horizontal-panel% [parent main]))
                                           
                                           
                                           (define startDop (new choice%
                                                                 (label "Start node")
                                                                 (parent top)
                                                                 (choices (list "test"))))
                                                                 
                                                                 ;(for-each symbol->string (send TCPclient TCPcall (list railwayManager 'getAllDetectionID))
                                                                 
                                                                ; ))))
                                           
                                           (define endDrop (new choice%
                                                                (label "End node")
                                                                (parent top)
                                                                (choices 
                                                                  (list "test"))))
                                                                ;(for-each symbol->string (send TCPclient TCPcall (list railwayManager 'getAllDetectionID))))))

                                           (define trajectID (new text-field%
                                                                  (label "Traject ID")
                                                                  (parent right)
                                                                  (init-value "traject id")))
                                           
                                           (define createButton (new button% [parent bottom]
                                                                     [label "create"]
                                                                     [callback (lambda (button event)
                                                                              ;  (when (initialised?)
                                                                              ;   (send routeManager calculateRoute (string->symbol (send trajectID get-value)))))
                                                                                (display "created")) ]))
                                           (define cancelButton (new button% [parent bottom]
                                                                     [label "cancel"]
                                                                     [callback (lambda (button event)
                                                                                 (display "trajcet canceled")
                                                                                 (send popframe show #f))]))
                                                              
                                           (send popframe show #t))]))

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

 

    (define newTrainButton (new button% [parent train-creator-left]
                                [label "new train"]
                                [callback (lambda (button event)
                                            
                                            (define popframe (new frame% [label"Create train"]))
                                            (define main (new vertical-panel% [parent popframe]))
                                            (define top (new horizontal-panel% [parent main]))
                                            (define middle (new horizontal-panel% [parent main]))
                                            (define left (new vertical-panel% [parent middle]))
                                            (define right (new vertical-panel% [parent middle]))
                                            (define bottom (new horizontal-panel% [parent main]))
                                           

                                            (define mainLoc (new choice%
                                                                 (label "main locomotive")
                                                                 (parent top)
                                                                 (choices (list "loc1" "loc2"))))
                                            
                                            (define railcar (new choice%    ;allow more choices
                                                                 (label "railcars")
                                                                 (parent top)
                                                                 (choices (list "car1" "car2"))))

                                            (define id (new text-field%
                                                            (label "Train name")
                                                            (parent left)
                                                            (init-value "train name")))
                                            
                                            (define create (new button% [parent bottom]
                                                                [label "create train"]
                                                                (callback (lambda (button event)
                                                                            (display "train created")))))
                                            (define cancel (new button% [parent bottom]
                                                                [label "cancel"]
                                                                (callback (lambda (button event)
                                                                            (send popframe show #f)))))
                                            (send popframe show #t))]))
                            
                          

    (define newLocoButton (new button% [parent train-creator-left]
                               [label "new locomotive"]
                               [callback (lambda (button event)
                                           
                                           (define popframe (new frame% [label "create locomotive"]))
                                           (define main (new vertical-panel% [parent popframe]))
                                           (define top (new horizontal-panel% [parent main]))
                                           (define middle (new horizontal-panel% [parent main]))
                                           (define left (new vertical-panel% [parent middle]))
                                           (define right (new vertical-panel% [parent middle]))
                                           (define bottom (new horizontal-panel% [parent main]))

                                           (define locoID (new text-field%
                                                               [label "locomotive name"]
                                                               [parent top]
                                                               [init-value "name"]))

                                           (define createLoco (new button% [parent bottom]
                                                                   [label "create"]
                                                                   [callback (lambda (button event)
                                                                               (display "locmotive created")
                                                                               (newline))]))
                                           (define cancel (new button% [parent bottom]
                                                               [label "cancel"]
                                                               [callback (lambda (button event)
                                                                           (send popframe show #f))]))
                                           (send popframe show #t))]))

    (define newRailcarButton (new button% [parent train-creator-left]
                                  [label "new railcar"]
                                  [callback (lambda (button event)
                                              (define popframe (new frame% [label "create railcar"]))
                                              (define main (new vertical-panel% [parent popframe]))
                                              (define top (new horizontal-panel% [parent main]))
                                              (define middle (new horizontal-panel% [parent main]))
                                              (define left (new vertical-panel% [parent middle]))
                                              (define right (new vertical-panel% [parent middle]))
                                              (define bottom (new horizontal-panel% [parent main]))

                                              (define railcarID (new text-field%
                                                                     [label "Railcar name"]
                                                                     [parent top]
                                                                     [init-value "name"]))

                                              (define create (new button%
                                                                  [parent bottom]
                                                                  [label "create"]
                                                                  [callback (lambda (button event)
                                                                              (display "railcar created")
                                                                              (newline))]))

                                              (define cancel (new button%
                                                                  [parent bottom]
                                                                  [label "cancel"]
                                                                  [callback (lambda (button event)
                                                                              (send popframe show #f))]))
                                              (send popframe show #t))]))
    


    (define setTraject (new button%
                            [parent traject-setter-right]
                            [label "set traject"]
                            [callback (lambda (button event)

                                        (define popframe (new frame% [label "set traject"]))
                                        (define main (new vertical-panel% [parent popframe]))
                                        (define top (new horizontal-panel% [parent main]))
                                        (define middle (new horizontal-panel% [parent main]))
                                        (define left (new vertical-panel% [parent middle]))
                                        (define right (new vertical-panel% [parent middle]))
                                        (define bottom (new horizontal-panel% [parent main]))

                                        (define trajectDrop (new choice%
                                                                 [label "traject"]
                                                                 [parent top]
                                                                 [choices (list "traject1" "traject2")]))

                                        (define trainDrop (new choice%
                                                               [label "train" ]
                                                               [parent top]
                                                               [choices (list "train1" "train2")]))

                                        (define assign (new button%
                                                            [label "set traject"]
                                                            [parent bottom]
                                                            [callback (lambda (button event)
                                                                        (display "assigned"))]))

                                        (define cancel (new button%
                                                            [label "cancel"]
                                                            [parent bottom]
                                                            [callback (lambda (button event)
                                                                        (send popframe show #f))]))
                                        (send popframe show #t))]))


    ))

