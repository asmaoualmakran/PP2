#lang racket
(require racket/gui)
(require dyoo-while-loop)
(require "../TCP/client.rkt")
(require "../NMBS/routeManager.rkt")

(provide GUI%)

;---------------------------------
; Class: GUI%
; Parameters: n/a 
; Output: object:GUI%
; Use: A GUI for the simulator.
;---------------------------------

(define GUI%
  (class object%
    (super-new)

    (field [TCPclient 'uninitialised]
           [routeManager 'uninitialised]
           [trainManager 'uninitialised]
           [running      #f])   ;Predicate to define if the simulator is running

    ; Variables to enable easy type checking

    (define clientType 'object:Client%)
    (define routeType 'object:RouteManager%)
    (define trainManType 'object:TrainManager%)

    ; Variables used as tags to enable TCP calls to the backend 

    (define railwayManager 'railwayManager)
    (define railway 'railway)

    ;---------------------------------------------------
    ; Function: initialised? 
    ; Parameters: n/a 
    ; Output: boolean: boolean
    ;   Use: Determine if the object is initialised.
    ; Use: Determine if the object isinitialised.
    ;---------------------------------------------------

    (define/public (initialised?)
      (and (not (eq? TCPclient 'uninitialised))
           (not (eq? routeManager 'uninitialised))
           (not (eq? trainManager 'uninitialised))))

    ;------------------------------------
    ; Function: initialise!
    ; Parameters: 
    ;     client: object:Client%
    ;       Use: TCP client
    ;     routeMan: object:RouteManager%
    ;       Use: The route manager use to track the trajects. 
    ;     trainMan: object:TrainManager%
    ;       Use: The train manager used.
    ; Use: Initialise the GUI object.
    ;-------------------------------------------

    (define/public (initialise! client routeMan trainMan)
    (if (and (eq? (object-name client) clientType)
             (eq? (object-name routeMan) routeType)
             (eq? (object-name trainMan) trainManType))
      (begin       
        (set! TCPclient client)
        (set! routeManager routeMan)
        (set! trainManager trainMan))
      (error "GUI% initialise!: Contract violation, expected a Client% routeManager% and TrainManager% recieved: " client routeMan trainMan )))

    ;-------------------------------------------------------
    ; Function: updateGUI!
    ; Parameters: 
    ;     info: symbol
    ;       Use: Select which info needs to be retrieved.
    ; Output: 
    ;     data: list<string>
    ;       Use: The retrieved data. 
    ; Use: Retrieve the needed data to update the GUI.
    ;-------------------------------------------------------

    (define/private (updateGUI! info)
      (if (initialised?)
        (cond ((eq? info 'trains)   (map symbol->string (send trainManager getAllTrainID)))
              ((eq? info 'trajects) (map symbol->string (send routeManager getAllRouteID)))
              ((eq? info 'blocks)   (map symbol->string (send TCPclient TCPcall railwayManager 'getAllDetectionID)))
        (error "GUI% updateGUI!: recieved unknown message: " info))
      (error "GUI% updateGUI!: Object is not initialised, please initialise before use.")))


    ;-------------------------------------------------------------------
    ; Function: errorPop
    ; Parameters: 
    ;       msg: string
    ;       Use: The error message that needs to be shown in a popup
    ; Output: n/a 
    ; Use: Create a popup window with a given error message 
    ;-------------------------------------------------------------------

    (define/private (errorPop msg)
      (if (string? msg)
      (let* ([pop (new frame% [label "Error"])]
            [message (new message% 
                        (parent pop)
                        (label msg))])
         (send pop show #t))
          (error "GUI% errorPop: Expected a string as parameter, recieved: " msg))
      )

    ;----------------------------------------------------------------------
    ; Function: updateDropdown!
    ; Parameters: 
    ;       dropdown: object:choice%
    ;         Use: The dropdown that needs updating
    ;       elm: list<string>
    ;         Use: The elements that need to be placed into the dropdown
    ; Output: n/a 
    ; Use: Update the dropdown by placing the updated elements in it
    ;----------------------------------------------------------------------

    (define/private (updateDropdown! dropdown elm)
      (if (not (null? elm ))
       (begin (send dropdown clear)
        (for ([e elm])
          (send dropdown append e)
       ))
      (error "GUI% updateDropdown: Expected a non empty list, recieved: " elm)))
       
    ;--------------------------
    ; Function: startGUI
    ; Parameters: n/a 
    ; Output: n/a 
    ; Use: Start the GUI
    ;--------------------------

    (define/public (startGUI)
      (send frame show #t))
    

    ; The frame used to create the GUI

    (define frame (new frame% [label "PP2"]))

    ; The main panel used to create the GUI

    (define mainPanel (new vertical-panel% [parent frame]))

    ; Track selector panes

    (define track-selector (new horizontal-panel% [parent mainPanel]))
    (define track-selector-left (new vertical-panel% [parent track-selector]))
    (define track-selector-right (new vertical-panel% [parent track-selector]))

    ; Traject selector pane

    (define traject-selector (new horizontal-panel% [parent mainPanel]))
    (define traject-selector-left (new vertical-panel% [parent traject-selector]))
    (define traject-selector-right (new vertical-panel% [parent traject-selector]))

    ; Traject creator pane

    (define traject-creator (new horizontal-panel% [parent mainPanel]))
    (define traject-creator-left (new vertical-panel% [parent traject-creator]))
    (define traject-creator-right (new vertical-panel% [parent traject-creator]))

    ; Train selectors panes

    (define train-selector (new horizontal-panel% [parent mainPanel]))
    (define train-selector-left (new vertical-panel% [parent train-selector]))
    (define train-selector-right (new vertical-panel% [parent train-selector]))

    ; Train creator panes

    (define train-creator (new horizontal-panel% [parent mainPanel]))
    (define train-creator-left (new vertical-panel% [parent train-creator ]))
    (define train-creator-right (new vertical-panel% [parent train-creator ]))

    ; Traject setter

    (define traject-setter (new horizontal-panel% [parent mainPanel]))
    (define traject-setter-left (new vertical-panel% [parent traject-setter ]))
    (define traject-setter-right (new vertical-panel% [parent traject-setter ]))
    
    ; Track selector items
    
    (define status (new radio-box%
                    (label "Select simulator")
                    (parent track-selector-left)
                    (choices (list "GUI Simulator" "Z21"))))

    ; Selector from track setup

    (define trackDropdown (new choice%
                               (label "Tracks")
                               (parent track-selector-left)
                               (choices (list "Hardware" "Straight""Straight-with-switch" "Loop")))) 
    
    ; Start button for the simulator
    
    (define startButton (new button% [parent track-selector-right]
                             [label "start simulator"]
                             [callback (lambda (button event)
                                (let ((radioSelection (send status get-selection))
                                      (trackSelection (string->symbol(send trackDropdown get-string-selection))))
                                      (if (= radioSelection 0)
                                        (set! radioSelection 'simulator)
                                        (set! radioSelection 'Z21))
                                        
                                  (send TCPclient TCPcall (list railway 'startSimulator trackSelection radioSelection))
                                  (send TCPclient TCPcall (list railwayManager 'startRailway trackSelection))
                                  (send routeManager setGraph! (send TCPclient TCPcall (list railwayManager 'getGraphList)))
                                  (display (send routeManager getGraph))
                                  (newline)
                                  (set! running #t)
                                  ))]))
                                
                                
                                         
                                         
    ; Stop button for the simulator 

    (define stopButton (new button% [parent track-selector-right]
                           [label "stop simulator"]
                           [callback (lambda (button event)
                                       (send TCPclient TCPcall (list railway 'stopSimulator))
                                       (set! running #f)
                                       )]))

    ; Traject selector items

    (define trajectDopdown (new choice%
                                (label "Trajects")
                                (parent traject-selector-left)
                                (choices (list ))))
      
    ; Buttong for deleting a selected traject

    (define trajectDelete (new button% [parent traject-selector-right]
                               [label "Delete traject"]
                               [callback (lambda (button event)
                                           (send routeManager deleteRoute! (string->symbol (send trajectDopdown get-string-selection))))]))
  

    ; Traject creator items
    
    (define trajectCreate (new button% [parent traject-creator-left]
                               [label "create traject"]
                               [callback (lambda (button event)
                                      (trajectPop))
                                      ]))
 
    ; Creat a popup window for creating trajects
    (define (trajectPop) 
      (define frame (new frame% [label"Create Traject"]))
      (define main (new vertical-panel% [parent frame]))
      (define top (new horizontal-panel% [parent main]))
      (define middle (new horizontal-panel% [parent main]))
      (define left (new vertical-panel% [parent middle]))
      (define right (new vertical-panel% [parent middle]))
      (define bottom (new horizontal-panel% [parent main]))
      
      
      (define startDrop (new choice%
                            (label "Start node")
                            (parent top)
                            (choices 
                             (if running
                    
                              (map symbol->string (send TCPclient TCPcall (list railwayManager 'getAllDetectionID)))
                            
                             (list ))   ; When the simulator is not running there is no detectionblocks
                             )))
                            
                            
                           ; (updateGUI! 'blocks)
                           ; ))))
      
      (define endDrop (new choice%
                           (label "End node")
                           (parent top)
                           (choices 
                              (if running
                    
                              (map symbol->string (send TCPclient TCPcall (list railwayManager 'getAllDetectionID)))
                            
                             (list ))
                             )))
                           
      (define trajectID (new text-field%
                             (label "Traject ID")
                             (parent right)
                             (init-value "traject id")))
      
      (define createButton (new button% [parent bottom]
                                [label "create"]
                                [callback (lambda (button event)

                                         (let ((trajectid (string->symbol (send trajectID get-value)))
                                               (startDetect (string->symbol (send startDrop get-string-selection)))
                                               (endDetect (string->symbol (send endDrop get-string-selection))))
                                            (cond ((eq? trajectid '|traject id|) (errorPop "Please enter a traject id."))   ;there is no traject id filled in
                                                  ((send routeManager isMember? trajectid) (errorPop "Given id already exists."))
                                            (else (send routeManager calculateRoute trajectid startDetect endDetect)
                                                  (updateDropdown! trajectDopdown (updateGUI! 'trajects)))
                                         )))
                                           ]))

      (define cancelButton (new button% [parent bottom]
                                [label "cancel"]
                                [callback (lambda (button event)
                                            (display "trajcet canceled")
                                            (send frame show #f))]))
                         
      (send frame show #t))

    ; Train-selector items

    (define trainDropdown (new choice%
                               (label "Trains")
                               (parent train-selector-left)
                               (choices (list )))) ; choices need to be loaded from manager


    (define setSpeedButton (new button% [parent train-selector-right]
                                [label "set speed"]
                               [callback (lambda (button event)
                                           
                                           (define popframe (new frame% [label "Set train speed"]))
                                           (define main (new vertical-panel% [parent popframe]))
                                           (define top (new horizontal-panel% [parent main]))
                                           (define middle (new horizontal-panel% [parent main]))
                                           (define left (new vertical-panel% [parent middle]))
                                           (define right (new vertical-panel% [parent middle]))
                                           (define bottom (new horizontal-panel% [parent main]))

                                           (define trainSpeed (new text-field%
                                                               [label "New speed"]
                                                               [parent top]
                                                               [init-value "0"]))

                                           (define setSpeed (new button% [parent bottom]
                                                                   [label "Set speed"]
                                                                   [callback (lambda (button event)
                                                                              (let ((train (string->symbol (send trainDropdown get-string-selection)))
                                                                                    (speed (string->number (send trainSpeed get-value))))
                                                                                  (if (number? speed )
                                                                                  (begin
                                                                                    (send trainManager setTrainSpeed! train speed)
                                                                                    (send TCPclient TCPcall (list railway 'setSpeed! train speed)))
                                                                                    (errorPop "Speed is not a number, please provide one"))
                                                                                    ))]))


                                           (define cancel (new button% [parent bottom]
                                                               [label "cancel"]
                                                               [callback (lambda (button event)
                                                                           (send popframe show #f))]))
                                           (send popframe show #t))]))

    (define getSpeedButton (new button% [parent train-selector-right]
                                [label "get speed"]
                                [callback (lambda (button event)
                                            (let ((train (string->symbol(send trainDropdown get-string-selection))))
                                            (errorPop (number->string (send trainManager getTrainSpeed train)))))
                                            ]))

    (define trainDelete (new button% [parent train-selector-right]
                               [label "Delete train"]
                               [callback (lambda (button event)
                                           (display "train selected"))]))
                                      
    ; Train creator items

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
                                           

                          ;                  (define mainLoc (new choice%
                          ;                                       (label "main locomotive")
                          ;                                       (parent top)
                          ;                                       (choices (list "loc1" "loc2"))))
                                            
                           ;                 (define railcar (new choice%    ;allow more choices
                           ;                                      (label "railcars")
                           ;                                      (parent top)
                           ;                                      (choices (list ))))

                                            

                                            (define id (new text-field%
                                                            (label "Train name")
                                                            (parent left)
                                                            (init-value "train name")))

                                            (define location (new choice%
                                                             [label "Location"]
                                                             [parent left]
                                                             [choices 
                                                              (if running
                                                              (map symbol->string (send TCPclient TCPcall (list railwayManager 'getAllDetectionID)))
                                                              (list ))]
                                                             [callback (lambda (choice event)
                                                              (when running
                                                                (let* ((loc (string->symbol (send location get-string-selection)))
                                                                      (track (send TCPclient TCPcall (list railwayManager 'getTrackID loc)))
                                                                      (dirs (send TCPclient TCPcall (list railwayManager 'getConnections track))))
                                                                (updateDropdown! direction (map symbol->string dirs))))
                                                             )]))
                                
                                            
                                            (define direction (new choice%  
                                                              [label "Direction"]
                                                              [parent right]
                                                              [choices 
                                                              (if running
                                                              (let* ((loc (string->symbol (send location get-string-selection)))
                                                                    (track (send TCPclient TCPcall (list railwayManager 'getTrackID loc)))
                                                                    (dirs (send TCPclient TCPcall (list railwayManager 'getConnections track))))
                                                              
                                                               (map symbol->string dirs))
                                                               (list ))]))
                                            
                                            (define create (new button% [parent bottom]
                                                                [label "create train"]
                                                                (callback (lambda (button event)
                                                                  (let ((trainid (string->symbol (send id get-value)))
                                                                        (loc (string->symbol (send location get-string-selection)))
                                                                        (dir (string->symbol (send direction get-string-selection))))

                                                                  (cond ((eq? trainid '|train name|) (errorPop "Please enter a train id."))  
                                                                        ((not(send trainManager isUnique? trainid)) (errorPop "Given id already exists."))
                                                                  (else (send trainManager createTrain! trainid loc dir)
                                                                        (updateDropdown! trainDropdown (updateGUI! 'trains))
                                                                        (send TCPclient TCPcall (list railway 'createTrain trainid loc dir))))))
                                                                            )))

                                            (define cancel (new button% [parent bottom]
                                                                [label "cancel"]
                                                                (callback (lambda (button event)
                                                                            (send popframe show #f)))))
                                            
                                            (send popframe show #t))]))
                                                 

  ;  (define newLocoButton (new button% [parent train-creator-left]
  ;                             [label "new locomotive"]
  ;                             [callback (lambda (button event)
  ;                                         
  ;                                         (define popframe (new frame% [label "create locomotive"]))
  ;                                         (define main (new vertical-panel% [parent popframe]))
  ;                                         (define top (new horizontal-panel% [parent main]))
  ;                                         (define middle (new horizontal-panel% [parent main]))
  ;                                         (define left (new vertical-panel% [parent middle]))
  ;                                         (define right (new vertical-panel% [parent middle]))
  ;                                         (define bottom (new horizontal-panel% [parent main]))
;
  ;                                         (define locoID (new text-field%
  ;                                                             [label "locomotive name"]
  ;                                                             [parent top]
  ;                                                             [init-value "name"]))
;
  ;                                         (define createLoco (new button% [parent bottom]
  ;                                                                 [label "create"]
  ;                                                                 [callback (lambda (button event)
  ;                                                                             (display "locmotive created")
  ;                                                                             (newline))]))
  ;                                         (define cancel (new button% [parent bottom]
  ;                                                             [label "cancel"]
  ;                                                             [callback (lambda (button event)
  ;                                                                         (send popframe show #f))]))
  ;                                         (send popframe show #t))]))
;
  ;  (define newRailcarButton (new button% [parent train-creator-left]
  ;                                [label "new railcar"]
  ;                                [callback (lambda (button event)
  ;                                            (define popframe (new frame% [label "create railcar"]))
  ;                                            (define main (new vertical-panel% [parent popframe]))
  ;                                            (define top (new horizontal-panel% [parent main]))
  ;                                            (define middle (new horizontal-panel% [parent main]))
  ;                                            (define left (new vertical-panel% [parent middle]))
  ;                                            (define right (new vertical-panel% [parent middle]))
  ;                                            (define bottom (new horizontal-panel% [parent main]))
;
  ;                                            (define railcarID (new text-field%
  ;                                                                   [label "Railcar name"]
  ;                                                                   [parent top]
  ;                                                                   [init-value "name"]))
;
  ;                                            (define create (new button%
  ;                                                                [parent bottom]
  ;                                                                [label "create"]
  ;                                                                [callback (lambda (button event)
  ;                                                                            (display "railcar created")
  ;                                                                            (newline))]))
;
  ;                                            (define cancel (new button%
  ;                                                                [parent bottom]
  ;                                                                [label "cancel"]
  ;                                                                [callback (lambda (button event)
  ;                                                                            (send popframe show #f))]))
  ;                                            (send popframe show #t))]))
    
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

