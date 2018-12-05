#lang racket
(require racket/gui)
(require "../simulator/gui_simulator/interface.rkt")

(provide GUI%)

(define GUI%
  (class object%
    (super-new)

    ; The NMBS object where the GUI is connected to
    
    (define connection 'none)
    (define NMBSType   'object:TrainManager%)

     
    (define/public (connectNMBS! nmbs)
      (if (eq? connection 'none)
          (if (eq? (object-name nmbs) NMBSType)
              (set! connection nmbs)
              (error "GUI% connectNMBS!: contract violation expected nmbsType"))
          (error "GUI% connectNMBS!: there is already a NMBS connection")))

    (define/public (getNMBSConnection)
      (if (not (eq? connection 'none))
          connection
          (error "GUI% getNMBSConnection: no nmbs connection available")))

    (define/public (disconnectNMBS!)
      (if (not (eq? connection 'none))
          (set! connection 'none)
          (error " GUI% disconnectNMBS: no nmbs connection available")))
    
    ;----------------------------------------------------------------------------
    ; Function: toStrings
    ; Parameters:
    ;      list: list<symbol>
    ;        Use: Convert list containing symbols to a list containing strings.
    ; Ouput:
    ;     list: list<string>
    ;       Use: The converted list. 
    ; Use: Convert a list containing symbols to a list of strings.
    ;----------------------------------------------------------------------------
    
    (define (toStrings list)
      (map
       (lambda (elm)
         (symbol->string elm))
       list))

    ;---------------------------------------------------------------------
    ; Function: combineStrings
    ; Parameters:
    ;       list: list<string>
    ;         Use: A list containing strings.
    ; Output:
    ;      string: string
    ;        Use: A string combined from a list.
    ; Use: Combine the strings contained by a list into one large string.
    ;---------------------------------------------------------------------
    
    (define (combineStrings list)
      (let loop ([arg list]
                 [res ""])
        (if (null? arg)
            res
            (begin
              (set! res (string-append res " "))
              (set! res (string-append res (car arg)))
              (loop (cdr arg) res)))))

    ;---------------------------------------------------------------
    ; Function: transformInfo
    ; Parameters:
    ;      list: list<symbol>
    ;        Use: A list containing symbols.
    ; Output:
    ;     string: string
    ;       Use: A string combined from the symbols.
    ; Use: Convert a list containing symbols to one large string.
    ;---------------------------------------------------------------
    
    (define (transformInfo list)
      (combineStrings (toStrings list)))
      

    (define frame (new frame% [label "PP2"]))

    (define panel (new vertical-panel% [parent frame]))

    (define setups (new radio-box%
                        [label "setups"]
                        [choices (list "straight" "hardware setup" "straight with switch" "loop"
                                       "loop and switches")]
                        [parent panel]))
                         

    (new button% [parent panel]
         [label "start simulator"]
         [callback (lambda (button event)
                     (cond ((= (send setups get-selection) 0)(setup-straight))
                           ((= (send setups get-selection) 1)(setup-hardware))
                           ((= (send setups get-selection) 2)(setup-straight-with-switch))
                           ((= (send setups get-selection) 3)(setup-loop))
                           ((= (send setups get-selection) 4)(setup-loop-and-switches))
                           (else (display "please select a setup")))
                     (start))])

    (new button% [parent panel]
         [label "Stop simulator"]
         [callback (lambda (button event)
                     (stop))])

    (new button% [parent panel]
         [label "Add train"]
         [callback (lambda (button event)
                     (send connection createTrain! (string->symbol(send trainID get-value)))
                     (add-loco (string->symbol(send trainID get-value)))
                     (updateTrain))])
    ;needs to be converted to a symbol, string is read out of the box.
    
    (define trainID (new text-field% [parent panel]
                         [label "Train ID"]))

                                              
    (define trains (new text-field% [parent panel]
                        [label "List of trains"]))
    
    (define/public (updateTrain)
      (when (not (eq? connection 'none))
        (let ([trainls (send connection getAllTrainID)])
          (send trains set-value (transformInfo trainls)))))

    (new button% [parent panel]
         [label "Change Train's speed"]
         [callback (lambda (button event)
                     (send connection setTrainSpeed! (string->symbol (send getTrain get-value))
                           (string->number (send trainSpeed get-value))))])

    (new button% [parent panel]
         [label "Get train's speed"]
         [callback (lambda (button event)
                     (let ([id (string->symbol(send getTrain get-value))])
                       (send trainSpeed set-value (number->string(send connection getTrainSpeed id)))))])
                                 

    (define getTrain (new text-field% [parent panel]
                          [label "Train that need to be set or get"]))
    
    (define trainSpeed (new text-field% [parent panel]
                            [label "Train's speed"]))
    

    (new button% [parent panel]
         [label "Add locomotive"]
         [callback (lambda (button event)
                     (begin(send connection createLocomotive! (string->symbol (send locomotiveID get-value)))
                           (updateLocomotive)))
                   ])
    

    (define locomotiveID (new text-field% [parent panel]
                              [label "Locomotive ID"]))

    (define/public (updateLocomotive)
      (when (not (eq? connection 'none))
        (let ([locols (send connection getAllLocomotiveID)])
          (send locomotives set-value (transformInfo locols)))))


    (define locomotives (new text-field% [parent panel]
                             [label "List of locomotives"]))
  

    (define/public (updateTracks)
      (when (not (eq? connection 'none))
        (let ([railway (send connection getRailwayObj)])
          (let ([tracksls (send railway getAllTrackID)])
            (send tracks set-value(transformInfo tracksls))))))

    (define tracks (new text-field% [parent panel]
                        [label "List of tracks"]
                        ))

    (define/public (updateSwitches)
      (when (not (eq? connection 'none))
        (let ([railway (send connection getRailwayObj)])
          (let ([switchls (send railway getAllSwitchID)])
            (send switches set-value(transformInfo switchls))))))

    (define switches (new text-field% [parent panel]
                          [label "List of switches"]
                          ))
                          

    (define/public (updateDetectionblocks)
      (when (not (eq? connection 'none))
        (let ([railway (send connection getRailwayObj)])
          (let ([detectionls (send railway getAllDetectionblockID)])
            (send detections set-value(transformInfo detectionls))))))
                            
      
    (define detections (new text-field% [parent panel]
                            [label "List of detectionblocks"]
                            ))
    
 

    
    (send frame show #t)
    

    ))