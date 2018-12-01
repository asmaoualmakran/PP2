#lang racket
(require racket/gui)

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
    
    (define (toStrings list)
      (map
       (lambda (elm)
         (symbol->string elm))
       list))

    (define (combineStrings list)
      (let loop ([arg list]
                 [res ""])
        (if (null? arg)
            res
            (begin
              (set! res (string-append res " "))
              (set! res (string-append res (car arg)))
              (loop (cdr arg) res)))))

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
                     (display 'ok))])

    (new button% [parent panel]
         [label "Stop simulator"]
         [callback (lambda (button event)
                     (display 'ok))])

    (new button% [parent panel]
         [label "Add train"]
         [callback (lambda (button event)
                     (send connection createTrain! (string->symbol(send trainID get-value))))])
                     ;needs to be converted to a symbol, string is read out of the box.
    
    (define trainID (new text-field% [parent panel]
      [label "Train ID"]))

    (define/public (updateLocomotive)
      (when (not (eq? connection 'none))
        (let ([locols (send connection getAllLocomotiveID)])
              (send locomotives set-value (transformInfo locols)))))
        

    (new button% [parent panel]
         [label "Add locomotive"]
         [callback (lambda (button event)
                     (begin(send connection createLocomotive! (string->symbol (send locomotiveID get-value)))
                           (updateLocomotive)))
                     ])

     (define locomotiveID (new text-field% [parent panel]
                                [label "Locomotive ID"]))
                                          
    (define trains (new text-field% [parent panel]
                        [label "List of trains"]))

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