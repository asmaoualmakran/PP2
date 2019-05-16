#lang racket

(define Interface%
  (class object%
    (super-new)

    (field [trainManager 'none]
           [routeManager 'none])

    (define trainManObj 'object:TrainManager%)
    (define routeManObj 'object:RouteManager%)    

    ; The tables that contain the used functions
    (define trainManFunc (make-hash))
    (define routeManFunc (make-hash))  

    ;-----------------------------------------------------------
    ; Function: initialise!
    ; Parameters: 
    ;       trainMan: object:TrainManager%
    ;         Use: The train manager that is currently used.
    ;       routeMan: object:RouteManager%
    ;         Use: The route manager that is currently used.
    ; Output: n/a 
    ; Use: Initialise the object.
    ;------------------------------------------------------------ 

    (define/public (initialise! trainMan routeMan)
      (if (and (eq? (object-name trainMan) trainManObj)
               (eq? (object-name routeMan) routeMan))
          (begin
            (set! trainManager trainMan)
            (set! routeManager routeMan)
            (addBasicFunctions!))   
      (error "Interface% initalise!: Contract violation expedec a Train manager and a route manager, recieved: " trainMan routeMan)))       
  
  (define/public (initialised?)
  'body
  )
  
  (define/private (addBasicFunctions!)
   'body
  )

  (define/public (callFunction lst)
  'body
  )
  
  ))