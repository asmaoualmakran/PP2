#lang racket

;elke trein weet welke route dat hij rijdt via daar de route opvragen
;routes worden gecreeerd via GUI -> interne communicatie
;ROUTEMANAGER IS NODIG!!!!!
; ---> initialisatie van de graf!!!!!
(provide Interface%)


(define Interface%
  (class object%
    (super-new)

    (field [trainManager 'none]
           [routeManager 'none])

    (define trainManObj 'object:TrainManager% )
    (define routeManObj 'object:RouteManager% )    

    ; The tables that contain the used functions.

    (define trainManFunc (make-hash))
    (define routeManFunc (make-hash))  

    (define trainManSym 'trainManager)
    (define routeManSym 'routeManager)

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
               (eq? (object-name routeMan) routeManObj))
          (begin
            (set! trainManager trainMan)
            (set! routeManager routeMan)
            (addBasicFunctions!))   
      (error "Interface% initalise!: Contract violation expected a Train manager and a route manager, recieved: " trainMan routeMan)))      
  
  ;----------------------------------------------------
  ; Function: initialsed? 
  ; Parameters: n/a 
  ; Output: 
  ;     boolean: boolean 
  ;      Use: Determine if the object is initialised.
  ; Use: Determine if the object is initialised.
  ;----------------------------------------------------
  
  (define/public (initialised?)
    (and (not (eq? 'uninitialsed trainManager))
         (not (empty? trainManFunc)))
         (not (eq? 'uninitialised routeManager))
         (not (empty? routeManFunc)))

  ;--------------------------------------------------------------------
  ; Function: addFunction! 
  ; Parameters: 
  ;       dest: symbol
  ;         Use: The table where the function needs to be added to.
  ;       key: symbol
  ;         Use: The key where under the function needs to be hashed.
  ;       function: procedure
  ;         Use: The function to be added to the table.
  ; Output: n/a 
  ; Use: Add a function to one of the function hash table.
  ;--------------------------------------------------------------------

  (define/public (addFunction! dest key function)
    (if (and (symbol? key)
             (procedure? function))

      (if (and (not (hash-has-key? trainManFunc key))
            (not (hash-has-key? routeManFunc key)))

        (cond ((eq? dest trainManSym) (hash-set! trainManFunc key function))
              ((eq? dest routeManSym) (hash-set! routeManFunc key function))
        (else (error "Interface addFunction!: Destination is unknown, recieved: " dest)))

      (error "Interface addFunction!: Key is not unique, function already exists, recieved: " key))
    (error "Interface addFuction!: Contract violation expected two symbols and a function, recieved: " dest key function)))
  
  ;---------------------------------------------------
  ; Function: addBasicFunctions!
  ; Parameters: n/a 
  ; Output: n/a 
  ; Use: Add the basic functions to the hash tables.
  ;---------------------------------------------------

  (define/private (addBasicFunctions!)
   
    ; The trainManager functions

    ;-----------------------------------------------------------------------------
    ; Function: n/a
    ; Parameters: 
    ;         trainID: symbol
    ;           Use: The id of the train which of it's traject needs to be get.
    ; Output: 
    ;      route: list<symbol>
    ;       Use: The route of the train.
    ; Use: Get the route of the train.
    ;-----------------------------------------------------------------------------

    (addFunction! trainManSym 'getTrainRoute (lambda (trainID)
                                                (let ([train (send trainManager getTrain trainID)]
                                                      [route 'none])
                                                      (let ([routeID (send train getTraject)])
                                                      
                                                      (set! route (send routeManager getRoute routeID)))
                                                route)))

    ;----------------------------------------------------------------------------
    ; Function: n/a
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: The ID of the train of which the speed needs to be adjusted.
    ;       speed: number
    ;         Use: The number to which te speeds needs to be set to.
    ; Output: n/a
    ; Use: Change the speed of the train.
    ;----------------------------------------------------------------------------

    (addFunction! trainManSym 'setTrainSpeed! (lambda (trainID speed)

                                                  (let ([train (send trainManager getTrain trainID)])
                                                        (send train setSpeed! speed))))

    ;-------------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       trainID: symbol
    ;         Use: The ID of the train of which the speeds needs to be retrieved.
    ; Output: 
    ;     speed: number
    ;       Use: The speed of the train.
    ; Use: Getting the speed of the train.
    ;--------------------------------------------------------------------------------

    (addFunction! trainManSym 'getTrainSpeed! (lambda (trainID)
                                                  (let ([train (send trainManager getTrain trainID)]
                                                        [speed 'none])
                                                        
                                                        (set! speed (send train getSpeed))
                                                        speed)))
                                                      
    ;--------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         trainID: symbol
    ;           Use: The ID of the train who's location needs to be set.
    ;         locID: symbol
    ;           Use: The location that needs to be set.
    ; Output: n/a
    ; Use: Setting the train's location.
    ;---------------------------------------------------------------------

    (addFunction! trainManSym 'setTrainLocation! (lambda (trainID locID)
                                                    (let ([train (send trainManager getTrain trainID)])
                                                    
                                                          (send train setCurrentPosition! locID))))

    ;--------------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;         trainID: symbol
    ;           Use: The ID of the train who's current location needs to be fetched.
    ; Output: 
    ;     location: symbol
    ;       Use: The current location of the train.
    ; Use: Retrieving the current location of the train.
    ;---------------------------------------------------------------------------------

    (addFunction! trainManSym 'getTrainLocation (lambda (trainID)
                                                       (let ([train (send trainManager getTrain trainID)]
                                                          [location 'none])
                                                          
                                                          (set! location (send train getCurrentLocation))
                                                          location)))
                                                    
                                                   
    ; The routeManager functions

    ;--------------------------------------------------------------------------------------
    ; Function: n/a 
    ; Parameters: 
    ;       graphList: list<list>
    ;         Use: The list containing the edges that is needed to constructed the graph.
    ; Output: n/a 
    ; Use: Set the graph of the route manager.
    ;--------------------------------------------------------------------------------------

    (addFunction! routeManSym 'setGraph! (lambda (graphList)
                                              (send routeManager setGraph! graphList)))

    ;-------------------------------------------------
    ; Function: n/a 
    ; Parameters: n/a 
    ; Output: 
    ;     graph: list<list>
    ;       Use: The edges contained by the graph.
    ; Use: Retrieving the graph representation.
    ;-------------------------------------------------

    (addFunction! routeManSym 'getGraph (lambda ()
                                            (send routeManager getGraph)))

  )

  ;-----------------------------------------------------------------------
  ; Function: getFunction
  ; Parameters: 
  ;         hasht: symbol
  ;           Use: The hashtable where the function needs to be fetched.
  ;         key: symbol
  ;           Use: The key where under the function is hashed.
  ; Output:
  ;      function: procedure
  ;       Use: The fetched function.
  ; Use: Getting a function from one of the hashtables.
  ;-----------------------------------------------------------------------

  (define/private (getFunction hasht key)
    (if (and (symbol? hasht)
             (symbol? key))
        (cond 
              ((eq? hasht trainManSym)  ((if (hash-has-key? trainManFunc key)
                                            (hash-ref trainManFunc key)
                                            (error "Interface% getFunction: given key does not belong to the table" hasht key))))
             
              ((eq? hasht routeManObj)  ((if (hash-has-key? routeManFunc key)
                                              (hash-ref routeManFunc key)
                                              (error "Interface% getFunction: given key does not belong to the table" hasht key))))
        (else (error "Interface getFunction: The given table name does not exist, recieved: " hasht)))
   
    (error "Interface% getFunction: Contract violation expected two symbols, recieved: " hasht key)))

  ;----------------------------------------------------------------------------
  ; Function: callFunction
  ; Parameters: 
  ;         lst: list<symbol symbol any any>
  ;           Use: List containing information to call the correct function.
  ; Output: 
  ;     output: any
  ;       Use: The output returned by the called function.
  ; Use: Call the correct function with parameters.
  ;----------------------------------------------------------------------------

  (define/public (callFunction lst)
    (if (list? lst)
      (if (not (null? lst))

        (let ([obj (first lst)]
              [func (second lst)]
              [pm (cddr lst)])

              (if (and (symbol? obj)
                       (symbol? func))
                (cond 
                      ((empty? pm) ((getFunction obj func)))  ;call parameters less function.
                      ((= (length pm) 1) ((getFunction obj func) (first pm)))
                      ((= (length pm) 2) ((getFunction obj func) ((first pm)(second pm))))
                (else (error "Interface% callFunction: Parameters list is to long.")))
    
            (error "Interface% callFunction: Contract violation expected a symbol as object and function as symbol, recieved: " obj func)))
      (error "Interface% callFunction: Contract vioaltion expected a non empty list, recieved: " lst))
    (error "Interface% callFunction: Contract violation expected a list, recieved: " lst)))
  
  ))