#lang racket

(require racket/file)
(require dyoo-while-loop)
(require logger)

(require "railwayManager.rkt")


(define manager (new RailwayManager%))


(provide FileReader%)

(define FileReader%
  (class object%
    (super-new)

    ; Hashstable used to save functions
    (define functionHash (make-hash))
    
    (define managerType 'object:RailwayManager%)

    (field [railmanager 'none]
           [maxCallLength 4])

    ;-----------------------------------------------------------
    ; Function: initialise!
    ; Parameters:
    ;          manager: object:RailwayManager%
    ;            Use: The railway manager that needs to be set
    ; Output: n/a
    ; Use: Initialise the object with a railway manager
    ;------------------------------------------------------------
    
    (define/public (initialise! manager)
      (if (not (initialised?))
          (if (eq? managerType (object-name manager))
              (begin (set! railmanager manager)
                     (addBasicFunctions!))
              (error "FileReader% initialise!: Contract violation expected object:RailwayManager% recieved:" manager))
          (error "FileReader% initialise!: object is already initialised")))

    ;-------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;        boolean: boolean
    ;         Use: Determine if the object is initialised
    ; Use: Determine if the object is initialised
    ;-------------------------------------------------------
    
    (define/public (initialised?)
      (and (not (eq? railmanager 'none))
           (not (hash-empty? functionHash))))

    ;-------------------------------------------------
    ; Function: getManager
    ; Parameters: n/a
    ; Output:
    ;       manager: object:RailwayManager%
    ;         Use: The railway manager that is used
    ; Use: Retrieve the used railway manager
    ;-------------------------------------------------

    (define/public (getManager)
      (if (initialised?)
          railmanager
          (error "FileReader% getManager: Object is not initialised please initialise before use")))
    
    ;-----------------------------------------------------------
    ; Function: add!
    ; Parameters:
    ;       key; symbol
    ;        Use: The key that will be used for the hashing.
    ;       function: #<procedure>
    ;        Use: The function that needs to be added.
    ; Output: n/a
    ; Use: Save a new function into the hashtable
    ;----------------------------------------------------------

    (define/public (add! key function)
      (if (and (symbol? key)
               (procedure? function))
          (if (not (hash-has-key? functionHash key))
              (hash-set! functionHash key function)
              (error "FileReader% add!: Given key is already member of the hashtable, recieved:" key))
          (error "FileReader% add!: Contract violation, expected a symbol and a function recieved:" key function)))

    ;-----------------------------------------------------------------------
    ; Function: update!
    ; Parameters:
    ;        key: symbol
    ;         Use: The key of the function that needs to be updated.
    ;        function: #<procedure>
    ;         Use: The new definition of the the to be updated function.
    ; Output: n/a
    ; Use: Update an existing function with a new definition.
    ;-----------------------------------------------------------------------
    
    (define/public (update! key function)
      (if (and (symbol? key)
               (procedure? function))
          (if (member? key)
              (hash-set! functionHash key function)
              (error "FileReader% update!: Given key does not belong to a function, can not update the function, recieved:" key))
          (error "FileReader% update!: Contract violation expected a symbol and a procedure, recieved" key function)))
               
    ;----------------------------------------------------------------
    ; Function: delete!
    ; Parameters:
    ;       key: symbol
    ;        Use: The key of the function that needs to be deleted.
    ; Output: n/a
    ; Use: Delete a function using it's key.
    ;----------------------------------------------------------------

    (define/public (delete! key)
      (if (symbol? key)
          (if (hash-has-key? functionHash key)
              (hash-remove! functionHash key)
              (error "FileReader delete!: Hashtable does not contain given key, recieved:" key))
          (error "FileReader% delete!: Contract violation, expected a symbol recieved:" key)))

    ;---------------------------------------------------------------------
    ; Function: member?
    ; Parameters:
    ;        key: symbol
    ;         Use: The key of the function that needs to be checked.
    ; Output:
    ;        boolean: boolean
    ;         Use: Determine if the function is in the hashtable.
    ; Use: Determine if the given key of a function is in the hashtable
    ;---------------------------------------------------------------------
    
    (define/public (member? key)
      (if (symbol? key)
          (hash-has-key? functionHash key)
          (error "FileReader% member?: Contract violation, expected a symbol recieved:" key)))


    ;-------------------------------------------------------------------
    ; Function: getFunc
    ; Parameters:
    ;         key: symbol
    ;          Use: The key used to hash the function in the hashtable
    ; Output:
    ;         func: #<procedure>
    ;          Use: The function that needs to be retrieved
    ; Use: Retrieve a function from the hashtable using it's key.
    ;-------------------------------------------------------------------
    
    (define/public (getFunc key)
      (if (symbol? key)
          (if (member? key)
              (hash-ref functionHash key)
              (error "FileReader%: Hashtable has no such key, recieved:" key))
          (error "FileReader% getKey: Contract violation, expected a symbol recieved:" key)))
          
    ;-------------------------------------------------------------------------------
    ; Function: loadRailway
    ; Parameters:
    ;       path: string
    ;        Use: The file that needs to be read
    ;       manager: object: RailwayManager%
    ;        Use: The railway manager that is used at that moment.
    ; Output: n/a
    ; Use: Read a file where the railway is saved and create the correct objects.
    ;-------------------------------------------------------------------------------
    
    (define/public (loadRailway path)
      (if (initialised?)
          
          (if (send railmanager containsRailway?)
              (begin
                (display "It contained one")
                (send railmanager clearAllTables!)
                (parseFile(openFile path))
                (info "FileReader% loadRailway: The railway manager contained a railway, this has been deleted and replaced"))
              
              (parseFile (openFile path)))
          
          ((error "FileReader% loadRailway: Object is not initialised, please initialise before use."))))
      
    ;----------------------------------------------------------------------
    ; Function: openFile
    ; Parameters:
    ;         file: string
    ;          Use: The path location of the file that needs to be opend.
    ; Output:
    ;         readFile: list
    ;          Use: The content of the file in list form
    ; Use: Open the file and return it as a list.
    ;----------------------------------------------------------------------

    (define/private (openFile file)
      (if (initialised?)
          (if (string? file)
              (let ([content '()])
                (set! content (file->list file #:mode 'text))   ;open it as a list and return the created list
                content)
              (error "FileReader% readFile: Contract violation, expected a string recieved:" file))
          (error "FileReader% readFile: Object is not initialised, please initialise before use")))

    ;------------------------------------------------------------
    ; Function: parseFile
    ; Parameters:
    ;         lst: list
    ;          Use: The content of the read file in list form.
    ; Output: n/a
    ; Use: Parse the file and call the correct functions.
    ;------------------------------------------------------------

    (define/private (parseFile lst)
      (display "parsing started")
      (if (initialised?)
          (if (and (list? lst)
                   (not (null? lst)))
              (for ([i lst])
                (parseList i))
              (error "FileReader% parseFile: Contract violation, expected a non empty list recieved:" lst))
          (error "FileReader% parseFile: Object is not initialised, please initalise before use.")))

    ;----------------------------------------------------------------------------
    ; Function: parseList
    ; Parameters:
    ;         lst: list
    ;          Use: The list that needs to be parsed
    ; Output: n/a
    ; Use: Parse the list and call the correct functions with it's parameters.
    ;----------------------------------------------------------------------------
    
    (define/private (parseList lst)
      (display "list gets parsed")
      (newline)
      (display lst)
      (if (list? lst)
          (if (not(null? lst))
              (if (<= (length lst) maxCallLength)    ; allow max 4 elements in the list  (make it a field)
                  (let ([func 'null]    ; saving the func and pms
                        [pm   'null]
                        [pmls '()])
                
                    ;     (for ([i lst])    
              
                    ;      (let ([str (cadr i)]) ; (string->symbol str)])
                        
                       
                    ;       (if (symbol? str)
                       
                    ;          (if (= idx 0)            ; The first element must be an registered function
                    ;
                    ;                 (if (member? str)
                    ;                    
                    ;                      (set! func str)
                    ;              
                    ;                  (error "FileReader% parseList: Given function does not exist, please add to the table, recieved:" str))
                    ;             
                    ;             (set! pm str))        ; Beginning from the second element is one of the pm's
                          
                    ;      
                    ;       (if (list? str)
                    ;          (set! pmls str)
                    ;         (error "FileReader% parseList: Expected a symbol or a list, recieved" str)))
                    ; (set! idx (+ idx 1))))

                    (for ([i lst]
                          [idx (in-range 0 (length lst))])

                      (let ([str (cadr i)])  ; string->symbol str

                        (if (= idx 0)    
                            
                            (if (member? str)
                                (set! func str)
                                (error "FileReader% parseList: Given function does not exist, please add to the table, recieved:" str))

                            (if (= idx 1)
                                (set! pm str)
                            (begin
                              (newline)
                              (display "appended: ")
                              (display str)
                              (newline)
                             (set! pmls (append pmls str)))))))
                    
                            (if (null? pmls)     
                                (callFunction func pm)     
                                (callFunction func pm pmls)))
                  
                        (error  "FileReader% parseList: More elements are given then parsed" lst))
                      (info "FileReader% parseList: Recieved an empty list, no parsing happened"))
                    (error "FileReader% parseList: Contract violation, expected list, recieved:" lst)))

              ;----------------------------------------------------------------------------------------
              ; Function: callFunction
              ; Parameters:
              ;        lst: list<string>
              ;         Use: The list that contains the parsed string which contains a function call.
              ; Output: n/a
              ; Use: Call the correct functions using the given string list
              ;----------------------------------------------------------------------------------------

              (define/private (callFunction func objname . connections)
                (if (<= (length connections) (- maxCallLength 2))   ;two positions are already in use by function and mandatory pm
                (if (and (symbol? func)
                         (symbol? objname))
                    (if (not (null? connections))
                        
                        (cond ((symbol? (car connections)) ((getFunc func) objname (car connections)))
                              ((symbol? (cdr connections) ((getFunc func objname (car connections)(cdr connections))))))
                   ;     ((getFunc func) objname connections)) 
                        ((getFunc func) objname))
                    (error "FileReader callFunction: Contract violation expected two symbols and a list, recieved:" func objname connections))
                (error "FileReader callFunction: Conctract violation, optional argument list is to long maximum length is:" (- maxCallLength 2))))

              ;------------------------------------------------------------------------------
              ; Function: validConnection?
              ; Parameters:
              ;          id1: symbol
              ;           Use: One of the connections.
              ;          id2: symbol
              ;           Use: One of the connections>
              ; Output:
              ;     boolean: boolean
              ;      Use: Determine if the connection between two railway objects is vaild.
              ; Use: Determine if the connection between two railway object is vaild. 
              ;-------------------------------------------------------------------------------
    
              (define/private (validConnection? id1 id2)
                (and (and (and (symbol? id1)
                               (symbol? id2))
                
                          (or (and (eq? id1 'none)
                                   (send railmanager isMember? id2))
                              (and (eq? id2 'none)
                                   (send railmanager isMember? id1))
                    
                              (and (send railmanager isMember? id1)
                                   (send railmanager isMember? id2))))
           
                     (or (and (send railmanager isDetectionblock? id1)
                              (send railmanager isTrack? id2))
                    
                         (and (send railmanager isTrack? id1)
                              (send railmanager isDetectionblock? id2))
                    
                         (and (not (send railmanager isDetectionblock? id1))
                              (not (send railmanager isDetectionblock? id2)))
                    
                         (and (not (send railmanager isSwitch? id1))
                              (not (send railmanager isDetectionblock? id2)))
                    
                         (and (not (send railmanager isDetectionblock? id1))
                              (not (send railmanager isSwitch? id2)))

                         (and (not (send railmanager isDetectionblock? id1))
                              (not (eq? 'none id2)))

                         (and (not (eq? 'none id1))
                              (not (send railmanager isDetectionblock? id2))))))

  ;  (define/private (validConnection? id1 id2)
   ;   (and (and (symbol? id1)
    ;            (symbol? id2))
     ;      (or (and (send railmanager isDetectionblock? id1)
      ;              (send railmanager isTrack? id2))
       ;        (and (send railmanager isTrack? id1)
        ;            (send railmanager isDetectionblock? id2))
         ;      (and (send railmanager isSwitch? id1)
          ;          (send railmanager isTrack? id2))
           ;    (and (send railmanager isTrack? id1)
            ;        (send railmanager isSwitch? id2)))))
              
                    
                              
              ;----------------------------------------------------------------------
              ; Function: addBasicFunctions!
              ; Parameters: n/a
              ; Output: n/a
              ; Use: Add functions for ceating detection block, switches and tracks
              ;----------------------------------------------------------------------

              (define/public (addBasicFunctions!)

                ;------------------------------------------------------------
                ; Function: n/a
                ; Parameters:
                ;         name: symbol
                ;          Use: The ID of the to be created detectionblock.
                ; Output: n/a
                ; Use: Create a detectionblock with the given ID.
                ;------------------------------------------------------------
      
                (add! 'block% (lambda (name)
                                (when (symbol? name)
                                
                                (send railmanager createDetectionblock! name)
                                (send(send railmanager getDetectionblock name) initialise!))
                                (display "block created")))

                ;---------------------------------------------------
                ; Function: n/a
                ; Parameters:
                ;        name: symbol
                ;         Use: The ID of the to be created switch.
                ; Output: n/a
                ; Use: Create a switch with the given ID.
                ;---------------------------------------------------
                                                       
                (add! 'switch% (lambda (name)
                                 (when (symbol? name)

                                       (send railmanager createSwitch! name)
                                       (send (send railmanager getSwitch name) initialise!))))
                                       
                                    

                ;-----------------------------------------------------
                ; Function: n/a
                ; Parameters:
                ;         name: symbol
                ;          Use: The ID of the to be created track.
                ; Output: n/a
                ; Use: Create a track with the given ID.
                ;-----------------------------------------------------

                (add! 'track% (lambda (name)
                                (when (symbol? name)
                                (send railmanager createTrack! name)
                                (send (send railmanager getTrack name) initialise!))))
                                

                ;-------------------------------------------------------------------------------------
                ; Function: n/a
                ; Parameters:
                ;        id1: symbol
                ;         Use: The identifiction of one of the railway objects, or the symbol none.
                ;        id2: symbol
                ;         Use: The identification of one of the railway objects, or the symbol none.
                ; Output: n/a
                ; Use: Connect two railway objects, or creating a dead end on the railway object.
                ;-------------------------------------------------------------------------------------
      
                (add! 'connect! (lambda (id1  id2)
                                  (if (and(symbol? id1)
                                      (symbol? id2))
                           
                                      (if (validConnection? id1 id2)
                                          (let ([obj1 'none]
                                                [obj2 'none])
                                            (when (send railmanager isMember? id1)
                                              (set! obj1 (send railmanager getObject id1)))
                                            (when (send railmanager isMember? id2)
                                              (set! obj2 (send railmanager getObject id2)))

                                            (cond
                                              ((and (eq? obj1 'none)
                                                    (or (send railmanager isSwitch? id2))
                                                    (send railmanager isTrack? id2)) (send obj2 setConnection! id1))

                                              ((and (eq? obj2 'none)
                                                    (or (send railmanager isSwitch? id1)
                                                        (send railmanager isTrack? id1))) (send obj1 setConnection! id2))

                                              ((and (send railmanager isDetectionblock? id1)
                                                    (send railmanager isTrack? id2)) (begin (send obj1 setTrackID! id2)
                                                                                             (send obj2 setDetectionblockID! id1)))
                                              ((and (send railmanager isDetectionblock? id2)
                                                    (send railmanager isTrack? id1)) (begin (send obj2 setTrackID! id1)
                                                                                             (send obj1 setDetectionblockID! id2)))
                                       
                                              (else (begin (send obj1 setConnection! id2)
                                                           (send obj2 setConnection! id1)))))
                                          (error "FileReader% add! 'connection: No valid connection between the given objects."))
                                      (error "FileReader% add! 'connection: Contract violation expected two symbols, recieved:" id1 id2))))

                ;---------------------------------------------------------------------------------------------------------
                ; Function: n/a
                ; Parameters:
                ;         switchID: symbol
                ;           Use: The identification of the switch.
                ;         id1: symbol
                ;          Use: The identification of the railway object that needs to be connected or the symbol none.
                ;         id2: symbol
                ;          Use: The identification of the railway object that needs to be connected or the symbol none.
                ; Output: n/a
                ; Use: Connecting two railway objects to the splitted connection of the switch, or create a dead end.
                ;----------------------------------------------------------------------------------------------------------
      
                (add! 'connectY! (lambda (switchID id1 id2)

                                   (if (and (symbol? switchID)
                                            (symbol? id1)
                                            (symbol? id2))
                            
                                       (if (send railmanager isSwitch? switchID)
                                
                                           (if (and (validConnection? switchID id1)
                                                    (validConnection? switchID id2))
                                    
                                               (let ([switch (send railmanager getSwitch switchID)]
                                                     [obj1 'none]
                                                     [obj2 'none])

                                                 (when (send railmanager isMember? id1)
                                                   (set! obj1 (send railmanager getObject id1)))
                                                 (when (send railmanager isMember? id2)
                                                   (set! obj2 (send railmanager getObject id2)))

                                                 (cond
                                         
                                                   ((eq? obj1 'none)(send switch setYConnection! 'none id2))
                                                   ((eq? obj2 'none (send switch setYconnection! id1 'none)))

                                                   (else (begin (send switch setYConnection! id1 id2)
                                                                (send obj1 setConnection! switchID)
                                                                (send obj2 setConnection! switchID)))))
                                    
                                               (error "FileReader% add! 'connectY!: No valid connection between the given objects."))
                                           (error "FileReader% add! 'connectY!: Contract violation expected a switch as parameter, recieved:" switchID))
                                       (error "FileReader% add! 'connectY!: Contract violation expected three symbols, recieved:" switchID id1 id2))))
                                 

                )))

      