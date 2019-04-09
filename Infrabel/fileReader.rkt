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

    (field [railmanager 'none])

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
          (parseFile(openFile path))
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
              (if (< (length lst) 3)    ; allow max 3 elements in the list  (make it a field)
                  (let ([func 'null]    ; saving the func and pms
                        [pm   'null]
                        [pmls '()]
                        [idx 0])
                
                    (for ([i lst])    
              
                      (let ([str (cadr i)]) ; (string->symbol str)])
                        
                       
                      (if (symbol? str)
                       
                          (if (= idx 0)            ; The first element must be an registered function
            
                              (if (member? str)
                                 
                                  (set! func str)
                              
                                  (error "FileReader% parseList: Given function does not exist, please add to the table, recieved:" str))
                              
                              (set! pm str))        ; Beginning from the second element is one of the pm's
                          
                          
                          (if (list? str)
                              (set! pmls str)
                              (error "FileReader% parseList: Expected a symbol or a list, recieved" str)))
                      (set! idx (+ idx 1))))
                    
                    (if (null? pmls)
                        (callFunction func pm)
                        (callFunction func pm pmls)))
                  
                  (error  "FileReader% pareseList: More elements are given then parsed" lst))
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
      (if (and (symbol? func)
               (symbol? objname))
          (if (not (null? connections))
              ((getFunc func) objname connections)                          
              ((getFunc func) objname))
          (error "FileReader callFunction: Contract violation expected two symbols and a list, recieved:" func objname connections)))


    ;TODO herbekijken van de functie + initialisatie van de objecten --> dit kan worden verbeterd. 
    ;----------------------------------------------------------------------
    ; Function: addBasicFunctions!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Add functions for ceating detection block, switches and tracks
    ;----------------------------------------------------------------------

    (define/public (addBasicFunctions!)
   
          (add! 'block% (lambda (name)
                                 (send railmanager createDetectionblock! name)
                                  (display "block created")))
                 
                 (add! 'connectBlockTrack (lambda (block track)
                                            (if (and (symbol? block)
                                                     (symbol? track))
                                                (if (and (send railmanager isTrack? track)
                                                         (send railmanager isBlock? block))
                                                    'body
                                                    (error "FileReader% 'connectBlockTrack: Contract violation, expected a track id and a block id recieved" track block))
                                                (error "FileReader% 'connectBlockTrack: Contract violation, expected symbols recieved" track block))))
                 (add! 'connect! (lambda (con1 con2)
                                  'body))
      
                 (add! 'switch% (lambda (name)
                                  (if (symbol? name)
                                          ; (list? connections))
                                      (begin
                                        (send railmanager createSwitch! name)
                                        (display "switch created")
                                      ;  (send railmanager initSwitch! name (cons 0 0) 'left 'left 10 (length connections))
                                      ;  (send (send railmanager getSwitch name) initconnections! connections))
                                        )
                                      (error "FileReader% 'switch%: Contract violation, expected symbol and list recieved:" name))))

                 (add! 'track% (lambda (name)
                                 (send railmanager createTrack! name)
                                 (display "track created"))))
                            ;     (send railmanager initTrack! name (cons 0 0) 0 0 0 ))))
      
    
    
    
    ))

