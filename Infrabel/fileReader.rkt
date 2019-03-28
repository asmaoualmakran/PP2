#lang racket

(require dyoo-while-loop)

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
      (not (eq? railmanager 'none)))

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
    ;       key; string
    ;        Use: The key that will be used for the hashing.
    ;       function: #<procedure>
    ;        Use: The function that needs to be added.
    ; Output: n/a
    ; Use: Save a new function into the hashtable
    ;----------------------------------------------------------

    (define/public (add! key function)
      (if (and (string? key)
               (procedure? function))
          (if (not (hash-has-key? functionHash key))
              (hash-set! functionHash key function)
              (error "FileReader% add!: Given key is already member of the hashtable, recieved:" key))
          (error "FileReader% add!: Contract violation, expected a string and a function recieved:" key function)))

    ;-----------------------------------------------------------------------
    ; Function: update!
    ; Parameters:
    ;        key: string
    ;         Use: The key of the function that needs to be updated.
    ;        function: #<procedure>
    ;         Use: The new definition of the the to be updated function.
    ; Output: n/a
    ; Use: Update an existing function with a new definition.
    ;-----------------------------------------------------------------------
    
    (define/public (update! key function)
      (if (and (string? key)
               (procedure? function))
          (if (member? key)
               (hash-set! functionHash key function)
              (error "FileReader% update!: Given key does not belong to a function, can not update the function, recieved:" key))
          (error "FileReader% update!: Contract violation expected a string and a procedure, recieved" key function)))
               

    ;----------------------------------------------------------------
    ; Function: delete!
    ; Parameters:
    ;       key: string
    ;        Use: The key of the function that needs to be deleted.
    ; Output: n/a
    ; Use: Delete a function using it's key.
    ;----------------------------------------------------------------

    (define/public (delete! key)
      (if (string? key)
          (if (hash-has-key? functionHash key)
              (hash-remove! functionHash key)
              (error "FileReader delete!: Hashtable does not contain given key, recieved:" key))
          (error "FileReader% delete!: Contract violation, expected a string recieved:" key)))

    ;---------------------------------------------------------------------
    ; Function: member?
    ; Parameters:
    ;        key: string
    ;         Use: The key of the function that needs to be checked.
    ; Output:
    ;        boolean: boolean
    ;         Use: Determine if the function is in the hashtable.
    ; Use: Determine if the given key of a function is in the hashtable
    ;---------------------------------------------------------------------
    
    (define/public (member? key)
      (if (string? key)
          (hash-has-key? functionHash key)
          (error "FileReader% member?: Contract violation, expected a string recieved:" key)))


    ;-------------------------------------------------------------------
    ; Function: getFunc
    ; Parameters:
    ;         key: string
    ;          Use: The key used to hash the function in the hashtable
    ; Output:
    ;         func: #<procedure>
    ;          Use: The function that needs to be retrieved
    ; Use: Retrieve a function from the hashtable using it's key.
    ;-------------------------------------------------------------------
    
    (define/public (getFunc key)
      (if (string? key)
          (if (member? key)
              (hash-ref functionHash key)
              (error "FileReader%: Hashtable has no such key, recieved:" key))
          (error "FileReader% getKey: Contract violation, expected a string recieved:" key)))
          

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

    (define file  "C:\\Users\\Asma\\GitHub\\PP2\\railwaySetup\\hardware.rkt")
    (define/public (loadRailway ) ;file)
      (readFile file))

    ;---------------------------------------------------------------
    ; Function: readFile
    ; Parameters:
    ;         file: string
    ;          Use: The path location of the to be read file
    ; Output: n/a
    ; Use: Read the file, parse it and call the correct functions
    ;---------------------------------------------------------------
    
    (define/private (readFile file)
      (define in (open-input-file file))
      (for ([l (in-lines in)])
        (callFunction(parseString l))))

    ;-----------------------------------------------------------------------------------
    ; Function: parseString
    ; Parameters:
    ;        string: string
    ;          Use: The string that needs to be parsed.
    ;        manager: object: RailwayManager%
    ;          Use: The railway manager that manages the railway at that time.
    ; Output: n/a 
    ; Use: Parse a string and call the appropriate function, determined by parsing it.
    ;-----------------------------------------------------------------------------------

    (define/private (parseString string)
      (for ([char string])
        (let ([count 0]
              [i 0]
              [j 0]
              [result '()])
          (if (eq? (vector-ref string i) "(")
              (begin (set! count (+ count 1))   ;initialisation of the counters an pointers.
                     (set! i (+ i 1))

                     (while (> count 0)       ;loop where the indexes and counters are manipulated
                            (if (not (eq? (vector-ref string j) " "))    ;TODO, refine the situations!!!!!!!
                                (set! j (+ j 1))
                                (set! result (result append (substring string i j))))))
              (error "FileReader% parseString: No opening ( in expression, it is not a function call expected function call."))
          (if (not (null? result))
              (callFunction result)
              (error "FileReader% parseString: Variable result is an empty list, no function to be called")))))

    ;----------------------------------------------------------------------------------------
    ; Function: callFunction
    ; Parameters:
    ;        lst: list<string>
    ;         Use: The list that contains the parsed string which contains a function call.
    ; Output: n/a
    ; Use: Call the correct functions using the given string list
    ;----------------------------------------------------------------------------------------
              
    (define/private (callFunction lst)
      (if (member? (car lst))
          (let ([func (getFunc (car lst))]
                [name (cadr lst)]
                [connections cddr])
            (getFunc name railmanager . connections))
          (error "FileReader% callFunction: Given hashtable does not contain the given key, recieved:" (car lst)) ))

    ;----------------------------------------------------------------------
    ; Function: addBasicFunctions!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Add functions for ceating detection block, switches and tracks
    ;----------------------------------------------------------------------

    (define/public (addBasicFunctions!)
      (if (initialised?)
      (begin(add! "block%" (lambda (name railmanager)
                      (send railmanager createBlock! name)))
      
      (add! "switch%" (lambda (name railmanager connections)
                        (send railmanager createSwitch! name)
                        (send railmanager initSwitch! name (cons 0 0) 'left 'left 10 (length connections))
                        (send (send railmanager getSwitch name) initconnections! connections)))

      (add! "track%" (lambda (name railmanager connections)
                       (send railmanager createTrack! name connections)
                       (send railmanager initTrack! name (cons 0 0) 0 0 0 ))))
      
      (error "FileReader% addBasicFunctions!: Object is not initialised, please initialise before use")))
    
    
    
    ))

