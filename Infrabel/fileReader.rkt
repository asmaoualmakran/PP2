#lang racket

(require dyoo-while-loop)

(require "railwayManager.rkt")

(provide FileReader%)

(define FileReader%
  (class object%
    (super-new)

    ; Hashstable used to save functions
    
    (define functionHash (make-hash))  

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
          (if (not (hash-has-key? key))
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
          (if (hash-has-key? functionHash key)
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

    (define/public (loadRailway file) ;manager)
      (define in (open-input-file file))  ;no flags needed the default mode is sufficient
      (readFile file)
      
      )


    (define/private (readFile file)
      (define in (open-input-file file))
      (for ([l (in-lines in)])
        (display l)
        (newline)))



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

    (define/private (parseString string manager)
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
              
    (define/private (callFunction lst manager)
      (if (and (not (null? lst))
               (list? lst))
          (let ([function 0]
                [pms '()])   ;Keep the parameters in a list
            (for ([elm lst])
              (cond ((eq? (car lst)) 'do)
                    (else (error "FileReader% callFunction: procedure is unknown cannot execute"(car lst))))))
          (error "FileReader% callFunction: Contract violation expected non-empty list recieved"lst)))

    
    
    
    ))

