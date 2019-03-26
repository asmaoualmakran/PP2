#lang racket

(require dyoo-while-loop)

(require "railwayManager.rkt")

(provide FileReader%)

(define FileReader%
  (class object%
    (super-new)

    
   

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

 ;   (define/private (parseString string)
 ;     (let ([count 0]
 ;           [i 0]
 ;           [j 0])
 ;       (for ([s string])
 ;            (while (and (> count 0) (>= i 0))  ;i >= 0 so that the loop will start 
 ;                   (cond ((eq? (vector-ref string i) "(") (begin (set! count (+ count 1))  ; if you read a ( or ) skip index, add or substract counter
 ;                                                                 (set! i (+ i 1))))
 ;                         ((eq? (vector-ref string i) ")") (begin (set! count (- count 1))  ; --> ) means also end of word, not only expression 
 ;                                                                 (set! i (+ i 1))))
 ;                         ((eq? (vector-ref string j) "(") (begin (set! count (+ count 1))
 ;                                                                 (set! j (+ j 1))))
 ;                         ((eq? (vector-ref string j) ")") (begin (set! count (- count 1))
 ;                                                                 (set! j (+ j 1))))
 ;                         
 ;                         (else (error "FileReader% parseString: invalid expression, cannot be parsed")))
 ;            
 ;       ))))

    ;-----------------------------------------------------------------------------------
    ; Function: parseString
    ; Parameters:
    ;        string: string
    ;          Use: The string that needs to be parsed.
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
                            (if (not (eq? (vector-ref string j) " "))
                                (set! j (+ j 1))
                                 (set! result (result append (substring string i j))))))
              (error "FileReader% parseString: No opening ( in expression, it is not a function call expected function call.")))))

    ;--------------------------------------------------------------
    ; Function: callFunction
    ; Parameters:
    ;        lst: list<string>
    ; Output: n/a
    ; Use: Call the correct functions using the given string list
    ;--------------------------------------------------------------
              
    (define/private (callFunction lst)
      'test)
      ))

    