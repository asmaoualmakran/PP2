#lang racket

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

    (define/private (parseString string)
      )

    ))