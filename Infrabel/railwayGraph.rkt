#lang racket

(require graph)

(provide RailwayGraph%)

(define RailwayGraph%
  (class object%
    (super-new)

    ;---------------------------------------------------------------------
    ; Function: makeAdjMatrix!
    ; Parameters:
    ;       size: number
    ;         Use: The size of the adjecency matrix
    ; Output:
    ;    matrix: vector<vector>
    ;      Use: Nested vectors to represent a matrix
    ; Use: Create an adjecency matrix to use for generating the graph.
    ;---------------------------------------------------------------------

    (define/private (makeAdjMatrix! size)
      (if (number? size)
          (if (> size 0)
              (let ([vector (make-vector size)])
              (for/vector ([i (in-range size)])
                (vector-set! vector i (make-vector size)))
                vector)
              (error "RailwayGraph% makeAdjMatrix!: size is smaller than 0"))
          (error "RailwayGraph% makeAdjMatrix!: contract violation size is not a number")))




              ))