#lang racket

(require graph)

(provide RailwayGraph%)

(define RailwayGraph%
  (class object%
    (super-new)

    (field
     [adjMatrix 'none]
     [referenceArr 'none]
     [railGraph 'none]
     [matrixGen? #f]
     [refArrFill? #f])

    (define managerType 'object:RailwayManager%)

    ;---------------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;    boolean: boolean
    ;      Use: Determine wheter or not the object is initialised.
    ; Use: Determine wheter or not the object is initialised.
    ;---------------------------------------------------------------
    
    (define/public (initialised?)
      (and (not (eq? adjMatrix 'none))
           (not (eq? referenceArr 'none))
           (not (eq? railGraph 'none))
           (matrixGenerated?)
           (refArrFilled?)))
    
    ;------------------------------------------------------------------------------------
    ; Function: matrixGenerated?
    ; Parameters: n/a
    ; Output:
    ;    boolean: boolean
    ;      Use: Boolean to determine wheter or not the adjecency matrix is generated.
    ; Use: Determine wheter or not the adjecency matrix is generated.
    ;------------------------------------------------------------------------------------

    (define/public (matrixGenerated?)
      matrixGen?)

    (define/public (refArrFilled?)
      refArrFill?)

    ;---------------------------------------------------------------------
    ; Function: createAdjMatrix
    ; Parameters:
    ;       size: number
    ;         Use: The size of the adjecency matrix
    ; Output:
    ;    matrix: vector<vector>
    ;      Use: Nested vectors to represent a matrix
    ; Use: Create an adjecency matrix to use for generating the graph.
    ;---------------------------------------------------------------------

    (define/private (createAdjMatrix size)
      (if (number? size)
          (if (> size 0)
              (let ([vector (make-vector size)])
                (for ([i (in-range size)])
                  (vector-set! vector i (make-vector size)))
                vector)
              (error "RailwayGraph% makeAdjMatrix!: size is smaller than 0"))
          (error "RailwayGraph% makeAdjMatrix!: contract violation size is not a number")))


    ;----------------------------------------------------------------
    ; Function: createRefArray
    ; Parameter:
    ;      size: number
    ;       Use: The size of the to be create reference array.
    ; Output:
    ;      referenceArray: vector
    ;         Use: The created reference array
    ; Use: Create a reference array with a given size.
    ;----------------------------------------------------------------

    
    (define/private (createRefArray size)
      (make-vector size))

    ;----------------------------------------------------------------
    ; Function: loadRailway
    ; Parameters:
    ;      manager: object:RailwayManager%
    ;        Use: The manager used to manage the railway objects
    ; Output: n/a
    ; Use: Place all the railway object IDs into the referenceArray
    ;----------------------------------------------------------------

    (define/private (loadRailway manager refArr)
      (if (and(eq? (object-name manager) managerType)
              (vector? refArr)
              (eq? refArr referenceArr))
              
          (let ([tracks (send manager getAllTrackID)]
                [switches (send manager getAllSwitchID)])

            (arrayPlacer tracks refArr 0)
            (arrayPlacer switches refArr (length tracks)))
        
          (error "RailwayGraph%: Contract violation, given object is not a railwaymanager or the reference array is not initialised." )))

    ;---------------------------------------------------------------------------
    ; Function: arrayPlacer
    ; Parameters:
    ;     list: list
    ;       Use: The list were the elements need to be placed into a vector
    ;     array: vector
    ;       Use: The vector where the elements need to be placed in.
    ;     start: number
    ;       Use: The displacement to be used for writing into the array.
    ; Output: n/a
    ; Use: Copy elements form a list into an vector.
    ;----------------------------------------------------------------------------

    (define/private (arrayPlacer list array start)
      (for* ([index (in-range (length list))]  ;In parallel, make indexes and take elements from the list
             [elm list])
        (vector-set! array (+ start index) elm)))

    ;------------------------------------------------------
    ; Function: saveInMatrix!
    ; Parameters:
    ; Output: n/a
    ; Use: Save values into a given nested vector.
    ;------------------------------------------------------

    (define/private (saveInMatrix! i j val matrix)
      (if (and (number? i)
               (number? j)
               (vector? matrix))
          (if (and (< i (vector-length (vector-ref matrix i)))
                   (< j (vector-length (vector-ref (vector-ref matrix i) i))))
              (vector-set! (vector-ref matrix i) j val)
              (error "RailwayManager%: Index out of bounds, i and j are not in range of the matrix"))
          (error "RailwayManager%: Contract violation i and j are not a number or matrix is not a vector")))

    ;------------------------------------------------------------------------------------
    ; Function: pushRailway!
    ; Parameters:
    ; Output: n/a
    ; Use: Place the connections between the railway objects into the adjacency matrix.
    ;------------------------------------------------------------------------------------
   
    (define/private (pushRailway! manager arr adjMat)
      (if (and (refArrFilled?)
               (matrixGenerated?)
               (eq? (object-name manager) managerType))
      (for* ([index (in-range (length referenceArr))]
             [elm (in-vector arr)])
           (cond ((eq? index (vector-member elm arr)) (vector-set! (vector-set! adjMat index) index)) ;use helper function
                 )
                  
            )
      (error "RailwayGraph%: Contract violation refArray not filled or AdjMatrix not generated or manager is not a railwaymanager")))

    ; create the matrix graph (use the adjMatrix as parameter)
    (define/private (commitRailway)
      'test
      ) 
 
    

    ))