#lang racket

(require racket/class data/heap)
;(require "infrastructure.rkt")

; NOTE operations on binary heaps are not thread-safe
; NOTE overerven van infrastructure
(provide Detectionblock%)

(define Detectionblock%
  (class object%
    (super-new)

    (inherit-field ID)
    (inherit setID! getID)

    
    (field

     [trackID         'uninitialised]
     [reservations    'uninitialised]   ; functies voor schrijven
     [maxReservations 'uninitialised]   ;getters en setters voor schrijven
     [length          'uninitialised])

   
    (struct reservation (id [priority #:mutable]))  ; a struct defineing a reservation

    (define/public (initialise!)
      (setTrackID! 'none)
      (setMaxReservations! 10)
      (initReservations!)
      (setLength! 0))

    ;-------------------------------------------------------
    ; Function: initialised?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;       Use: Determine if the object is initialised.
    ; Use: Check if the object is initialised.
    ;-------------------------------------------------------
    
    (define/public (initialised?)
      (and
       (not (eq? trackID 'uninitialised))
       (not (eq? reservations 'uninitialised))
       (not (eq? maxReservations 'uninitialised))
       (not (eq? length 'uninitialised))))

    ;-----------------------------------------------------------------------------------
    ; Function: setTrackID
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the track where the detectionblock is located.
    ; Output: n/a
    ; Use: Set the identification of the track where the detectionblock is located on.
    ;-----------------------------------------------------------------------------------
    
    (define/public (setTrackID! id)
      (if (symbol? id)
          (set! trackID id)
          (error "Detectionblock% setTrackID!: contract violation expected symbol given" id)))

    ;---------------------------------------------------------------------------------
    ; Function: getTrackID
    ; Parameters: n/a
    ; Output:
    ;   trackID: symbol
    ;     Use: The identification of the track where the block is located on.
    ; Use: Retrieving the identification of the track where the block is located on.
    ;---------------------------------------------------------------------------------

    (define/public (getTrackID)
      (if (and(initialised?)
              (hasTrack?))
          trackID
          (error "Detectionblock% getTrackID: object is not initialised" trackID)))

    ;-----------------------------------------------------------
    ; Function: hasTrack?
    ; Parameters: n/a
    ; Output:
    ;     boolean: boolean
    ;       Use: Determine if a detectionblock is on a track.
    ; Use: Deterimine if a detectionblock is on a track.
    ;-----------------------------------------------------------
   
    (define/public (hasTrack?)
      (and(initialised?)
          (not (eq? trackID 'none))))
         
    ;----------------------------------------------------------
    ; Function: deleteTrackID!
    ; parameters: n/a
    ; output: n/a
    ; Use: Delete the identification of the associated track.
    ;----------------------------------------------------------

    (define/public (deleteTrackID!)
      (if (initialised?)
          (set! trackID 'none)
          (error "Detectionblock% deleteTrackID: Object is not initialised please initialise before use.")))

    ;--------------------------------------------------------------------
    ; Function: isPlaced?
    ; Parameters: n/a
    ; Ouput:
    ;    boolean: boolean
    ;      Use: Boolean to determine if it's already placed.
    ; Use: Check whether or not the detectionblock is placed on a track.
    ;--------------------------------------------------------------------
    
    (define/public (isPlaced?)
      (if (initialised?)
      (not (eq? (getTrackID 'none)))
      (error "Detectionblock% isPlaced: Object is not initialised please initialise before use.")))

    ;----------------------------------------------------------------------
    ; Function: setLength!
    ; Parameters:
    ;   number: number
    ;     Use: The lenght where the detecionblock has control over.
    ; Output: n/a
    ; Use: Setting the lenght where the detectionblock has control over.
    ;----------------------------------------------------------------------

    (define/public (setLength! number)
      (if (number? number)
          (set! length number)
          (error "Detectionblock% setLength!: contract violation expected number received:" number)))

    ;-------------------------------------------------------------------------
    ; Function: getLength
    ; Parameters: n/a
    ; Output:
    ;  length: number
    ;    Use: the length where the detectionblock has control over.
    ; Use: Retrieving the length where the detectionblock has control over.
    ;-------------------------------------------------------------------------

    (define/public (getLength)
      (if (initialised?)
          length
          (error "Detectionblok% getLength: object is not initialised, initialise before use")))

    ;-----------------------------------------------------------------------
    ; Function: setMaxReservations!
    ; Parameters:
    ;       res: number
    ;        Use: The number of reservations the detectionblok can take.
    ; Output: n/a
    ; Use: Set the number of reservations the detectionblok takes.
    ;-----------------------------------------------------------------------
    
    (define/public (setMaxReservations! res)
      (if (and(number? res)
              (eq? maxReservations 'uninitialised))
          (begin (set! maxReservations res)
                 (initReservations!))
          (error "Detectionblock% setMaxReservations!: contract violation expected number recieved" res)))

    ;------------------------------------------------------------------------------
    ; Function: getMaxReservations
    ; Parameters: n/a
    ; Output:
    ;     maxReservations: number
    ;         Use: The maximum reservations the detectionblock can take
    ; Use: Retrieve the number of reservations that the detectionblock can take.
    ;------------------------------------------------------------------------------
    
    (define/public (getMaxReservations)
      (if (initialised?)
          maxReservations
          (error "Detectionblock% getMaxReservations: objects is not initialised please initialise before use")))

    ;---------------------------------------------------------------------------------
    ; Function: initReservations!
    ; Parameter: n/a
    ; Output: n/a
    ; Use: Initialising reservations with the array where the reservations are kept.
    ;---------------------------------------------------------------------------------

    (define/private (initReservations!)
      (if (not (eq? maxReservations 'uninitialised))   ; not using initialised? this will be false if the reservations can't be initialised.
          (set! reservations (make-heap order))
          (error "Detectionblock% initReservations!: maxReservations is not initialised, please initialse before use")))

    ;------------------------------------------------------------
    ; Function: order
    ; Parameters:
    ;       pair1: pair
    ;       pair2: pair
    ; Output: boolean
    ; Use: Boolean to define the order between two pairs.
    ;------------------------------------------------------------

    (define (order pair1 pair2)
      (if (and(pair? pair1)
              (pair? pair2))
          (if (and (number? (car pair1))
                   (number? (car pair2)))
              (<= (car pair1)(car pair2))
              (error "Detectionblock% order: contract violation car of pairs are not a number"))
          (error "Detectionblock% order: contract violation given parameters are not a pair")))

    ;-----------------------------------------------------
    ; Function: makeNode
    ; Parameters:
    ;       number: number
    ;       val   : symbol
    ; Output: pair
    ; Use: A pair consisting of a number and a symbol.
    ;-----------------------------------------------------
    
    (define/private (makeNode number val)
      (if (and (number? number)
               (symbol? val))
          (cons number val)
          (error "Detectionblock% makeNode: contract violation given parameters are not a number and a value")))

    ;----------------------------------------------------
    ; Function: heapEmpty?
    ; Parameters: n/a
    ; Output: boolean
    ; Use: Determine if the reservations heap is empty.
    ;----------------------------------------------------

    (define/private (heapEmpty?)
      (=(heap-count reservations)0))

    ;------------------------------------------------------
    ; Function: heapFull?
    ; Parameters: n/a
    ; Output: boolean
    ; Use: Determine if the reservations heap is full.
    ;------------------------------------------------------
    (define/private (heapFull?)
      (= (heap-count reservations) maxReservations))

    ;----------------------------------------------
    ; Function: reserve!
    ; Parameters:
    ;        number: number
    ;        val   : symbol
    ; Output: n/a
    ; Use: Add a reservation to the heap.
    ;----------------------------------------------
      
    (define/public (reserve! number val)
      (if (and (number? number)
               (symbol? val))
          (if (not (heapFull?))
              (heap-add! reservations (makeNode number val))
              (error "Detectionblock% reserve!: can not add reservation, reservation has reached it's maxiumum"))
          (error "Detectionblock% reserve!: contract violation given parameters are not a number and a value")))

    ;----------------------------------------------------------------
    ; Function: getTopReservation
    ; Parameters: n/a
    ; Output:
    ;    node: pair
    ; Use: Get the top reservation of the heap without removing it.
    ;----------------------------------------------------------------

    (define/public (getTopReservation)
      (if (not (heapEmpty?))
          (heap-min reservations)
          (error "Detectionblock% getTopReservation: heap is empty can not get reservation")))

    ;----------------------------------------------------
    ; Function: serverReservation
    ; Parameters: n/a
    ; Output:
    ;    node: pair
    ; Use: Remove the top node of the reservation heap
    ;----------------------------------------------------

    (define/public (serveReservation)
      (if (not (heapEmpty?))
          (begin (heap-min reservations)
                 (heap-remove-min! reservations))
          (error "Detectionblock% serveReservation: heap is empty can not serve the reservations")))

    ))
