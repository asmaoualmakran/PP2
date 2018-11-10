#lang racket

(require racket/class data/heap)
(require "infrastructure.rkt")

; NOTE operations on binary heaps are not thread-safe
; NOTE overerven van infrastructure
(provide Detectionblock%)

(define Detectionblock%
  (class Infrastructure%
    (super-new)

    (field
     
     [trackID         'uninitialised]
     [reservations    'uninitialised]   ; functies voor schrijven
     [maxReservations 'uninitialised]   ;getters en setters voor schrijven
     [length          'uninitialised])

    (inherit/super  setMaximalConnections!)
    (inherit/super setSpeedlimit!)


    ;private function to make sure all the fields are initialised.
    (define (init)
    (setSpeedlimit! 0)
    (setMaximalConnections! 0))

    (init)
   
    (struct reservation (id [priority #:mutable]))  ; a struct defineing a reservation

   (define (initialised?)
     (super initialised?)                     ;useing the initialised? predicate from the super class
     (and (not(eq? trackID 'uninitialised))
          (not(eq? reservations 'uninitialised))
          (not(eq? maxReservations 'uninitialised))
          (not(eq? length 'uninitialised)))
     (inner (false) initialised?))
    (overment initialised?)          ;both the current as the superclass implementation needs to be used.


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
      (if (initialised?)
          trackID
          (error "Detectionblock% getTrackID: object is not initialised")))


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
         (error "Detectionblock% setLength!: contract violation expected number received" number)))

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

;---------------------------------------------------------------------------------
; Function: initReservations!
; Parameter: n/a
; Output: n/a
; Use: Initialising reservations with the array where the reservations are kept.
;---------------------------------------------------------------------------------

    (define/private (initReservations!)
      (if (not (eq? maxReservations 'uninitialised))   ; not using initialised? this will be false if the reservations can't be initialised.
          (set! reservations (make-vector maxReservations))
          (error "Detectionblock% initReservations!: maxReservations is not initialised, please initialse before use")))

;-------------------------------------------------------------------
; Function: freeLocation
; Parameter:
;    vector: vector
;      Use: The vector where the reservations are kept.
; Output:
;    index: number
;     Use: An index were there is a free location in the vector.
; Use: Search for a free location in the vector.
;-------------------------------------------------------------------
   
  ;helper function to find a free location in a vector
    (define/private (freeLocation vector)
      (let loop ([vec vector]
                [index 0])
        (if (< index (vector-length vec))
            (if (eq? null(vector-ref vec index))
            index
            (loop vec (+ index 1)))
            null)))

;helper function to execute a storeage move
    (define/private (storageMove vector size)
      (let ([new-vec (make-vector size)])
            (vector-copy! new-vec 0 vector 0 (vector-length vector))
            new-vec))   ;return the new vector

    (define/private (resizeReservations! number)
      (if (< maxReservations number)
          (set! maxReservations number)
          (error "DetectionBlock% resizeReservations! given parameter is smaller than the current maxReservations" number)))

    
        

;    (define/public (addReservation id priority)
 ;     (if (initialised?)
  ;        (if (and (symbol? id)
   ;                (number? priority))
    ;          (reservations 

  ;  (define/public (removeReservation id)
      ;TODO resizen is niet altijd groter dit kan ook kleiner ->test
 ;   (define/private (resizeReservations! number)  ;interne procedure om vector te resizen en te moven van de interne data
  ;    (if (and (initialised?)
   ;            (eq? null (freeLocation reservations)))
    ;     (begin (set! maxReservations (+ maxReservations number))
     ;           (storageMove reservations maxReservations))
      ;    (display "No resize needed enough space available")))

    ;-------------------------------------------------------------------------------------------------
    ;TODO check if you need to use a vector as datastructure and use heap sort to order it. (enables abuse of intern deleteing an element)

    ;    (define/public (initReservations!)
    ;     (struct node (name val))
    ;    (let ((node<=? ((lambda (x y)         ;helper function to define the order between priorities
    ;                      (<= (cdr x)(cdr y))))))
    ;      (set! reservations (make-heap node<=?))))    ; initialing the reservations to a heap.


    ;  (define/public (addReservation! id number)  ;reservation needs to be a struct of a symbol and a number.
    ;    (if (and(symbol? id)
    ;            (number? number))                 ; contract check
          
    ;        (if (initialised?)                    ; initialisation check 
    ;            (heap-add! reservations (cons id number))
    ;            (error "Detectionblock% addReservation!: reservations not initialised please init before use"))
    ;        (error "Detectionblock% addReservation!: contract violation expected symbol and number received" id number)))

    ;  (define/public (removeReservation! id number)
    ;   (if (and (symbol? id)
    ;           (number? number))
    ;     (if (initialised?)
    ;        (
    ;--------------------------------------------------------------------------------------------------

      ))