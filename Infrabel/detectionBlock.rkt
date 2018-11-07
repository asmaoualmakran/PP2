#lang racket

(require racket/class data/heap)

; NOTE operations on binary heaps are not thread-safe

(provide Detectionblock%)

(define Detectionblock%
  (class object%
    (super-new)

    (field
     [ID              'uninitialised]
     [trackID         'uninitialised]
     [reservations    'uninitialised]
     [maxReservations 'uninitialised]
     [available       'uninitialised]
     [x               'uninitialised]
     [y               'uninitialised]
     [length          'uninitialised])


    (struct reservation (id [priority #:mutable]))  ; a struct defineing a reservation
    
    (define/public (initialised?)
      (and (not(eq? ID 'uninitialised))
           (not(eq? trackID 'uninitialised))
           (not(eq? reservations 'uninitialised))
           (not(eq? maxReservations 'uninitialised))
           (not(eq? available 'uninitialised))
           (not(eq? x 'uninitialised))
           (not(eq? y 'uninitialised))
           (not(eq? length 'uninitialised))))

    (define/public (setID! id)
      (if (and (symbol? id)
               (eq? ID 'uninitialised))
          (set! ID id)
          (error "Detectionblock% setID!: ID is already initialised, ID can't be reset")))

    (define/public (getID)
      (if (initialised?)
          ID
          (error "Detectionblock% getID: object is not initialised")))

    (define/public (setTrackID! id)
      (if (symbol? id)
          (set! trackID id)
          (error "Detectionblock% setTrackID!: contract violation expected symbol given" id)))


    (define/public (getTrackID)
      (if (initialised?)
          trackID
          (error "Detectionblock% getTrackID: object is not initialised")))

    (define/public (setState! id)
      (if (and(initialised?)
              (symbol? id))
          (set! available id)
          (error "Detectionblock% setState!: object is not initialised or contract violation, expected symbol received" id)))
          
  (define/public (getState)
    (if (initialised?)
        (available)
        (error "Detectionblock% getState: object is not initialised, initialise before use")))

   (define/public (setLocation! coordinate)
     (if (and (pair? coordinate)
              (number? (car coordinate))
              (number? (cdr coordinate)))
         (begin (set! x (car coordinate))
                (set! y (cdr coordinate)))
         (error "Detectionblock% setLocation!: contract violation, pair of numbers expected given" coordinate)))

  (define/public (getLocation)
    (if (initialised?)
        (cons x y)
        (error "Detectionblock% getLocation: object is not initialised, initialise before use")))

   (define/public (setLength! number)
     (if (number? number)
         (set! length number)
         (error "Detectionblock% setLength!: contract violation expected number received" number)))

   (define/public (getLength)
     (if (initialised?)
         length
         (error "Detectionblok% getLength: object is not initialised, initialise before use")))

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

    

    
    (define/public (initReservations!)
      (if (initialised?)
          (set! reservations (make-vector maxReservations))
          (error "Detectionblock% initReservations!: reservations already initialised")))
  
  ;helper function to find a free location in a vector
    (define (freeLocation vector)
      (let loop ([vec vector]
                [index 0])
        (if (< index (vector-length vec))
            (if (eq? null(vector-ref vec index))
            index
            (loop vec (+ index 1)))
            null)))

;helper function to execute a storeage move
    (define (storageMove vector size)
      (let ([new-vec (make-vector size)])
            (vector-copy! new-vec 0 vector 0 (vector-length vector))
            new-vec))   ;return the new vector
            
    
        

;    (define/public (addReservation id priority)
 ;     (if (initialised?)
  ;        (if (and (symbol? id)
   ;                (number? priority))
    ;          (reservations 

  ;  (define/public (removeReservation id)
      
    (define/private (resizeReservations! number)  ;interne procedure om vector te resizen en te moven van de interne data
      (if (and (initialised?)
               (eq? null (freeLocation reservations)))
         (begin (set! maxReservations (+ maxReservations number))
                (storageMove reservations maxReservations))
          (display "No resize needed enough space available")))
          
      
    

      ))