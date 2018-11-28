#lang racket

(require racket/class)
(provide CommunicationManager%)

;------------------------------------------------------------
; Class: CommunicationManager%
; Parameters: n/a
; Use: Enable two API's to communicate in an uniform manner.
;      Also keeps the relation between two objects.
;------------------------------------------------------------

(define CommunicationManager%
  (class object%
    (super-new)

    (field [obj1 'none]
           [obj2 'none])

    ;----------------------------------------------------------------------
    ; Function: connected?
    ; Parameters: n/a
    ; Output:
    ;    boolean: boolean
    ;      Use: Determine if there is a living connection.
    ; Use: Determine if there is a living connection between two objects.
    ;----------------------------------------------------------------------

    (define (connected?)
      (and (not (eq? 'none obj1))
           (not (eq? 'none obj2))))

    ;---------------------------------------------------------
    ; Function: connect!
    ; Parameters:
    ;      self: object:*
    ;        Use: The object that wants to connect.
    ; Output: n/a
    ; Use: Safe an object in the communication manager.
    ;---------------------------------------------------------

    (define/public (connect! self)
      (if (not (or (eq? obj1 self)
                   (eq? obj2 self)))
          (cond ((eq? obj1 'none)(set! obj1 self))
                ((eq? obj2 'none)(set! obj2 self))
                (else (error "CommunicationManager% connect!: there is no connection available")))
          (error "CommunicationManager% connect!: you are already connected")))

    ;---------------------------------------------------------
    ; Function: disconnect!
    ; Parameters:
    ;       self: object:*
    ;        Use: The object that get's disconnected.
    ; Output: n/a
    ; Use: Delete an object from the communication manager.
    ;---------------------------------------------------------
    
    (define/public (disconnect! self)
      (cond ((eq? obj1 self)(set! obj1 'none))
            ((eq? obj2 self)(set! obj2 'none))
            (else "CommunicationManager% disconnect!: you were not connected, cannot disconnect")))

    ;-------------------------------------------------------------------
    ; Function: getConnected
    ; Parameters:
    ;      self: object:*
    ;       Use: The object that wants to get it's connected object.
    ; Ouput:
    ;    object: object:*
    ;      Use: The objecte where the asking object is connected to.
    ; Use: Retrieve the connected object of an object.
    ;-------------------------------------------------------------------

    (define/public (getConnected self)
      (if (connected?)
          (if (eq? self obj1)
              obj2
              obj1)
              (error "CommunicationManager% getConnected: you are not connected")))

    ;-------------------------------------------------------------------------
    ; Function: receiveMessage
    ; Parameters:
    ;     recipient: object:*
    ;       Use: The recieving object of the message.
    ;     message: function
    ;       Use: A function that the recipient supports.
    ;     args: any
    ;       Use: The optional parameters that a function expects.
    ; Output:
    ;     return: any
    ;       Use: Any optional return value of an executed function.
    ; Use: Let an object execute one of it's functions/ methods.
    ;------------------------------------------------------------------------

    (define (receiveMessage recipient message . args)
      (if (null? args)
          (send recipient message)
          (send recipient message . args)))

    ;-----------------------------------------------------------------------
    ; Function: sendMessage
    ; Parameters:
    ;     self: object:*
    ;       Use: The sendig object of the message.
    ;     message: function
    ;       Use: A function that the recipient supports.
    ;     args: any
    ;       Use: The optional parameters that a function expects.
    ; Output:
    ;     return: any
    ;       Use: Any optional return value of an executed function.
    ; Use: Let a connected object execute one of it's supported functions.
    ;-----------------------------------------------------------------------
    
     (define/public (sendMessage self message . args)
     (if (connected?)
         (receiveMessage (getConnected self))
      (error "CommunicationManager% sendMessage: you are not connected")))
    
    ))