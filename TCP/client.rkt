#lang racket

(require racket/tcp)
(require logger)
(require racket/serialize)

(provide Client%)

;--------------------------
; Class: Client%
; Parameters: n/a 
; Output: object:Client%
; Use: A TCP client.
;--------------------------

(define Client%
  (class object%
  (super-new)

  (field [host "localhost"]
         [port 9883]
         [input  'uninitialised]
         [output 'uninitialised])

    ;-----------------------------
    ; Function: connect! 
    ; Parameters: n/a 
    ; Output: n/a 
    ; Use: Connect to a server
    ;-----------------------------     

    (define/public (connect!)
      (if (not (connectionActive?))
            (set!-values (input output) (tcp-connect host port))  
          (error "Client% connect!: There is already a connection active, please disconnect before connecting.")))     

    ;-------------------------------------------------------------------------
    ; Function: connectionActive? 
    ; Parameters: n/a 
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if the client has an active connection.
    ; Use: Determine if the client has an active connection with the server.
    ;-------------------------------------------------------------------------

    (define/public (connectionActive?)
      (and (inputPortOpen?)
           (outputPortOpen?)))

    ;-----------------------------------------------------------------------
    ; Function: inputPortOpen? 
    ; Parameters: n/a 
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if there is an active input port.
    ; Use: Determine if there is an active input port and if it is active.
    ;-----------------------------------------------------------------------

    (define/public (inputPortOpen?)
      (port? input))

    ;--------------------------------------------------------------------
    ; Function: getInputPort
    ; Parameters: n/a 
    ; Output: 
    ;     port: input-port
    ;       Use: The input port used for communication with the server.
    ; Use: Getting the input port for communication with the server.
    ;--------------------------------------------------------------------

    (define/public (getInputPort)
      (if (inputPortOpen?)
        input
        (error "Client% getInputPort: There is no input port active.")))

    ;---------------------------------------------------------
    ; Function: outputPortOpen? 
    ; Parameters: n/a 
    ; Output: 
    ;     boolean: boolean
    ;       Use: Determine if there is an active output port.
    ; Use: Determine if there is an active output port.
    ;----------------------------------------------------------

    (define/public (outputPortOpen?)
      (port? output))

    ;--------------------------------------------------
    ; Function: getOutputPort
    ; Parameters: n/a 
    ; Output: 
    ;     output: output port
    ;       Use: The output port used.
    ; Use: Getting the output port used by the client.
    ;---------------------------------------------------

    (define/public (getOutputPort)
      (if (outputPortOpen?)
        output
        (error "Client% getOutputPort: There is no output port active.")))

    ;------------------------------------------------------
    ; Function: closeConnection!
    ; parameters: n/a 
    ; Output: n/a 
    ; Use: Close the active input port and output port.
    ;------------------------------------------------------

    (define/public (closeConnection!)
      (if (and (inputPortOpen?)
               (outputPortOpen?))
          (begin 
            (close-input-port (getInputPort))
            (set! input 'none)
            (info "Client% closeConnection!: Input port closed.")
            (close-output-port (getOutputPort))
            (set! output 'none)
            (info "Client closeConnection!: Output port closed."))
        (error "Client% closeConnection!: There is no output port and input port active.")))

    ;--------------------------------------------------------------------
    ; Function: writeOutput
    ; Parameters: 
    ;         data: any
    ;           Use: The data that must be written on the output port.
    ; Output: n/a 
    ; Use: Serialize and wirte data to the output port.
    ;--------------------------------------------------------------------

    (define/public (writeOutput data)
      (if (connectionActive?)
        (if (serializable? data)
              (let ([serData (serialize data)])
                (write serData output)
                (flush-output output)
                (display "data written"))
          (error "Client% writeOutput: The given data is not serializable, recieved: " data))
        (error "Client% writeOutput: No connection active.")))

    ;------------------------------------------
    ; Function: readInput
    ; Parameters: n/a 
    ; Output: 
    ;     inputData: any serializable data.
    ;       Use: The data that is recieved.
    ; Use: Read data for the input port.
    ;------------------------------------------
  
    (define/public (readInput)
      (let ([readData (read input)]
            [deserialzed 'none])
        (set! deserialzed (deserialize readData))
        deserialzed))
  
  ))