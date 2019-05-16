#lang racket

(require racket/tcp)
(require logger)
(require racket/serialize)

(provide Server%)

;----------------------------------------
; Class: Server%
; Parameters: n/a 
; Output: object:Server%
; Use: A single connection TCP server.
;----------------------------------------

(define Server%
  (class object%
  (super-new)


(field [listener    'uninitialised]
       [input       'uninitialised]
       [output      'uninitialised]
       [port        9883]
       [maximalConnections 1])

  ;------------------------------------
  ; Function: getPortnr
  ; Parameters: n/a 
  ; Output:
  ;     portnr: number
  ;       Use: The used port number.
  ; Use: Get the used port number.
  ;------------------------------------

  (define/public (getPortnr)
    (if (number? port)
        port
      (error "Server% getPortnr: There is no port number set.")))

  ;----------------------------------------------------------------------------
  ; Function: setPort!
  ; Parameters: 
  ;       portnr: number
  ;         Use: The port number that needs to be used for the connections.
  ; Output: n/a 
  ; Use: Setting the port used for establishing connections.
  ;----------------------------------------------------------------------------

  (define/public (setPort! portnr)
    (if (not (connectionActive?))
      (if (number? portnr)
        (if (> portnr 1000)
          (begin 
            (set! port portnr)
            (info "Server% setPort!: Port number is set to: " portnr))
        (error "Server setPort!: Contract violation, given port number must be larger than 1000, recieved: " portnr))
      (error "Server% setPort!: Given port is not a number, it cannot be set."))
    (error "Server% setPort!: There is an connection active, port number cannot be altered.")))

  ;-----------------------------------------------------------------
  ; Function: getMaximalConnections
  ; Parameters: n/a 
  ; Output: 
  ;      maximalConnections: number
  ;       Use: The maximal number of connections that can be made.
  ;-----------------------------------------------------------------

  (define/public (getMaximalConnections)
    (if (number? maximalConnections)
        maximalConnections
        (error "Server getMaximalConnections: Maximalconnections is not a number.")))

  ;---------------------------------------------------------------------------
  ; Function: setMaximalConnections!
  ; Parameters: 
  ;       numb: number
  ;         Use: The number where the maximal connections needs to be set to.
  ; Output: n/a 
  ; Use: Set the number of maximal connections.
  ;---------------------------------------------------------------------------

  (define/public (setMaximalConnections! numb)
    (if (number? numb)
      (if (> numb 0)
        (set! maximalConnections numb)
      (error "Server% setMaximalConnections!: Given number is not larger than 0, recieved: " numb))
    (error "Server% setMaximalConnection!: Contract violation expected a number, recieved: " numb)))
  ;-----------------------------------------------------
  ; Function: listenerActive?
  ; Parameters: n/a 
  ; Output: 
  ;     boolean: boolean 
  ;      Use: Determine if there is a listener active.
  ; Use: Determine if there is a listener active.
  ;-----------------------------------------------------

  (define/public (listenerActive?)
    (tcp-listener? listener))

  ;-----------------------------------------------------
  ; Function: getListener
  ; Parameters: n/a 
  ; Ouput: 
  ;   listener: input-port, output-port
  ;     Use: Input and output port used as connection.
  ; Use: Get the used listener.
  ;-----------------------------------------------------

  (define/public (getListener)
    (if (listenerActive?)
          listener
          (error "Server% getListener: Object is not initialised, please initialise before use")))

  ;-----------------------------------------------------------
  ; Function: openConnection!
  ; Parameters: n/a   
  ; Output: n/a 
  ; Use: Open a server connection, and setting the listener
  ;-----------------------------------------------------------

  (define/public (openConnection!)
    (set! listener (tcp-listen port maximalConnections #t)))

  ;----------------------------------------------------------
  ; Function: connectionActive?
  ; Parameters: n/a
  ; Output: 
  ;      boolean: boolean
  ;       Use: Determine if there is an connection active.
  ; Use: Determine if there is an connection active.
  ;----------------------------------------------------------

  (define/public (connectionActive?)
    (and (listenerActive?)
         (inputPortOpen?)
         (outputPortOpen?)))

  ;-------------------------------------------
  ; Function: connect!
  ; Parameters: n/a
  ; Output: n/a 
  ; Use: Connect the server to a client.
  ;--------------------------------------------

  (define/public (connect!)
    (if (listenerActive?)
      (if (not (and (inputPortOpen?)
                    (outputPortOpen?)))
            (if (tcp-accept-ready? (getListener))
                (begin
                  (set!-values (input output) (tcp-accept (getListener)))
                  (info "Server% connect!: Client connection set." input output))
              (error "Server% connect!: There is no client waiting for connection"))      
          (error "Server% connect!: There are already ports open, please close the ports."))
        (error "Server% connect!: There is no listener active, it cannot be connected.")))
 
  ;-----------------------------------------------------------
  ; Function: closeConnection! 
  ; Parameters: n/a 
  ; Output: n/a 
  ; Use: Close the listener and the input and output ports.
  ;-----------------------------------------------------------

  (define/public (closeConnection!)
    (tcp-close (getListener))
    (set! listener 'none)
    (closePorts!))

  ;-------------------------------------------------
  ; Function: inputPortOpen? 
  ; Parameters: n/a 
  ; Ouput: 
  ;    boolean: boolean
  ;     Use: Determine if the input-port is open.
  ; Use: Determine if the input-port is open.
  ;-------------------------------------------------

  (define/public (inputPortOpen?)
    (port? input))

  ;-------------------------------------------------
  ; Function: getInputPort
  ; Parameters: n/a 
  ; Output:
  ;      port: input-port
  ;       Use: Port for incomming communiaction.
  ; Use: Getting the input-port if it is active.
  ;-------------------------------------------------

  (define/public (getInputPort)
    (if (inputPortOpen?)
        input
        (error "Server% getInputPort: There is no input port active")))

  ;----------------------------------------------------
  ; Function: outputPortOpen? 
  ; Parameters: n/a 
  ; Output: 
  ;      boolean: boolean 
  ;       Use: Determine if the output-port is open. 
  ; Use: Determine if the output-port is open.
  ;----------------------------------------------------

  (define/public (outputPortOpen?)
    (port? output))  
  
  ;------------------------------------------------
  ; Function: getOutputPort
  ; Parameters: n/a 
  ; Output: 
  ;      port: output-port 
  ;       Use: Port for outgoing communication.
  ; Use: Getting the output-port if it is active. 
  ;-------------------------------------------------

  (define/public (getOutputPort)
    (if (outputPortOpen?)
        output
        (error "Server getOutputPort: There is no output port active")))  

  ;----------------------------------------------------------
  ; Fuction: closePorts! 
  ; Parameters: n/a 
  ; Output: n/a 
  ; Use: Close the listener and the input and output port.
  ;----------------------------------------------------------

  (define/public (closePorts!)

    (if (inputPortOpen?)
        (begin
          (close-input-port (getInputPort))
          (set! input 'none))
    (info "Server% closePorts!: There is no input-port open, input-port cannot be closed."))

    (if (outputPortOpen?)
        (begin
        (close-output-port (getOutputPort))
        (set! output 'none))
    (info "Server% closePorts!: There is no output-port open, output-port cannot be closed.")))

  ;------------------------------------------------
  ; Function: writeOutput
  ; Parameters: 
  ;       data: any serializable data.
  ;         Use: The data that needs to be send.
  ; Output: n/a 
  ; Use: Write data to the output port.
  ;------------------------------------------------

  (define/public (writeOutput data)
    (if (connectionActive?)
      (if (serializable? data)
        (let ([serData (serialize data)])
          (write serData output)
          (flush-output output))
      (error "server% writeOutput: The given data is "))
    (error "Server% writeOutput: There is no connection active.")))

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