#lang racket

(require racket/class)
(require "infrastructure.rkt")

(provide Switch%)

;------------------------------------------------
; Class: Switch%
; Parameters: n/a 
; Use: Create an object that represents a switch.
;------------------------------------------------

(define Switch%
  (class Infrastructure%
    (super-new)

    (field 
     [state        'uninitialised]
     [direction    'uninitialised]
     [connection   'uninitialised]
     [y-connection 'uninitialised])

    ;-------------------------------------------------------
    ; Variable: connections : list
    ;      Use: All the connected objects.
    ; Use: Get the connections field from the super class.
    ;-------------------------------------------------------

    ;  (define connections (generic Infrastructure% getConnections))

    ;---------------------------------------------------------------------
    ; Varialbe:
    ;       maximalConnections: number
    ; Use: The number of the maximal connections the object can have.
    ;---------------------------------------------------------------------
    
    (define maximalConnections (generic Infrastructure% getMaximalConnections))


    ;----------------------------------------------------
    ; Funtion: initialised?
    ; Parameters: n/a
    ; Output:
    ;   boolean: boolean
    ;     Use: Wheter or not the object is initialised.
    ; Use: Determine if the object is initialised.
    ;----------------------------------------------------
    
    (define (initialised?)
      (and (not(eq? state 'uninitialised))
           (not(eq? direction 'uninitialised))))
    (augment initialised?)

    ;-------------------------------------------
    ; Function: initState!
    ; Parameters:
    ;     newstate: symbol
    ;       Use: The state of the switch.
    ; Output: n/a
    ; Use: Initialise the state of the switch.
    ;-------------------------------------------

    ;  (define/public (initState! newstate)
    ;    (if (and(or (eq? newstate 'left)
    ;                (eq? newstate 'right))
    ;            (not(initialised?)))       ; only allowed to initialise the fields, when they are uninitialised.
    ;        (set! state newstate)
    ;        (error "Switch% initState!: State not initialised or given direction is not correct" newstate)))

    (define/public (initialise! direction)
      (if (not (initialised?))
          (if (symbol? direction)
              (setDirection! direction)
              (error "Switch% initialise!: contract violation, expected a symbol, recieved" direction))
          (error "Switch% initialise!: Object is already initialised you cannot reinitialise the object")))

    ;TODO aanpassen naar ID's van de connecties!!!!
    ;----------------------------------------
    ; Funtion: setState!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Change the state of the switch.
    ;---------------------------------------

    (define/public (setState!)
      (cond ((initialised?)(error "Switch% initState!: State not initialised" state))
            ((eq? state 'left)(set! direction 'right))
            (else(set! state 'right))))

    ;--------------------------------------
    ; Function: getState
    ; Parameters: n/a
    ; Output:
    ;    state: symbol
    ;      Use: The state of the switch.
    ; Use: Get the state of the switch.
    ;--------------------------------------
    
    (define/public (getState)
      (if (initialised?)
          state
          (error "Switch% initState!: State not initialised or given direction is not correct")))

    ;--------------------------------------------------------
    ; Function: setDirection!
    ; Parameters:
    ;    dir: number
    ;     Use: The driving direction of the switch.
    ; Output: n/a
    ; Use: Setting the dirving direction of the switch.
    ;--------------------------------------------------------
    
    (define/public (setDirection! dir)
      (if (and (symbol? dir)
               (or (eq? 'left dir)
                   (eq? 'right dir)
                   (eq? 'none dir)))    ;For the direction, numbers 1 and 2 are used.
          (set! direction dir)
          (error "Switch% setDirection!: contract violation, symbol 'left or 'right expected received" dir)))

    ;----------------------------------------------------------
    ; Function: getDirection
    ; Parameters: n/a
    ; Output:
    ;   direction: number
    ;      Use: The driving direction of the switch.
    ; Use: Retrieving the diriving direction of the switch.
    ;----------------------------------------------------------
    
    (define/public (getDirection)
      (if (initialised?)
          direction
          (error "Switch% getDirection: Object is not initialised, please initialise before use.")))

    ;-----------------------------------------------------------------------------------
    ; Function: setConnections!
    ; Parameters:
    ;         conn1: symbol
    ;           Use: ID of the rail or switch that is connected on the straight side.
    ;         conn2: symbol
    ;           Use: ID of the rail or switch that is connected on the splitted side.
    ;         conn3 symbol
    ;           Use: ID of the rail or switch that is connected on the splitted side.
    ; Output: n/a
    ; Use: Set all the connections of the switch.
    ;----------------------------------------------------------------------------------

    (define/public (setConnections! conn1 conn2 conn3)
      (if (and (symbol? conn1)
               (symbol? conn2)
               (symbol? conn3))
          (begin
            (setConnection! conn1)
            (setYConnection! conn2 conn3))
          (error "Switch% setConnections!: Contract violation expected three symbols, recieved:" conn1 conn2 conn3)))

    ;------------------------------------------------------------------------------------------------
    ; Function: setConnection!
    ; Parameters:
    ;          conn: symbol
    ;            Use: ID of the rail or switch that is connected on the straight side of the switch.
    ; Output: n/a
    ; Use: Set the connection of the switch on the straight side.
    ;------------------------------------------------------------------------------------------------
    
    (define/public (setConnection! conn)
      (if (and(symbol? conn)
              (not (eq? conn 'uninitialised)))
          (set! connection conn)
          (error "Switch% setConnection!: Contract violation expected a symbol, recieved:" conn)))

    ;--------------------------------------------------------------------------------------------------------
    ; Function: setYConnection!
    ; Parameters:
    ;         conn1: symbol
    ;          Use: ID of the rail or switch that is connected on one of the splitted parts of the switch.
    ;         conn2: symbol
    ;          Use: ID of the rail or switch that is connected on one of the splitted parts of the switch.
    ; Output: n/a
    ; Use: Set the connections of the switch on the splitted side.
    ;--------------------------------------------------------------------------------------------------------
    
    (define/public (setYConnection! conn1 conn2)
      (if (and (symbol? conn1)
               (symbol? conn2)
               (not (eq? conn1 'uninitialised))
               (not (eq? conn2 'uninitialised)))
          (set! y-connection (cons conn1 conn2))
          (error "Switch% setYConnection!: Contract violation expected two symbols, recieved:" conn1 conn2)))

    ;------------------------------------------------------------------------------------
    ; Function: completelyConnected?
    ; Parameters: n/a
    ; Output:
    ;      boolean: boolean
    ;        Use: Determine if the connection on the straight and splitted side is set.
    ; Use: Determine if the connection on the straight and splitted side is set.
    ;------------------------------------------------------------------------------------
    
    (define/public (completelyConnected?)
      (and (y-connected?)
           (connected?)))

    ;-------------------------------------------------------------------------
    ; Function: y-connected?
    ; Parameters: n/a
    ; Output:
    ;      boolean: boolean
    ;       Use: Determine if the connection on the splitted side is set.
    ; Use: Determine if the connection on the splitted side is set.
    ;-------------------------------------------------------------------------

    (define/public (y-connected?)
      (not (eq? y-connection 'uninitialised)))

    ;------------------------------------------------------------------------
    ; Function: connected?
    ; Parameters: n/a
    ; Output:
    ;      boolean: boolean
    ;       Use: Determine if the connection on the straight side is set.
    ; Use: Determine if the connection on the straight side is set.
    ;------------------------------------------------------------------------
    
    (define/public (connected?)
      (not (eq? connection 'uninitialised)))

    ;------------------------------------------------------------------------------
    ; Function: getConnections
    ; Parameters: n/a
    ; Output:
    ;      connections: list<symbols>
    ;       Use: List containing the ID's of the connections.
    ; Use: Get the ID's of the connected railway object connected to the switch.
    ;------------------------------------------------------------------------------

    (define/public (getConnections)
      (list connection (car y-connection) (cdr y-connection)))
    
    ))


