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
    
;    (define maximalConnections (generic Infrastructure% getMaximalConnections))


    ;----------------------------------------------------
    ; Funtion: initialised?
    ; Parameters: n/a
    ; Output:
    ;   boolean: boolean
    ;     Use: Wheter or not the object is initialised.
    ; Use: Determine if the object is initialised.
    ;----------------------------------------------------
    
    (define (init?)
      (and (not(eq? state 'uninitialised))
           (not(eq? direction 'uninitialised))))
    (augment init?)

    (define/public (initialised?)
      (init?))

    (define (init!)
      (init!))
    (augment init!)

    (define/public (initialise!)
      (setConnections! 'none 'none 'none)
      (setState! 'none)
      (init!))

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

 ;   (define/public (initialise! direction state)
 ;     (if (not (initialised?))
 ;         (if (and (symbol? direction)
 ;                  (or (eq? direction connection)
 ;                      (member direction y-connection)   ; if the direction is none -> there is no specific driving direcion
 ;                      ('none)))
 ;             (setDirection! direction)
 ;             (error "Switch% initialise!: contract violation, expected a symbol, recieved" direction))
 ;         (error "Switch% initialise!: Object is already initialised you cannot reinitialise the object")))

    ;---------------------------------------------------------------------
    ; Function: setState!
    ; Parameters:
    ;         ID: symbol
    ;          Use: One of the y-connection id's
    ; Output: n/a
    ; Use: Initialise the switch to be directed to a certain connection.
    ;---------------------------------------------------------------------
    
    (define/public (setState! ID)
      (if (connected?)
          (if (member ID y-connection)
              (set! state ID)
              (error "Switch% setState!: Given ID does not belong to one of the connections, recieved:" ID))
          (error "Switch% setState!: The switch is not connected please connect before use.")))

    ;-----------------------------------------------------
    ; Function: Switch!
    ; Parameters: n/a
    ; Output: n/a
    ; Use: Switch the switch to the opposite connection.
    ;-----------------------------------------------------
    
    (define/public (switch!)
      (if (completelyConnected?)
          (set! state (getOppositeYConnection state))
          (error "Switch% switch!: Switch is not connected please connect before use.")))

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

    ;-------------------------------------------------------------------------------------
    ; Function: setDirection!
    ; Parameters:
    ;         dirID: symbol
    ;          Use: One of the connections or 'none when there is no specific direction.
    ; Output: n/a
    ; Use: Set the driving direction of the switch.
    ;-------------------------------------------------------------------------------------

    (define/public (setDirection! dirID)
      (if (and (symbol? dirID)
               (or (eq? 'none dirID)
                   (eq? connection dirID)
                   (member dirID y-connection)))
          (set! direction dirID)
          (error "Switch% setDirection!: Given direction is not a symbol, not equal to 'none or not a connection of the switch, recieved:" dirID)))
               
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
      (if (symbol? conn)
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
               (symbol? conn2))
          (set! y-connection (list conn1 conn2))
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
     (and (not (eq? connection 'uninitialised))
           (not (eq? y-connection 'uninitialised))))

    ;------------------------------------------------------------------------------
    ; Function: getConnections
    ; Parameters: n/a
    ; Output:
    ;      connections: list<symbols>
    ;       Use: List containing the ID's of the connections.
    ; Use: Get the ID's of the connected railway object connected to the switch.
    ;------------------------------------------------------------------------------

    (define/public (getConnections)
   ;   (if (connected?)
          (list connection (car y-connection) (cadr y-connection)))
    ;      (error "Switch getConnections: Switch is not connected, please connect before use.")))

    ;--------------------------------------------------------------------------------------
    ; Function: getConnection
    ; Parameters: n/a
    ; Output:
    ;       connection: symbol
    ;        Use: The ID of the object that is connected to the switch.
    ; Use: Get the ID of the railway object connected on the straight part of the swith.
    ;--------------------------------------------------------------------------------------

    (define/public (getConnection)
 ;     (if (connected?)
          connection)
  ;        (error "Switch% getConnection: Switch is not connected, please connect before use.")))

    ;----------------------------------------------------------------------------------------------------
    ; Function: getYConnection
    ; Parameters: n/a
    ; Output:
    ;      yConnection: pair<symbol>
    ;       Use: The ID's of the objects that are connected to the switch.
    ; Use: Get the ID's of the railway objects that are connected on the splitted part of the switch.
    ;----------------------------------------------------------------------------------------------------
    
    (define/public (getYConnection)
  ;    (if (connected?)
          y-connection)
   ;       (error "Switch% getYConnection: Switch is not connected, please connect before use.")))

    ;-----------------------------------------------------------------------------------------
    ; Function: hasOppositeYconnection?
    ; Parameters:
    ;        connID: symbol
    ;          Use: One of the y connections of the switch who's opposite needs to be found.
    ; Output:
    ;       boolean: boolean
    ;        Use: Determine if the given connection has an opposite connection.
    ; Use: Determine if one of the switch's y connection has an opposite connection.
    ;------------------------------------------------------------------------------------------
    
    (define/private (hasOppositeYConnection? connID)
      (and (member connID y-connection)
           (not (member 'none y-connection))))

    ;----------------------------------------------------------------------------------------
    ; Function: getOppositeYConnection
    ; Parameters:
    ;          connID: symbol
    ;            Use: The y connection of which the opposite connection needs to be found.
    ; Output:
    ;       oppositeConnection: symbol
    ;         Use: The opposite connection of the given connection.
    ; Use: Get the opposite connection of the given y connection.
    ;----------------------------------------------------------------------------------------
    
    (define/public (getOppositeYConnection connID)
      (if (connected?)
          (when (hasOppositeYConnection? connID)
            (if (eq? connID (car y-connection))
                (cdr y-connection)
                (car y-connection)))
          (error "Switch% getOppositeYConnection: It has no opposite connection connection:" connID)))
      
    
    ))


