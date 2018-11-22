#lang racket

(require "track.rkt")
(require "switch.rkt")
(require "detectionBlock.rkt")

(define RailwayManager%
  (class object%
    (super-new)

    ; The hashtables where the objects are saved.
    ; The keys are the id's and values, the objects.
    (define trackTable (make-hash))
    (define switchTable (make-hash))
    (define detectionblockTable (make-hash))

    (define trackType 'object:Track%)
    (define switchType 'object:Switch%)
    (define detectionblockType 'object:Detectionblock%)

    ;----------------------------------------------------------------------
    ; Function: isUnique?
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification that needs to be checked.
    ; Output:
    ;    boolean: boolean
    ;     Use: Boolean to determine whether or not a symbol is unique.
    ; Use: Determine whether or not a given symbol is unique.
    ;----------------------------------------------------------------------

    (define (isUnique? id)
      (if (symbol? id)
          (and (not (hash-has-key? trackTable id))
               (not (hash-has-key? switchTable id))
               (not (hash-has-key? detectionblockTable id)))
          (error "RailwayManager% isUnique?: Contract violation symbol expected received" id)))

    ;------------------------------------------------------------------
    ; Function: isTrack?
    ; Parameters:
    ;     id: symbol
    ;      Use: The id of the object that needs to be checked.
    ; Output:
    ;    boolean: boolean
    ;      Use: A boolean that determines if the object is a track.
    ; Use: Determine if an id belongs to a track object.
    ;------------------------------------------------------------------
    
    (define/public (isTrack? id)
      (if (symbol? id)
          (hash-has-key? trackTable id)
          (error "RailwayManager% isTrack?: contract violation, expected symbol received" id)))

    ;-------------------------------------------------------------------------------
    ; Function: createTrack!
    ; Parameters:
    ;      id: symbol
    ;       Use: The id to identify the object uniquely.
    ; Ouput: n/a
    ; Use: Create a Switch% object with the given ID and store it in the hashtable.
    ;-------------------------------------------------------------------------------

    (define/public (createTrack! id)
      (if (isUnique? id)
          (let ([track (make-object Track%)])
            (send track setID! id)
            (hash-set! trackTable id track))
          (error "RailwayManager% createRail: id is not unique, received" id)))

    ;--------------------------------------------------------------------------
    ; Function: getTrack
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the object that needs to be retrieved.
    ; Ouput:
    ;    track: object: Track%
    ;      Use: The track object that needed to be retrieved.
    ; Use: Retrieve a track object with the corresponding id.
    ;--------------------------------------------------------------------------
    
    (define/public (getTrack id)
      (if (isTrack? id)
          (hash-ref trackTable id)
          (error "RailwayManager% getTrack: given id is not a track member received" id)))

    ;-----------------------------------------------------------------
    ; Function: deleteTrack!
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the to be deleted track object.
    ; Output: n/a
    ; Use: Delete a track object from the hashtable.
    ;-----------------------------------------------------------------

    (define/public (deleteTrack! id)
      (if (isTrack? id)
          (hash-remove! trackTable id)
          (error "RailwayManager% deleteTrack!: given id is not a track member received" id)))

    ;----------------------------------------------------------------------------------------
    ; Function: isSwitch?
    ; Parameters:
    ;     id: symbol
    ;      Use: The identification of the object that needs to be checked.
    ; Output:
    ;    boolean: boolean
    ;     Use: A boolean that determines whether or not an object is in the switch table.
    ; Use: Check whether or not an objet is in the switch table.
    ;----------------------------------------------------------------------------------------

    (define/public (isSwitch? id)
      (if (symbol? id)
          (hash-has-key? switchTable id)
          (error "RailwayManager% isSwitch?: contract violation expected symbol recieved"id)))

    ;--------------------------------------------------------------------------------
    ; Function: createSwitch!
    ; Parameters:
    ;     id: symbol
    ;      Use: The id to uniquely identify the switch object.
    ; Output: n/a
    ; Use: Create a Switch% object with the given ID and store it in the hashtable.
    ;--------------------------------------------------------------------------------
    
    (define/public (createSwitch! id)
      (if (isUnique? id)
          (let ([switch (make-object Switch%)])
            (send switch setID! id)
            (hash-set! switchTable id switch))
          (error "RailwayManager% createSwitch: id is not unique, received" id)))

    ;----------------------------------------------------------------------------
    ; Function: getSwitch
    ; Parameters:
    ;      id: symbol
    ;       Use: The identification of the object that needs to be retrieved.
    ; Output:
    ;    switch: object:Switch%
    ;      Use: The switch object that needs to be retrieved.
    ; Use: Retrieve a Switch% object from the hashtable using it's id.
    ;----------------------------------------------------------------------------
    (define/public (getSwitch id)
      (if (isSwitch? id)
          (hash-ref switchTable id)
          (error "RailwayManager% getSwitch: id is not a member of the switchTable, recieved"id)))

    ;-------------
    ; TODO
    ;-------------

    (define/public (deleteSwitch! id)
      (if (isSwitch? id)
          (hash-remove! switchTable id)
          (error "RailwayManager% deleteSwitch!: id is not a member of the switchtable, recieved" id)))

    ;--------------------------------------------------------------------------------
    ; Function: createDetectionblock!
    ; Parameters:
    ;       id: symbol
    ;        Use: The identification of the new created detectionblock object.
    ; Output: n/a
    ; Use: Create a Detectionblock% object and store it in the detectionblockTable.
    ;--------------------------------------------------------------------------------

    (define/public (createDetectionblock! id)
      (if (isUnique? id)
          (let ([block (make-object Detectionblock%)])
            (send block setID! id)
            (hash-set! detectionblockTable id block))
          (error "RailwayManager% createDetectionBlock: id is not unique, received" id)))

    ;----------------------------------------------------------------------
    ; Function: isDetectionblock?
    ; Parameters:
    ;    id: symbol
    ;     Use: The identification of the object that needs checking.
    ; Output:
    ;     boolean: boolean
    ;       Use: Determine whether or not the object is a detectionblock
    ;----------------------------------------------------------------------

    (define/public (isDetectionblock? id)
      (if (symbol? id)
          (hash-has-key? detectionblockTable id)
          (error "RailwayManager% isDetectionblock?: contract violation, expected symbol received" id)))

    (define/public (getDetectionblock id)
      (if (isDetectionblock? id)
          (hash-ref detectionblockTable id)
          (error "RailwayManager% getDetectionblock: given id is not from a detectionblock recieved" id)))

    (define/public (deleteDetectionblock! id)
      (if (isDetectionblock? id)
          (hash-remove! detectionblockTable id)
          (error "RailwayManager% deleteDetectionbolck!: id is not a member of the detectionblockTable recieved" id)))

     (define (findObject id)
      (cond ((isTrack? id)(getTrack id))
            ((isSwitch? id)(getSwitch id))
            ((isDetectionblock? id)(getDetectionblock id))
            (else
             (error "RailwayManager% findObject: id is not a member of one of the tables recieved" id))))
      
    ))
