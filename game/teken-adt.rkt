#lang racket
(require "Graphics.rkt")
(require compatibility/mlist)


(#%provide maak-teken-adt)

(define (maak-teken-adt title width height)
  ;--------------------------------------------------;
  ;                                                  ;
  ; Aanmaak van de window en de verschillende layers ;
  ;                                                  ;
  ;--------------------------------------------------;
  
  (define window (make-window width height title))

  ;---------------------------------------------------;
  ;                                                   ;
  ;     Definieren van de verschillende layers        ;
  ;                                                   ;
  ;---------------------------------------------------;
  ;De Tiles die in de immovable-layer zitten zijn de Tiles die niet ter beweging gebracht
  ;werden. Deze Tiles zijn de Tiles die als versiering dienen, de platformen, en de ladders.
  ;De andere layers bevatten de Tiles waarnaar hun naam wijzigt.
  (let ((immovable-layer (window 'make-layer))
        (mario-layer (window 'make-layer))
        (donkey-kong-layer (window 'make-layer))
        (pauline-layer (window 'make-layer))
        (barrel-layer (window 'make-layer))
        (ghost-layer (window 'make-layer))
        (flame-layer (window 'make-layer))
        (lava-layer (window 'make-layer))
        (menu-layer (window 'make-layer)))

    ;---------------------------------------------------;
    ;                                                   ;
    ;              Aanmaak hulpprocedures               ;
    ;                                                   ;
    ;---------------------------------------------------;
    ;De volgende procedures zijn hulpprocedures of procedures die aangemaakt
    ;werden als hogere orde procedures.
    
    ;De procedure add-tile! is de procedure die opgeroepen wordt bij het
    ;maken van Tiles die enkel voor versiering dienen.
    (define (add-tile! x y path . mask)
      (let ((tile (if (null? mask)
                      (make-bitmap-tile path)
                      (make-bitmap-tile path (car mask)))))
        ((immovable-layer 'add-drawable) tile)
        ((tile 'set-x!) (* x width))
        ((tile 'set-y!) (* y height))))

    ;change-mario-sequence verandert de sequence waanaar de variabele mario
    ;aan gelijk staat. De verandering van sequence gebeurt vaak bij de verschillende
    ;bewegingen van Mario.
    (define (change-mario-sequence sequence)
      ((mario-layer 'remove-drawable) mario)
      (set! mario sequence)
      ((mario-layer 'add-drawable) mario))

    ;draw-character! wordt opgeroepen in verschillende teken-procedures.
    (define (draw-character! character-tile character-adt)
      (let ((x-pos (* (character-adt 'get-x) width))
            (y-pos (* (character-adt 'get-y) height)))
        ((character-tile 'set-x!) x-pos)
        ((character-tile 'set-y!) y-pos)))

    ;next-tile-in-seq laat de volgende Tile in de Tile-sequence verschijnen.
    (define (next-tile-in-seq tile-sequence)
      (tile-sequence 'set-next!))

    ;set-position! "set" de x- en y-positie van een bepaalde personage
    (define (set-position! x y character)
      ((character 'set-x!) x)
      ((character 'set-y!) y))

    ;Draw-enemies is verantwoordelijk voor het tekenen van de spoken en de kleine vlammen. Om dit te doen
    ;worden volgende gegevens opgevraagd: x- en y-positie, de richting waar de vijhand aan het kijken is, een boolean
    ;die aangeeft of er van Tile-sequence verandert moet worden, en de vijand zelf. Afhankelijk van deze gegevens,
    ;wordt er beslist of de Tile-sequence verandert moet worden, en indien dat het geval is, welke Tile-sequence er
    ;getoond moet worden.
    (define (draw-enemies dispatch-enemy right-sequence left-sequence tile-list layer)
      (let* ((x-pos (* (dispatch-enemy 'get-x) width))
             (y-pos (* (dispatch-enemy 'get-y) height))
             (face (dispatch-enemy 'get-face))
             (changed? (dispatch-enemy 'changed?))
             (object (massoc dispatch-enemy tile-list)))
        (when changed?
          (let ((new-object-sequence (if (eq? face 'right)
                                         right-sequence
                                         left-sequence))
                (old-object (mcdr object)))
            ((layer 'remove-drawable) old-object)
            (set-mcdr! object new-object-sequence)
            ((layer 'add-drawable) new-object-sequence)))   
        (let ((tile (mcdr object)))
          (set-position! x-pos y-pos tile))))

    ;Normalize-lava verkleint de lava naar haar initiële grootte.
    (define (normalize-lava)
      ((lava-layer 'remove-drawable) lava)
      (set! lava-height lava-height-reference)
      (set! lava (make-tile lava-width lava-height "img/lavaa.png" "img/lavaa_mask.png")))

    ;Clean-up "kuist" de scherm op door alle Tiles die in haar body
    ;gedefinieerd zijn weg te halen.
    (define (clean-up)
      (immovable-layer 'remove-all)
      (when (not (null? ghost-tiles))
        (ghost-layer 'remove-all)
        (set! ghost-tiles '()))
      
      (when (not (null? flame-tiles))
        (flame-layer 'remove-all)
        (set! flame-tiles '()))
      
      (barrel-layer 'remove-all))

    ;Deze procedure is een hulp procedure die het aantal nullen die
    ;achter de high score of score moeten verschijnen genereert.
    (define (generate-zeros number-string)
      (let loop ((length (- 12 (string-length number-string)))
                 (new-string ""))
        (if (= length 0)
            (string-append new-string number-string)
            (loop (- length 1)
                  (string-append new-string "0")))))

    ;Hulpprocedure die elke score-rij die zich in de databank bevindt tekent.
    (define (draw-high-score database-rows)
      ((menu-layer 'remove-drawable) main-menu)
      (set! main-menu high-score-tile)
      ((menu-layer 'add-drawable) main-menu)
      (let* ((rows database-rows)
             (leng (length rows)))
        (let loop ((index 1)
                   (y-pos 110))
          (when (not (>= (- index 1) leng))
            (let* ((first-row (list-ref rows (- index 1)))
                   (player (vector-ref first-row 0))
                   (score (number->string (vector-ref first-row 1)))
                   (rank (string-append (number->string index) ".")))
              ((main-menu 'draw-text) rank 20 (- (/ width 2) 170) y-pos "white")
              ((main-menu 'draw-text) player 20 (- (/ width 2) 50) y-pos "white")
              ((main-menu 'draw-text) score 20 (+ (/ width 2) 130) y-pos "white")
              (loop (+ index 1)(+ y-pos 40)))))))
    
     ;Hulpprocedure die het main-menu terug laat verschijnen.
    (define (restore-main-menu)
      ((menu-layer 'remove-drawable) main-menu)
      (set! main-menu arrow-sequence)
      ((menu-layer 'add-drawable) main-menu)
      ((main-menu 'set-current!) 0)
      (set! high-score-tile (make-bitmap-tile "img/highscore-screen.png")))

    ;Pprocedure die toelaat om op het Tile van het einde van de spel te schrijven. 
     (define (write-name char)
      ((game-over-screen 'draw-text) char 25 x-pos-name 210 "white")
      (set! x-pos-name (+ x-pos-name 30)))
    ;-------------------------------------------------------------------------------------------------------
   
    ;--------------------------------------------------;
    ;                     Mario                        ;
    ;--------------------------------------------------;

    ;Right-sequence is een tile-sequence bestaande uit de variabelen img1 en img2,
    ;deze variabelen bevatten de eigenlijke Tiles. Right-sequence wordt gebruikt
    ;om de animatie van Mario, lopend naar rechts, te maken.
    (define img1 (make-bitmap-tile "img/marioRight.png" "img/marioRight_mask.png"))
    (define img2 (make-bitmap-tile "img/marioRightRun.png" "img/marioRightRun_mask.png"))
    (define right-sequence (make-tile-sequence (list img1 img2)))

    ;Dezelfde als right-sequence, maar in de tegengestelde richting.
    (define img3 (make-bitmap-tile "img/marioLeft.png" "img/marioLeft_mask.png"))
    (define img4 (make-bitmap-tile "img/marioLeftRun.png" "img/marioLeftRun_mask.png"))
    (define left-sequence (make-tile-sequence (list img3 img4)))

    ;Jump-left en jump-right bevatten de Tiles van de springende Mario. Deze worden gebruikt
    ;bij het springen van Mario.
    (define jump-left (make-bitmap-tile "img/marioLeftJump.png" "img/marioLeftJump_mask.png"))
    (define jump-right (make-bitmap-tile "img/marioRightJump.png" "img/marioRightJump_mask.png"))

    ;Climb-left en climb-right bevatten de Tiles van de klimmende Mario. Ze worden gebruikt
    ;bij het maken van climbing-sequence, om Mario te animeren bij het klimmen van de ladders.
    (define climb-left (make-bitmap-tile "img/marioUp1.png" "img/marioUp1_mask.png"))
    (define climb-right (make-bitmap-tile "img/marioUp2.png" "img/marioUp2_mask.png"))
    (define climbing-sequence (make-tile-sequence (list climb-left climb-right)))

    ;De variabelen Mario is de variabelen die aangetast wordt bij het lopen, klimmen, springen, enz...
    ;het is dus de eingelijke Mario. Bij het begin, is deze variabele gelijk aan right-sequence aangezien
    ;Mario, bij het begin van het spel, naar rechts kijkt.
    (define mario right-sequence)
   
    ;De draw-mario! is verantwoordelijk voor het aanpassen van de Mario-Tile bij de verschillende
    ;bewegingen. Om dit te doen worden verschillende gegevens opgevraagd, namelijk de x- en y-positie,
    ;een boolean die aangeeft of er van sequence verandert moet worden, een boolean die aangeeft
    ;of Mario in de lucht zit, een boolean die aangeeft of Mario op een ladder zit ;en de kant waarnaar
    ;Mario aan het kijken is. Afhankelijk van deze gegevens, verschijnt de juiste sequence op het scherm.
    (define (draw-mario! mario-adt)
      (let ((x-pos (* (mario-adt 'get-x) width))
            (y-pos (* (mario-adt 'get-y) height))
            (changed? (mario-adt 'changed?))
            (up? (mario-adt 'up?))
            (on-ladder? (mario-adt 'on-ladder?))
            (face (mario-adt 'get-face)))
        (cond
          (changed?
           (if (eq? face 'right)
               (change-mario-sequence right-sequence)
               (change-mario-sequence left-sequence)))
          (up?
           (if (eq? face 'right)
               (change-mario-sequence jump-right)
               (change-mario-sequence jump-left)))
          (on-ladder?
           (change-mario-sequence climbing-sequence)))
      
        ((mario 'set-x!) x-pos)
        ((mario 'set-y!) y-pos)))

    ;mario-next-tile! zorgt voor het verschijnen van de volgende Tile in de Tile-sequence. Deze procedure
    ;krijgt optioneel een index. Indien er geen index meegegeven wordt, zal de volgende tile, in de tile-sequence,
    ;op het scherm veschijnen.
    (define (mario-next-tile! . idx) 
      (if (not (null? idx))
          ((mario 'set-current!) (car idx))
          (next-tile-in-seq mario)))
    ;-------------------------------------------------------------------------------------------------------

    ;--------------------------------------------------;
    ;                   Donkey Kong                    ;
    ;--------------------------------------------------;

    ;De variabelen donkey-kong-left-arm-up/right-arm-up bevatten de Tiles waarbij Donkey Kong, ofwel
    ;de linkerarm, ofwel de rechterarm naar boven heeft. Deze twee variabelen vormen de Tile-sequence
    ;donkey-kong.
    (define donkey-kong-left-arm-up (make-bitmap-tile "img/Kong1.png" "img/Kong1_mask.png"))
    (define donkey-kong-right-arm-up (make-bitmap-tile "img/Kong3.png" "img/Kong3_mask.png"))
    (define donkey-kong (make-tile-sequence (list donkey-kong-left-arm-up donkey-kong-right-arm-up)))
  
    ;Teken-procedure voor Donkey Kong
    (define (draw-donkey-kong! donkey-kong-adt)
      (draw-character! donkey-kong donkey-kong-adt))

    ;Deze procedure gaat, simpelweg, de volgende Tile laten verschijnen.
    (define (donkey-kong-next-tile!)
      (next-tile-in-seq donkey-kong))
    
    ;-------------------------------------------------------------------------------------------------------

    ;--------------------------------------------------;
    ;                     Pauline                      ;
    ;--------------------------------------------------;

    ;De Tile-sequence pauline wordt gevormd door pauline-no-sign en pauline-sign. Pauline-no-sign
    ;is de Tile waar Pauline niet "roept", en pauline-sign is degene waarbij ze wel "roept".
    (define pauline-no-sign (make-bitmap-tile "img/pauline1.png" "img/pauline1_mask.png"))
    (define pauline-sign (make-bitmap-tile "img/pauline2.png" "img/pauline2_mask.png"))
    (define pauline (make-tile-sequence (list pauline-no-sign pauline-sign))) 

    ;teken-procedure van Pauline.
    (define (draw-pauline! pauline-adt)
      (draw-character! pauline pauline-adt))
    
    ;Zelfde doel als donkey-kong-next-tile! maar met Pauline.
    (define (pauline-next-tile!)
      (next-tile-in-seq pauline))
    ;-------------------------------------------------------------------------------------------------------

    ;--------------------------------------------------;
    ;               Platformen en ladder               ;
    ;--------------------------------------------------;

    ;draw-platform! teken de platformen. Om dit te doen, neemt deze procedure verschillende gegevens, namelijk
    ;de x-positie, de y-positie, en een getal genoemd type. Type geeft aan welke type platform er gebruikt
    ;zal worden voor een bepaalde platform-object, en zal dus ook de passende Tile tekenen.
    (define (draw-platform! platform-adt)
      (let* ((x-pos (* (platform-adt 'get-x) width))
             (y-pos (* (platform-adt 'get-y) height))
             (type (platform-adt 'get-type))
             (platform (cond
                         ((= type 1) (make-bitmap-tile "img/floor1.png"))
                         ((= type 2) (make-bitmap-tile "img/floor2.png"))
                         ((= type 3) (make-bitmap-tile "img/floor3.png")))))
        ((immovable-layer 'add-drawable) platform)
        (set-position! x-pos y-pos platform)))

    
    (define (draw-ladder! ladder-adt)
      (let ((x-pos (* (ladder-adt 'get-x) width))
            (y-pos (* (ladder-adt 'get-y) height))
            (ladder (make-bitmap-tile "img/ladder.png" "img/ladder_mask.png")))
        ((immovable-layer 'add-drawable) ladder)
        (set-position! x-pos y-pos ladder)))
    ;-------------------------------------------------------------------------------------------------------

    ;--------------------------------------------------;
    ;                     Barrels                      ;
    ;--------------------------------------------------;
    (define barrel-tiles '());lijst om de barrels bij te houden.

    ;add-barrel is verantwoordelijk voor het genereren van de barrels
    (define (add-barrel dispatch-barrel)
      (let ((barrel-tile (make-bitmap-tile "img/ton.png" "img/ton_mask.png")))
        (set! barrel-tiles (cons (cons dispatch-barrel barrel-tile) barrel-tiles))
        ((barrel-layer 'add-drawable) barrel-tile)))
    
    (define (draw-barrel! barrel-adt)
      (let ((x-pos (* (barrel-adt 'get-x) width))
            (y-pos (* (barrel-adt 'get-y) height))
            (barrel (cdr (assoc barrel-adt barrel-tiles))))
        (set-position! x-pos y-pos barrel)))
    
    ;Deze procedure wordt opgeroepen bij het verwijderen van een barrel.
    ;Een barrel wordt verwijdert wanneer die niet meer op de scherm zit.
    (define (delete-barrel dispatch-barrel)
      (let ((barrel (cdr (assoc dispatch-barrel barrel-tiles))))
        ((barrel-layer 'remove-drawable) barrel)
        (set! barrel-tiles (drop-right barrel-tiles 1))))

    ;-------------------------------------------------------------------------------------------------------
    ;--------------------------------------------------;
    ;                      Ghosts                      ;
    ;--------------------------------------------------;
    (define ghost-tiles '());lijst om de spoken vij te houden.

    ;Ik heb beslist om de spoken te laten genereren in plaats van een vast aantal spoken te
    ;hebben, omdat er misschien voor een niveau meerdere spoken kunnen gewenst worden
    ;en dus om dit gemakkelijk toe te laten heb ik beslist om ze te genereren.
    (define (add-ghost dispatch-ghost)
      (let ((ghost-sequence (make-tile-sequence (list (make-bitmap-tile "img/ghostRight1.png" "img/ghostRight1_mask.png")(make-bitmap-tile "img/ghostRight2.png" "img/ghostRight2_mask.png")(make-bitmap-tile "img/ghostRight3.png" "img/ghostRight3_mask.png")(make-bitmap-tile "img/ghostRight4.png" "img/ghostRight4_mask.png")))))
        (set! ghost-tiles (mcons (mcons dispatch-ghost ghost-sequence) ghost-tiles))
        ((ghost-layer 'add-drawable) ghost-sequence)))

    (define (draw-ghost dispatch-ghost)
      (draw-enemies dispatch-ghost (make-tile-sequence (list (make-bitmap-tile "img/ghostRight1.png" "img/ghostRight1_mask.png")(make-bitmap-tile "img/ghostRight2.png" "img/ghostRight2_mask.png")(make-bitmap-tile "img/ghostRight3.png" "img/ghostRight3_mask.png")(make-bitmap-tile "img/ghostRight4.png" "img/ghostRight4_mask.png"))) (make-tile-sequence (list (make-bitmap-tile "img/ghostLeft1.png" "img/ghostLeft1_mask.png")(make-bitmap-tile "img/ghostLeft2.png" "img/ghostLeft2_mask.png")(make-bitmap-tile "img/ghostLeft3.png" "img/ghostLeft3_mask.png")(make-bitmap-tile "img/ghostLeft4.png" "img/ghostLeft4_mask.png"))) ghost-tiles ghost-layer)) 

    (define (ghost-next-tile! dispatch-ghost)
      (let ((ghost-sequence (mcdr (massoc dispatch-ghost ghost-tiles))))
        (next-tile-in-seq ghost-sequence)))
                     
    ;-------------------------------------------------------------------------------------------------------
    ;--------------------------------------------------;
    ;                      Flames                      ;
    ;--------------------------------------------------;
    (define flame-tiles '())

    (define (add-flame dispatch-flame)
      (let ((flame-sequence (make-tile-sequence (list (make-bitmap-tile "img/fireRight1.png" "img/fireRight1_mask.png")(make-bitmap-tile "img/fireRight2.png" "img/fireRight2_mask.png")))))
        ((flame-layer 'add-drawable) flame-sequence)
        (set! flame-tiles (mcons (mcons dispatch-flame flame-sequence) flame-tiles))))
   
    (define (draw-flame dispatch-flame)
      (draw-enemies dispatch-flame (make-tile-sequence (list (make-bitmap-tile "img/fireRight1.png" "img/fireRight1_mask.png")(make-bitmap-tile "img/fireRight2.png" "img/fireRight2_mask.png"))) (make-tile-sequence (list (make-bitmap-tile "img/fireLeft1.png" "img/fireLeft1_mask.png")(make-bitmap-tile "img/fireLeft2.png" "img/fireLeft2_mask.png"))) flame-tiles flame-layer))
    
    (define (flame-next-tile! dispatch-flame)
      (let ((flame-sequence (mcdr (massoc dispatch-flame flame-tiles))))
        (next-tile-in-seq flame-sequence)))
                     
    ;-------------------------------------------------------------------------------------------------------
   
    ;--------------------------------------------------;
    ;                       Lava                       ;
    ;--------------------------------------------------;
    (define lava-width 590)
    (define lava-height 8)
    ;Aangezien de lava altijd groter wordt, wordt de lengte van de lava bijgehouden zodat wanneer
    ;de lengte (lava-height) aangepast werd en dat er gevraagd wordt om de lava terug naar haar
    ;initiele staat te brengen, er gewoon naar lava-height-reference bekeken moet worden 
    (define lava-height-reference lava-height)
    (define lava (make-tile lava-width lava-height "img/lavaa.png" "img/lavaa_mask.png"))

    ;Procedure verantwoordelijk voor het groeien van de lava
    ;Het groeien van de lava komt neer tot het aanpassen van de lengte van de tile.
    (define (grow-lava growing-factor)
      ((lava-layer 'remove-drawable) lava)
      (set! lava-height (+ lava-height (* growing-factor height)))
      (set! lava (make-tile lava-width lava-height "img/lavaa.png" "img/lavaa_mask.png"))
      ((lava-layer 'add-drawable) lava))
    
    (define (remove-lava)
      (normalize-lava))

    (define (resize-lava)
      (normalize-lava)
      ((lava-layer 'add-drawable) lava))

    (define (draw-lava dispatch-lava)
      (let ((drawn? (dispatch-lava 'drawn?)))
        (when (not drawn?)
          ((lava-layer 'add-drawable) lava))
        (draw-character! lava dispatch-lava)))

              
    ;-------------------------------------------------------------------------------------------------------
    ;--------------------------------------------------;
    ;                       Menu                       ;
    ;--------------------------------------------------;
    ;De spelmenu wordt onderverdeelt in twee submenu's: de main-menu en de menu bij het sterven van Mario.
    ;Elke submenu moet zich op de window kunnen zetten, en zich eraf kunnen zetten.
    
    ;De main-menu wordt al bij het begin getekend aangezien dit het eerste is dat de speler gezien moet
    ;krijgen bij het starten van het spel.
    ;De procedure main-menu-tile tekent de tile van de main-menu uit de window, en zet de Tile van de
    ;levens op de window.
    (define arrow-start-game (make-bitmap-tile "img/intro.png"))
    (define arrow-high-score (make-bitmap-tile "img/intro2.png"))
    (define arrow-exit-game (make-bitmap-tile "img/intro3.png"))
    (define arrow-sequence (make-tile-sequence (list arrow-start-game arrow-high-score arrow-exit-game)))
    (define high-score-tile (make-bitmap-tile "img/highscore-screen.png"))
    (define victory (make-bitmap-tile "img/victory.png"))
    (define defeat (make-bitmap-tile "img/game-over.png"))
    (define lives (make-tile-sequence (list (make-bitmap-tile "img/lives1.png")(make-bitmap-tile "img/lives2.png")(make-bitmap-tile "img/lives3.png"))))
    (define highscore (make-bitmap-tile "img/highscore.jpg"))
    (define time-tile (make-bitmap-tile "img/time.png"))
    (define score (make-bitmap-tile "img/score.png"))
    (define game-over-screen '())
    (define x-pos-name 175)

    ;De main-menu wordt geinitialiseerd naar arrow-sequence
    (define main-menu arrow-sequence)
    ((menu-layer 'add-drawable) main-menu)
    ((main-menu 'set-x!) 13)

    ;De procedure die zich beig houdt met de main-menu krijgt twee parameters,
    ;namelijk een "bericht" die zegt wat er gedaan moet worden, en elementen
    ;uit de databank om te tekenen.
    (define (main-menu-tile msg . database)
      (cond
        ((eq? msg 'next) (main-menu 'set-next!))
        ((eq? msg 'previous) (main-menu 'set-previous!))
        ((eq? msg 'highscore) (draw-high-score (car database))) ;Hier worden de de hoogste scores opgetekend.
        ((eq? msg 'back-to-menu) (restore-main-menu))
        ;Bij het weghalen van de main-menu worden volgende componenten getekend: de levens, de score, en de high score.
        ((eq? msg 'remove)
         (let* ((element (vector-ref (caar database) 0))
                (highest-score
                 (if (not (number? element))
                     "000000000000"
                     (generate-zeros (number->string element)))))
           ((menu-layer 'remove-drawable) main-menu)
           ((menu-layer 'add-drawable) lives)
           ((menu-layer 'add-drawable) score)
           ((menu-layer 'add-drawable) time-tile)
           ((score 'draw-text) "000000000000" 16 20 22 "white")
           ((highscore 'draw-text) highest-score 16 0 20 "white") 
           ((menu-layer 'add-drawable) highscore)
           ((highscore 'set-x!) (- (/ width 2) 68))
           ((highscore 'set-y!) 5)))
        (else
         (error "Wrong messange sent [teken-adt] - main-menu-tile: " msg))))

    ;Game-over-tile houdt zich bezig met de Tile die getoond wordt bij het
    ;einde van het spel (win en verlies). Deze procedure neemt twee parameters,
    ;namelijk wat er met de Tile moet gebeuren (verschijnen of verdwijnen) en
    ;welke Tile aangetoond moet worden.
    (define (game-over-tile msg . won-or-lost)
      (cond
        ((and (eq? msg 'over)(not (null? won-or-lost)))
         ;Bij het eindigen van het spel wordt er bekeken of de speler
         ;gewonnen heeft of niet ( (eq? (car won-or-lost) 'victory) ),
         ;wordt aan de de hand van deze informatie de juiste Tile gezet.
         (if (eq? (car won-or-lost) 'victory)
             (set! game-over-screen victory)
             (set! game-over-screen defeat))
         ;Ook wordt bij het beindigen van het spel de scherm "opgekuist". De Tiles
         ;van de levens, high score en tijd worden weggehaald en terug "proper" gemaakt,
         ;en wordt de proceudre clean-up opgeroepen.
         ((menu-layer 'add-drawable) game-over-screen)
         ((menu-layer 'remove-drawable) lives)
         ((lives 'set-current!) 0)
         ((menu-layer 'remove-drawable) highscore)
         ((menu-layer 'remove-drawable) score)
         ((menu-layer 'remove-drawable) time-tile)
         (set! highscore (make-bitmap-tile "img/highscore.jpg"))
         (set! score (make-bitmap-tile "img/score.png"))
         (set! time-tile (make-bitmap-tile "img/time.png"))
         (clean-up))
        ;Indien het spel terug opgestart wordt, dan wordt de Tile weggehaald.
        ((eq? msg 'restart)
         ((menu-layer 'remove-drawable) game-over-screen)
         (set! x-pos-name 175)
         (set! victory (make-bitmap-tile "img/victory.png"))
         (set! defeat (make-bitmap-tile "img/game-over.png"))
         (set! game-over-screen '())
         ((menu-layer 'add-drawable) main-menu))
        (else
         (error "Wrong message sent [teken-adt] - game-over: " msg))))

    ;Positioneren van de verschillende Tiles.
    ((lives 'set-x!) (- width 140))
    ((lives 'set-y!) (- height 675))
       
    ((time-tile 'set-x!) (- width 115))
    ((time-tile 'set-y!) (- height 620))

    ;Procedure om de volgende Tile in de tile-sequence lives te laten verschijnen.
    (define (loose-life!)
      (lives 'set-next!)
      (barrel-layer 'remove-all)
      (set! barrel-tiles '()))
    
    ;-------------------------------------------------------------------------------------------------------
    ;--------------------------------------------------;
    ;                      Score                       ;
    ;--------------------------------------------------;
        
    (define (update-score player-score)
      (let ((the-score (generate-zeros (number->string player-score))))
        ((menu-layer 'remove-drawable) score)
        (set! score (make-bitmap-tile "img/score.png"))
        ((score 'draw-text) the-score 16 20 22 "white")
        ((menu-layer 'add-drawable) score)))
    ;-------------------------------------------------------------------------------------------------------
    ;--------------------------------------------------;
    ;                      Clock                       ;
    ;--------------------------------------------------;

    ;Draw-time tekent het tijd
    (define (draw-time timer-adt)
      (let* ((time (timer-adt 'get-time))
             (min (car time))
             (sec (cdr time))
             (time-text (string-append min ":" sec)))
        ((menu-layer 'remove-drawable) time-tile)
        (set! time-tile (make-bitmap-tile "img/time.png"))
        ((time-tile 'draw-text) time-text 15 8 25 "white")
        ((time-tile 'set-x!) (- width 115))
        ((time-tile 'set-y!) (- height 620))
        ((menu-layer 'add-drawable) time-tile)))
      
    ;-------------------------------------------------------------------------------------------------------

    ;---------------------------------------------;
    ;                                             ;
    ; Toevoegen van de Tiles in de juiste layers  ;
    ;                                             ;
    ;---------------------------------------------;
  
    ((window 'set-background!) "black")
    ((mario-layer 'add-drawable) mario)
    ((donkey-kong-layer 'add-drawable) donkey-kong)
    ((pauline-layer 'add-drawable) pauline)
    ;-------------------------------------------------------------------------------------------------------
    
    ;---------------------------------------------------------;
    ;                                                         ;
    ; Setten van de begin posities van de verschillende tiles ;
    ;                                                         ;
    ;---------------------------------------------------------;
    ;Alle Tiles zitten vanaf het begin van het spel al op de window getekend, maar ze moeten niet
    ;verschijnen als het spel niet begonnen is. Om dit te doen hebben alle Tiles een x-positie van -200.
    
    ((mario 'set-x!) -200)
    ((donkey-kong 'set-x!) -200)
    ((pauline 'set-x!) -200)
    ;-------------------------------------------------------------------------------------------------------
    
    ;Toetsenbord functie
    (define (onkeydown function)
      ((window 'set-key-callback!) function))

    ;set-update-callback!
    (define (update-loop function)
      ((window 'set-update-callback!) function))

    (define (dispatch-teken-adt m)
      (cond
        ((eq? m 'draw-mario!) draw-mario!)
        ((eq? m 'mario-next-tile!) mario-next-tile!)
        ((eq? m 'draw-donkey-kong!) draw-donkey-kong!)
        ((eq? m 'donkey-kong-next-tile!)(donkey-kong-next-tile!))
        ((eq? m 'draw-pauline!) draw-pauline!)
        ((eq? m 'pauline-next-tile!)(pauline-next-tile!))
        ((eq? m 'draw-platform!) draw-platform!)
        ((eq? m 'draw-ladder!) draw-ladder!)
        ((eq? m 'add-tile!) add-tile!)
        ((eq? m 'loose-life!)(loose-life!))
        ((eq? m 'delete-barrel) delete-barrel)
        ((eq? m 'add-barrel) add-barrel)
        ((eq? m 'draw-barrel!) draw-barrel!)
        ((eq? m 'main-menu-tile) main-menu-tile)
        ((eq? m 'game-over-tile) game-over-tile)
        ((eq? m 'write-name) write-name)
        ((eq? m 'update-score) update-score)
        ((eq? m 'draw-time) draw-time)
        ((eq? m 'ghost-next-tile!) ghost-next-tile!)
        ((eq? m 'draw-ghost) draw-ghost)
        ((eq? m 'add-ghost) add-ghost)
        ((eq? m 'flame-next-tile!) flame-next-tile!)
        ((eq? m 'draw-flame) draw-flame)
        ((eq? m 'add-flame) add-flame)
        ((eq? m 'draw-lava) draw-lava)
        ((eq? m 'resize-lava) (resize-lava))
        ((eq? m 'grow-lava) grow-lava)
        ((eq? m 'remove-lava) (remove-lava))
        ((eq? m 'onkeydown) onkeydown)
        ((eq? m 'update-loop) update-loop)
        ((eq? m 'clean-up)(clean-up))
        (else
         (error "Wrong message sent! - dispatch-teken-adt" m))))

    dispatch-teken-adt))