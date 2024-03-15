#lang racket
(require "teken-adt.rkt")
(require "constants.rkt")
(require compatibility/mlist)
(require db)

;--------------------------------------------------;
;                    Ladder ADT                    ;
;--------------------------------------------------;

(define (make-ladder position)
  (define (get-x)
    (car (position 'get-position)))

  (define (get-y)
    (cdr (position 'get-position)))
  
  (define (set-x! new-x)
    ((position 'set-x!) new-x))

  (define (set-y! new-y)
    ((position 'set-y!) new-y))

  (define (draw! teken-adt)
    ((teken-adt 'draw-ladder!) dispatch-general-adt))
    
  (define (dispatch-general-adt m)
    (cond
      ((eq? m 'get-x) (get-x))
      ((eq? m 'get-y) (get-y))
      ((eq? m 'set-y!) set-y!)
      ((eq? m 'set-x!) set-x!)
      ((eq? m 'draw!) draw!)
      (else
       (error "Wrong message sent! - dispatch-general-adt:" m))))
  dispatch-general-adt)


;--------------------------------------------------;
;                  Platform ADT                    ;
;--------------------------------------------------;
;De argument type zegt welke platform er getekend moet worden, aangezien
;er drie types platformen zijn.

(define (make-platform position type)
  (define (get-x)
    (car (position 'get-position)))

  (define (get-y)
    (cdr (position 'get-position)))
  
  (define (set-x! new-x)
    ((position 'set-x!) new-x))

  (define (set-y! new-y)
    ((position 'set-y!) new-y))

  (define (draw! teken-adt)
    ((teken-adt 'draw-platform!) dispatch-platform))
    
  (define (dispatch-platform m)
    (cond
      ((eq? m 'get-x) (get-x))
      ((eq? m 'get-y) (get-y))
      ((eq? m 'set-y!) set-y!)
      ((eq? m 'set-x!) set-x!)
      ((eq? m 'get-type) type)
      ((eq? m 'draw!) draw!)
      (else
       (error "Wrong message sent! - dispatch-platform:" m))))
  dispatch-platform)


;--------------------------------------------------;
;                   Positie ADT                    ;
;--------------------------------------------------;
;Elke ADT krijgt het Positie ADT als argument.

(define (maak-position x y)
  
  (define (get-position)
    (cons x y))
  
  (define (set-x! new-x)
    (set! x new-x))

  (define (set-y! new-y)
    (set! y new-y))

  (define (dispatch-position m)
    (cond
      ((eq? m 'get-position) (get-position))
      ((eq? m 'set-x!) set-x!)
      ((eq? m 'set-y!) set-y!)
      (else
       (error "Wrong message sent - dispatch-position:" m))))
  dispatch-position)
;-----------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                    Mario ADT                     ;
;--------------------------------------------------;

(define (maak-mario position)
  (let ((y-memory 0) ;y-memory bevat de y-positie van Mario vóór het springen.
        (moving-value (/ 7 canvas-width)) ;moving-value is de waarde waarmee Mario verplaatst wordt wanneer Mario naar links of naar rechts gaat.
        (jumping-value (/ -5 canvas-length)) ;jumping-value is de waarde waarmee Mario, elke x milliseconden, stijgt/daalt nadat het toets voor het springen ingedrukt werd.
        (climbing-speed (/ 5 canvas-length)) ;climing-speed is de waarde waarmee Mario verplaatst wordt bij het klimmen van ladders. 
        (ladder-bound (/ 5 canvas-width)) ;ladder-bound is het toegelaten afstand tussen Mario en een ladder om Mario te laten klimmen. 
        (left-bound (/ (- canvas-width 55) canvas-width)) ;Mario is niet toegelaten om verder dan left-bound te gaan.
        (right-bound 0) ;Mario is niet toegelaten om verder dan right-bound te gaan.
        (lives 3) ;Aantal levens van Mario. 
        (level 1) ;Level is de niveau (platform) waar Mario zich bevindt.
        (maximum-height (/ 48 canvas-length))
        (face 'right) ;face bevat de richting waar Mario aan het kijken is.
        (changed? #f) ;Dit variabele is een signaal voor het teken-adt om de tile-sequence van Mario te veranderen.
        (up? #f) ;Dit variabele geeft aan of Mario in de lucht is of niet.
        (has-to-go-up? #f) ;has-to-up? is een signaal voor de spellus om Mario constant te laten stijgen.
        (on-ladder? #f) ;Dit variabele geeft aan of Mario op een ladder is of niet.
        (went-down? #f)
        (has-to-go-down? #f)) ;Dit variabele zorgt voor het laten dalen van Mario wanneer hij van een platform komt.

    (define (get-x)
      (car (position 'get-position)))

    (define (get-y)
      (cdr (position 'get-position)))
    
    (define (set-x! new-x)
      ((position 'set-x!) new-x))

    ;Bij het muteren van de y-positie wordt ook y-memory aangepast.
    (define (set-y! new-y)
      ((position 'set-y!) new-y)
      (set! y-memory new-y))

    ;De procedure move-right! wordt opgeroepen om Mario naar rechts te laten lopen.
    ;De body van de procedure wordt enkel uitgevoerd indien Mario niet op een ladder is en niet
    ;uit een platform aan het vallen is
    (define (move-right! platforms)
      (when (and (not on-ladder?)(not has-to-go-down?))
        (let* ((mario-x-pos (get-x))
               (actual-platform (list-ref platforms (- level 1)))
               (actual-platform-x-pos (actual-platform 'get-x))
               (type (actual-platform 'get-type))
               (right-bound (cond
                              ((= type 1)
                               platform1-width)
                              ((= type 2)
                               platform2-width)
                              ((= type 3)
                               platform3-width)))) ;De type van de ladder is nodig om de right-bound te bepalen
          ;aangezien er verschillende platformen zijn met verschillende
          ;lengtes en dus met verschillende right-bounds.
          
          ;Eerst wordt er bekeken of Mario niet naar rechts kijkt (dit is het geval wanneer we juist
          ;naar links zijn gelopen). Indien dat het geval is, wordt de variabele face gelijk gesteld
          ;aan 'right, en changed? wordt naar #t gelijk gesteld zodat bij het herteken naar Mario
          ;het Teken-ADT het tile-sequence van Mario verandert.
          (when (not (eq? face 'right))
            (set! face 'right)
            (set! changed? #t))
          ;In alle andere gevallen, indien Mario nog niet aan de right-bound geraakt is, wordt
          ;de x-positie van Mario met moving-value vermeerderd.
          (cond
            ((and (or (odd? level)(even? level))(= type 2)(> mario-x-pos (+ actual-platform-x-pos (- right-bound (/ 35 canvas-width)))))
             (set-x! (* mario-x-pos 1)))
            ((and (odd? level)(> mario-x-pos (+ actual-platform-x-pos (- right-bound (/ 35 canvas-width)))))
             (set-x! (* mario-x-pos 1)))
            ((and (even? level)(> mario-x-pos (+ actual-platform-x-pos right-bound)))
             (set! has-to-go-down? #t)) ;has-to-go-down? wordt naar #t gezet omdat Mario uit een een platform
            ;is gevallen.
            (else
             (set-x! (+ mario-x-pos moving-value)))))))

    ;De procedure move-left! doet hetzelfde als move-right! maar in de tegengestelde richting.
    (define (move-left! platforms)
      (when (and (not on-ladder?)(not has-to-go-down?))
        (let* ((mario-x-pos (get-x))
               (actual-platform (list-ref platforms (- level 1)))
               (actual-platform-x-pos (actual-platform 'get-x))
               (type (actual-platform 'get-type)))
          (when (not (eq? face 'left))
            (set! face 'left)
            (set! changed? #t))
          (cond
            ((and (or (odd? level)(even? level))(= type 2)(< mario-x-pos (+ actual-platform-x-pos (/ 5 canvas-width))))
             (set-x! (* mario-x-pos 1)))
            ((and (even? level)(< mario-x-pos (+ actual-platform-x-pos (/ 5 canvas-width))))
             (set-x! (* mario-x-pos 1)))
            ((and (odd? level)(<= mario-x-pos (- actual-platform-x-pos (/ 35 canvas-width))))
             (set! has-to-go-down? #t))
            (else
             (set-x! (- mario-x-pos moving-value)))))))

    ;Pull-down is de procedure die ervoor zorgt dat Mario, bij het vallen van een platform,
    ;naar beneden valt.
    (define (pull-down! platforms)
      (when has-to-go-down?
        (let ((mario-y-pos (get-y))
              (platform-beneath-y-pos ((list-ref platforms (- level 2)) 'get-y))) 
          (when (>= mario-y-pos (- platform-beneath-y-pos platform-length (/ 30 canvas-length)))
            (set! has-to-go-down? #f)
            (set! level (- level 1)))
          (set-y! (+ mario-y-pos climbing-speed)))))
            
    ;De procedure jump! wordt opgeroepen enkel wanneer Mario springt.
    ;De body van de procedure wordt enkel uitgevoerd indien Mario niet op een ladder is.
    (define (jump!)
      (when (and (not on-ladder?)(not has-to-go-down?))
        (let ((mario-y-pos (get-y))) 
          (cond
            ;Indien het spellus Mario nog niet aan het laten stijgen is (dus als has-to-go-up? niet #t is),
            ;wordt has-to-go-up naar #t gezet, en wordt ook up? naar #t gezet aangezien nu Mario in de lucht is.
            ((not has-to-go-up?)
             (set! has-to-go-up? #t)
             (set! up? #t)
             ((position 'set-y!) (+ mario-y-pos jumping-value)))
            ;Indien het verschil tussen de actuele y-positie van Mario en de y-positie vóór het springen
            ;groter is dan het toegelaten hoogte, wordt de jumping-value vermenigvuldigd met 1 zodat Mario
            ;naar beneden begint te dalen.
            ((>= (- y-memory mario-y-pos) maximum-height)
             (set! jumping-value (* -1 jumping-value))
             ((position 'set-y!) (+ mario-y-pos jumping-value)))
            ;Als Mario terug op grond is, dus als de y-positie van Mario terug gelijk is aan de y-positie
            ;vóór het springen, moeten alle variabelen die verandert zijn terug naar hun initiële staat gebracht worden
            ((>= mario-y-pos y-memory)
             (set! jumping-value (* -1 jumping-value))
             (set! up? #f)
             (set! has-to-go-up? #f)
             ;Uiteindelijk wordt changed? naar #t gezet om aan de teken-adt te zeggen dat de
             ;tile-sequence veranderd moet worden.
             (set! changed? #t))
            (else
             ((position 'set-y!) (+ mario-y-pos jumping-value)))))))

    ;Deze procedure wordt gebruikt om Mario op een ladder te laten klimmen
    ;De body van de procedure wordt enkel uitgevoerd indien Mario niet aan het springen is.
    (define (climb-up! ladders)
      (when (not up?)
        (let ((mario-x-pos (get-x))
              (mario-y-pos (get-y)))
          ;Aangezien de ladders in een vector zitten, moet er opzoek gegaan worden naar de ladder
          ;waarbij Mario wil klimmen. Dit opzoekwerk wordt uitgevoerd door de procedure search-ladder.
          (define (search-ladder ladders)
            (let ((length (vector-length ladders)))
              (let loop ((i 0))
                (when (not (>= i length))
                  (let ((level-ladder (vector-ref (vector-ref ladders i) 0))
                        (ladder-x-pos (vector-ref (vector-ref ladders i) 1)))
                    ;Als de "level" van Mario gelijk is aan de level van de ladder, en als de x-positie
                    ;van Mario in een bepaalde grens zit die bepaald wordt door de x-positie van de ladder
                    ;te verminderen/vermeerderen met de ladder-bound, dan betekent dit dat de bijhorende ladder
                    ;gevonden werd, en dus wordt de variabele on-ladder? naar #t gezet.
                    ;Anders, blijven we verder zoeken.
                    (if (and (= level level-ladder) (and (>= mario-x-pos (- ladder-x-pos ladder-bound)) (<= mario-x-pos (+ ladder-bound ladder-x-pos))))
                        (set! on-ladder? #t)
                        (loop (+ i 1))))))))
      
          (cond ((and on-ladder? went-down? (= mario-y-pos y-memory))
                 (set! on-ladder? #f)
                 (set! changed? #t))
                ;Als Mario op de ladder zit en als zijn y-positie kleiner of gelijk is
                ;aan zijn y-positie vóór het klimmen van de ladder, vermindert met de lengte van de ladder
                ;en de lengte van de platform, dan betekent dit dat Mario helemaal tot boven is geraakt.
                ;Eens Mario helemaal tot boven is geraakt wordt changed? naar #t gezet om een signaal te geven
                ;aan het teken adt, on-ladder? wordt naar #f gezet om te zeggen dat Mario niet meer op de ladder zit,
                ;de level van Mario wordt met ééntje verhoogt, en uiteindelijk wordt y-memory gewijzigd naar de
                ;nieuwe positie.
                ((and on-ladder? (<= mario-y-pos (- y-memory ladder-length platform-length)))
                 (set! changed? #t)
                 (set! on-ladder? #f)
                 (set! level (+ level 1))
                 (set! y-memory mario-y-pos))
                ;Als Mario gewoon op de ladder zit, wordt simpelweg de y-positie van Mario aangepast. 
                (on-ladder? 
                 ((position 'set-y!) (- mario-y-pos climbing-speed)))
                ;Het grote nut om een variabele on-ladder? te hebben, is om ervoor te zorgen dat het opzoeken
                ;van de ladder niet bij elke oproep van de procedure "climb-up!" gebeurt. Dus, als on-ladder? niet
                ;#t is betekent dit dat het opzoeken van de ladder nog niet gebeurt is en moet het gebeuren.
                (else 
                 (search-ladder ladders))))))

    ;Climb-down! is het tegengestelde van climb-up!, maar met enkele verschillen na.
    (define (climb-down! ladders)
      (when (not up?)
        (let ((mario-x-pos (get-x))
              (mario-y-pos (get-y)))
          (define (search-ladder ladders)
            (let ((length (vector-length ladders)))
              (let loop ((i 0))
                (when (not (>= i length))
                  (let ((level-ladder (vector-ref (vector-ref ladders i) 0))
                        (ladder-x-pos (vector-ref (vector-ref ladders i) 1)))
                    ;Hier wordt er niet bekeken of de level van Mario gelijk is aan de level van de ladder, maar als
                    ;de niveau van Mario - 1 gelijk is aan de level van de ladder. Dit wordt gedaan omdat wij niet opzoek
                    ;zijn naar de ladder die op hetzelfde platform is als Mario, maar die eronder.
                    (if (and (= (- level 1) level-ladder) (and (>= mario-x-pos (- ladder-x-pos ladder-bound)) (<= mario-x-pos (+ ladder-bound ladder-x-pos))))
                        (begin (set! on-ladder? #t)
                               (set! went-down? #t))
                        (loop (+ i 1))))))))

          ;Went-down: Als hij nog niet "naar beneden is gegaan" en hij altijd op een ladder zit, hoeft er om de y-memory te laten "zakken" zoals in de 2de cond-tak.
          ;Voorbeeld van toepassing van went-down: Stel Mario staat op de tweede verdieping. Wanneer hij naar beneden   gaat, wordt went-down op #t geset. Eens hij beneden is wordt
          ;                                        went-down op #f geset. Als ik daarna nog op de ladder klim, maar in het midden beslist op terug naar beneden te gaan, wordt
          ;                                        de eerste-tak van de conditional getriggerd.

          
          (cond ((and on-ladder? (not went-down?)(= mario-y-pos y-memory))
                 (set! on-ladder? #f)
                 (set! changed? #t))
                ((and on-ladder? (>= mario-y-pos (+ y-memory ladder-length platform-length)))
                 (set! changed? #t)
                 (set! on-ladder? #f)
                 (set! went-down? #f)
                 (set! level (- level 1))
                 (set! y-memory mario-y-pos))
                (on-ladder? 
                 ((position 'set-y!) (+ mario-y-pos climbing-speed)))
                (else 
                 (search-ladder ladders))))))

    ;Deze procedure wordt gebruikt om het aantal levens van Mario met ééntje te verminderen.
    (define (loose-life!)
      (set! lives (- lives 1)))

    ;Set-lives! past het aantal levens van Mario aan.
    (define (set-lives! number-of-lives)
      (set! lives number-of-lives))
    
    ;Deze procedure wordt gebruikt om alle verschillende variabelen naar hun initiële staat te brengen.
    (define (reset!)
      (when up?
        (set! jumping-value (/ -5 canvas-length))
        (set! up? #f)
        (set! has-to-go-up? #f))

      (when (eq? face 'left)
        (set! face 'right))
      
      (when on-ladder?
        (set! on-ladder? #f)
        (set! went-down? #f))
      (set! changed? #t)
      (set! level 1)
      (set-x! 0)
      (set-y! (- actual-canvas-length mario-length platform-length)))
    
    ;Het teken procedure van Mario neemt deze het Teken ADT en een symbol als parameter
    ;Het symbol wordt gebruikt om de tile-sequence van Mario te manipuleren.
    ;Als er geen symbol wordt gegeven dan wordt Mario gewoon hertekend. 
    (define (draw! teken-adt . m)
      ((teken-adt 'draw-mario!) dispatch-mario)
      (cond
        ((and (not (null? m))(eq? (car m) 'next))((teken-adt 'mario-next-tile!)))
        ((and (not (null? m))(eq? (car m) 'reset))((teken-adt 'mario-next-tile!) 0)))
      (set! changed? #f))
  
    (define (dispatch-mario m)
      (cond
        ((eq? m 'get-x) (get-x))
        ((eq? m 'get-y) (get-y))
        ((eq? m 'set-x!) set-x!)
        ((eq? m 'set-y!) set-y!)
        ((eq? m 'get-lives) lives)
        ((eq? m 'set-lives!) set-lives!)
        ((eq? m 'get-level) level)
        ((eq? m 'move-right!) move-right!)
        ((eq? m 'move-left!) move-left!)
        ((eq? m 'jump!)(jump!))
        ((eq? m 'climb-up!) climb-up!)
        ((eq? m 'climb-down!) climb-down!)
        ((eq? m 'loose-life!) (loose-life!))
        ((eq? m 'get-face) face)
        ((eq? m 'changed?) changed?)
        ((eq? m 'pull-down!) pull-down!)
        ((eq? m 'up?) up?)
        ((eq? m 'may-up?) has-to-go-up?)
        ((eq? m 'on-ladder?) on-ladder?)
        ((eq? m 'has-to-go-down?) has-to-go-down?)
        ((eq? m 'reset!)(reset!))
        ((eq? m 'draw!) draw!)
        (else
         (error "Wrong message sent! - dispatch-mario:" m))))
    dispatch-mario))
;-----------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                 Donkey Kong ADT                  ;
;--------------------------------------------------;

(define (maak-donkey-kong position)

  (define (get-x)
    (car (position 'get-position)))

  (define (get-y)
    (cdr (position 'get-position)))

  (define (set-x! new-x)
    ((position 'set-x!) new-x))

  (define (set-y! new-y)
    ((position 'set-y!) new-y))
  
  ;De draw procedure van Donkey Kong neemt, optioneel, een symbol
  ;als parameter. De nut van het symbol is om de volgende tile
  ;in de tile-sequence te laten verschijnen.
  (define (draw! teken-adt . m)
    (when (and (not (null? m))(eq? (car m) 'next))
      (teken-adt 'donkey-kong-next-tile!))
    ((teken-adt 'draw-donkey-kong!) dispatch-donkey-kong))
      
   
  (define (dispatch-donkey-kong m)
    (cond
      ((eq? m 'get-x) (get-x))
      ((eq? m 'get-y) (get-y))
      ((eq? m 'set-x!) set-x!)
      ((eq? m 'set-y!) set-y!)
      ((eq? m 'draw!) draw!)
      (else
       (error "Wrong message sent! - dispatch-donkeyKong:" m))))
  dispatch-donkey-kong)
;-------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                    Pauline ADT                   ;
;--------------------------------------------------;
(define (maak-pauline position)
  (let ((distance (/ 60 canvas-width))); Dit variabele bevat de afstand tussen Mario en Pauline
    
    (define (get-x)
      (car (position 'get-position)))

    (define (get-y)
      (cdr (position 'get-position)))
  
    (define (set-x! new-x)
      ((position 'set-x!) new-x))

    (define (set-y! new-y)
      ((position 'set-y!) new-y))

    ;Deze procedure wordt gebruikt om te weten of Pauline gered werd of niet.
    (define (saved? mario)
      (let ((mario-x-pos (mario 'get-x))                                                           
            (mario-y-pos (mario 'get-y))
            (pauline-x-pos (get-x))
            (pauline-y-pos (get-y)))
        ;Als Mario zich op een afstand kleiner dan distance bevindt, en de y-positie van Mario kleiner of gelijk is
        ;aan die van Pauline (daarbij wordt het verschil tussen de lengte van Pauline en Mario opgetelt), dan werd
        ;Pauline gered en wordt er #t gegeven. Anders, wordt er #f teruggegeven.
        (and (< (- mario-x-pos pauline-x-pos) distance) (<= mario-y-pos (+ pauline-y-pos (- pauline-length mario-length))))))

    ;De draw procedure van Pauline neemt, optioneel, een symbol als parameter.
    ;De nut van het symbol is om de volgende tile in de tile-sequence te laten verschijnen.
    (define (draw! teken-adt . m)
      (when (and (not (null? m))(eq? (car m) 'next))
        (teken-adt 'pauline-next-tile!)) 
      ((teken-adt 'draw-pauline!) dispatch-pauline))
    
    (define (dispatch-pauline m)
      (cond
        ((eq? m 'get-x) (get-x))
        ((eq? m 'get-y) (get-y))
        ((eq? m 'set-x!) set-x!)
        ((eq? m 'set-y!) set-y!)
        ((eq? m 'saved?) saved?)
        ((eq? m 'draw!) draw!)
        (else
         (error "Wrong message sent! - dispatch-pauline:" m))))
    dispatch-pauline))
;-----------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------;
;                    Barrel ADT                    ;
;--------------------------------------------------;
;Het Barrel ADT neemt als parameter, naast het Position ADT,
;een naam. Deze naam wordt gebruikt zodat het Teken ADT tegen
;het juiste barrel aanspreekt.

(define (maak-barrel position)
  (let ((level 6) ;Level van de barrel
        (falling-value (/ 5 canvas-length)) ;Waarde waarmee de barrel wordt verplaats bij het vallen van een platform
        (moving-value (/ 10 canvas-width)) ;Waarde waarmee de barrel wordt verplaats bij het bewegen van de barrel
        (collision-bound (/ 30 canvas-width)) ;Collision-bound is de grote van de afstand van Mario tegenover de barrel waarbij er van collision kan gesproken worden.
        (out? #f) ;Deze variabele wordt gebruikt om te weten of de barrels altijd getekend moeten worden of niet.
        (has-to-go-down? #f)) ;Dit variabele is een signaal voor de spellus om de barrel te laten dalen
 
    (define (get-x)
      (car (position 'get-position)))

    (define (get-y)
      (cdr (position 'get-position)))
    
    (define (set-x! new-x)
      ((position 'set-x!) new-x))

    (define (set-y! new-y)
      ((position 'set-y!) new-y))

    ;Deze procedure geeft aan of Mario zich juist boven de barrel bevindt
    (define (over-me? mario)
      (let ((mario-x-pos (mario 'get-x))
            (mario-y-pos (mario 'get-y))
            (mario-level (mario 'get-level))
            (barrel-x-pos (get-x))
            (barrel-y-pos (get-y)))
        (and (<= (abs (- mario-x-pos barrel-x-pos)) 0.1)(>= (abs (- mario-y-pos barrel-y-pos)) (/ 16 canvas-length))(= level mario-level))))

    ;Deze procedure wordt gebruikt om te weten of er een collision is tussen Mario
    ;en de barrel. 
    (define (collision? mario teken-adt)
      (let ((mario-level (mario 'get-level))
            (mario-x-pos (mario 'get-x))
            (mario-y-pos (mario 'get-y))
            (barrel-x-pos (get-x))
            (barrel-y-pos (get-y)))
        
        ;Indien de barrel op hetzelfde niveau is als Mario, en als de x-positie en y-positie van
        ;de barrel op een afstand collision-bound zitten van de x-positie en y-positie van Mario, dan is er
        ;collision.
        ;Bij collision moet Mario terug op zijn plaats worden gebracht, en daarom wordt 'reset opgeroepen. Ook moet
        ;er aan Mario een leven afgetrokken worden met 'loose-life!. Uiteindelijk, wordt er #t teruggegeven.
        ;
        ;Indien er geen collision is, wordt er gewoon #f terug.
        (if (and (= level mario-level)(<= (abs (- barrel-x-pos mario-x-pos)) collision-bound)(<= (abs (- barrel-y-pos mario-y-pos)) collision-bound))
            (begin (mario 'reset!)
                   (mario 'loose-life!)
                   ((mario 'draw!) teken-adt)
                   #t)
            #f)))

                      
                 

    ;De procedure move! zorgt voor de beweging van de barrel.
    (define (move! platform)
      ;Om de bewegingen van de barrels juist te kunnen regelen, zijn er enkele gegevens
      ;nodig van de platform die zich onder de barrel bevindt, en de platform waarop de barrel
      ;zich momenteel bevindt. Het is voor deze reden, dat de move! procedure de lijst van
      ;als parameter neemt.
      (let* ((actual-platform (list-ref platform (- level 1)))
             (actual-platform-x-pos (actual-platform 'get-x))
             (platform-beneath (when (not (= level 1))
                                 (list-ref platform (- level 2)))) 
             (platform-y-pos (when (not (= level 1))
                               (platform-beneath 'get-y)))
             (barrel-x-pos (get-x))
             (barrel-y-pos (get-y)))
        ;Wanneer de barrel nog niet naar beneden moet dalen, worden 2 zaken bekeken:
        (if (not has-to-go-down?)
            (begin (cond
                     ;Wanneer de barrel helemaal tot beneden gekomen is, wordt out? naar #t gezet
                     ;om te vermijden dat de barrel terug getekend wordt.
                     ((and (= level 1)(<= barrel-x-pos actual-platform-x-pos))
                      (set! out? #t))
                     ;Wanneer de barrel zich op het laatste niveau bevindt maar nog niet aan het einde
                     ;moet het simpelweg verplaatst worden.
                     ((= level 1)
                      (set-x! (+ barrel-x-pos moving-value)))
                     ;Als de barrel aan de linker - of rechter bound geraakt is, dan moeten de tonnen naar
                     ;beneden gebracht worden. Om dit te doen wordt has-to-go-down? naar #t gezet, en wordt
                     ;de waarde van moving-value aangepast.
                     ;Er wordt hier altijd bekeken of level even of oneven is om te vermijden dat de barrel
                     ;constant zal dalen
                     ((or (<= barrel-x-pos (- actual-platform-x-pos (/ 35 canvas-width)))(>= barrel-x-pos (+ actual-platform-x-pos platform1-width)));((or (and (even? level)(>= barrel-x-pos right-bound))(and (odd? level)(<= barrel-x-pos left-bound)))
                      (set! has-to-go-down? #t)
                      (set! moving-value (* -1 moving-value)))
                     (else
                      ;Anders, beweegt de barrel.
                      (set-x! (+ barrel-x-pos moving-value)))))
            ;Indien de barrel moet dalen, wordt er eerst bekeken of de barrel op de platform is aangeraakt.
            ;Als dat het geval is wordt has-to-go-down? naar #f gezet, aangezien de barrel niet meer moet dalen,
            ;en wordt de level vermindert. Anders, als de barrel nog niet op een platform is, laten we
            ;het simpelweg dalen.
            (begin (when (>= barrel-y-pos (- platform-y-pos platform-length (/ 20 canvas-length)))
                     (set! has-to-go-down? #f)
                     (set! level (- level 1)))
                   (set-y! (+ barrel-y-pos falling-value))))))

    ;Remove-barrel verwijdert de barrel uit de scherm
    (define (remove-barrel teken-adt)
      ((teken-adt 'delete-barrel) dispatch-barrel))

    ;Create-barrel genereert een barrel.
    (define (create-barrel teken-adt)
      ((teken-adt 'add-barrel) dispatch-barrel))
      
    (define (draw! teken-adt)
      ((teken-adt 'draw-barrel!) dispatch-barrel))
     
    (define (dispatch-barrel m)
      (cond
        ((eq? m 'get-x) (get-x))
        ((eq? m 'get-y) (get-y))
        ((eq? m 'set-x!) set-x!)
        ((eq? m 'set-y!) set-y!)
        ((eq? m 'collision?) collision?)
        ((eq? m 'move!) move!)
        ((eq? m 'level) level)
        ((eq? m 'out?) out?)
        ((eq? m 'over-me?) over-me?)
        ((eq? m 'create-barrel) create-barrel)
        ((eq? m 'remove-barrel) remove-barrel)
        ((eq? m 'draw!) draw!)
        (else
         (error "Wrong message sent - dispatch-barrel" m))))
    dispatch-barrel))
;-----------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------;
;                     Ghost ADT                    ;
;--------------------------------------------------;

(define (make-ghost positie)
  (let ((moving-value (/ 5 canvas-width)) ;Snelheid waarmee de spook zich verplaatst
        (face 'right) ;Kant waarnaa het spook kijkt
        (changed? #f) ;Variabele die aangeeft aan het teken-adt dat de Tile-sequence verandert moet worden.
        (level 1)) ;De variabele level wordt gebruikt als constante waarmee er vermenigvuldigt kan worden
    ;om de spook op het juiste platform te zetten.

    (define (get-x)
      (car (positie 'get-position)))

    (define (get-y)
      (cdr (positie 'get-position)))

    (define (set-x! new-x)
      ((positie 'set-x!) new-x))

    (define (set-y! new-y)
      ((positie 'set-y!) new-y))

    (define (set-level! new-level)
      (set! level new-level))

    ;Om de spook correct te laten bewegegen zijn er gegevens nodig van de platform
    ;waarop de spook zich momenteel bevindt. Daarom worden actual-platform en
    ;actual-platform-x-pos gebruikt.
    (define (move! platforms)
      (let* ((x-pos (get-x))
             (actual-platform (list-ref platforms (- level 1)))
             (actual-platform-x-pos (actual-platform 'get-x))
             (bound (cond 
                      ((= (actual-platform 'get-type) 1)
                       platform1-width)
                      ((= (actual-platform 'get-type) 2)
                       platform2-width)
                      ((= (actual-platform 'get-type) 3)
                       platform3-width))))     
        (cond
          ((< x-pos actual-platform-x-pos) ;Indien de spook helemaal naar links is aangekomen...
           (set! moving-value (* moving-value -1))
           (set-x! (+ x-pos moving-value))
           (set! face 'right)
           (set! changed? #t))
          ((> x-pos (+ actual-platform-x-pos (- bound (/ 40 canvas-width)))) ;Indien de spook helemaal naar rechts is aangekomen...
           (set! moving-value (* moving-value -1))
           (set-x! (+ x-pos moving-value))
           (set! face 'left)
           (set! changed? #t))
          (else
           (set! x-pos (+ x-pos moving-value))
           (set-x! x-pos)))))

    (define (collision? mario teken-adt)
      (let ((mario-x-pos (mario 'get-x))
            (mario-y-pos (mario 'get-y))
            (ghost-x-pos (get-x))
            (ghost-y-pos (get-y)))
        (if (and (< (abs (- mario-y-pos ghost-y-pos)) (/ 50 canvas-length))(< (abs (- mario-x-pos ghost-x-pos)) (/ 20 canvas-width)))
            (begin (mario 'loose-life!)
                   (mario 'reset!)
                   ((mario 'draw!) teken-adt)
                   #t)
            #f)))

    ;Genereren van de spook
    (define (create-ghost teken-adt)
      (let ((y-pos (- (- actual-canvas-length platform-length) ghost-length (* (/ 85 canvas-length) (- level 1)))))
        (set-y! y-pos)
        ((teken-adt 'add-ghost) dispatch-ghost)))

    ;Dezelfde principe als de teken-procedure van Donkey Kong, Pauline, enz...
    (define (draw! teken-adt . m)
      (when (and (not (null? m))(eq? (car m) 'next))
        ((teken-adt 'ghost-next-tile!) dispatch-ghost))
      ((teken-adt 'draw-ghost) dispatch-ghost)
      (when changed?
        (set! changed? #f)))

    (define (dispatch-ghost m)
      (cond
        ((eq? m 'get-x)(get-x))
        ((eq? m 'get-y)(get-y))
        ((eq? m 'set-x!) set-x!)
        ((eq? m 'set-y!) set-y!)
        ((eq? m 'set-level!) set-level!)
        ((eq? m 'changed?) changed?)
        ((eq? m 'get-face) face)
        ((eq? m 'move!) move!)
        ((eq? m 'get-level) level)
        ((eq? m 'collision?) collision?)
        ((eq? m 'create-ghost) create-ghost)
        ((eq? m 'draw!) draw!)
        (else
         (error "Wrong message sent - dispatch-ghost" m))))
    dispatch-ghost))
;-----------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                    Flames ADT                    ;
;--------------------------------------------------;
;Dezelde als de spoken met enkele verschillen na.
(define (make-flame position)
  (let  ((moving-value (/ 10 canvas-width)) 
         (face 'right)
         (changed? #f)
         (level 1))
    
    (define (get-x)
      (car (position 'get-position)))

    (define (get-y)
      (cdr (position 'get-position)))

    (define (set-x! new-x)
      ((position 'set-x!) new-x))

    (define (set-y! new-y)
      ((position 'set-y!) new-y))

    (define (set-level! new-level)
      (set! level new-level))

    (define (over-me? mario)
      (let ((mario-x-pos (mario 'get-x))
            (mario-y-pos (mario 'get-y))
            (mario-level (mario 'get-level))
            (flame-x-pos (get-x))
            (flame-y-pos (get-y)))
        (and (<= (abs (- mario-x-pos flame-x-pos)) 0.1) (and (>= (abs (- mario-y-pos flame-y-pos)) (/ 25 canvas-length))
                                                             (<= (abs (- mario-y-pos flame-y-pos)) (/ 40 canvas-length))) (= level mario-level))))

    (define (move! platforms)
      (let ((x-pos (get-x))
            (actual-platform-x-pos ((list-ref platforms (- level 1)) 'get-x)))
        (cond
          ((< x-pos actual-platform-x-pos)
           (set! moving-value (* moving-value -1))
           (set-x! (+ x-pos moving-value))
           (set! face 'right)
           (set! changed? #t))
          ((> x-pos (- (+ actual-platform-x-pos platform2-width) (/ 25 canvas-width)))
           (set! moving-value (* moving-value -1))
           (set-x! (+ x-pos moving-value))
           (set! face 'left)
           (set! changed? #t))
          (else
           (set! x-pos (+ x-pos moving-value))
           (set-x! x-pos)))))

    (define (collision? mario teken-adt)
      (let ((mario-x-pos (mario 'get-x))
            (mario-y-pos (mario 'get-y))
            (mario-level (mario 'get-level))
            (flame-x-pos (get-x))
            (flame-y-pos (get-y)))
        (if (and (< (abs (- mario-y-pos flame-y-pos)) (/ 25 canvas-length))(< (abs (- mario-x-pos flame-x-pos)) (/ 25 canvas-width))(= level mario-level))
            (begin (mario 'loose-life!)
                   (mario 'reset!)
                   ((mario 'draw!) teken-adt)
                   #t)
            #f)))
          
    (define (create-flame teken-adt)
      (let ((y-pos (- (- actual-canvas-length platform-length) flame-length (* (/ 85 canvas-length) (- level 1)))))
        (set-y! y-pos)
        ((teken-adt 'add-flame) dispatch-flame)))

    (define (draw! teken-adt . m)
      (when (and (not (null? m))(eq? (car m) 'next))
        ((teken-adt 'flame-next-tile!) dispatch-flame))
      ((teken-adt 'draw-flame) dispatch-flame)
      (when changed?
        (set! changed? #f)))


    (define (dispatch-flame m)
      (cond
        ((eq? m 'get-x)(get-x))
        ((eq? m 'get-y)(get-y))
        ((eq? m 'set-x!) set-x!)
        ((eq? m 'set-y!) set-y!)
        ((eq? m 'set-level!) set-level!)
        ((eq? m 'changed?) changed?)
        ((eq? m 'get-face) face)
        ((eq? m 'move!) move!)
        ((eq? m 'set-level!) set-level!)
        ((eq? m 'over-me?) over-me?)
        ((eq? m 'collision?) collision?)
        ((eq? m 'create-flame) create-flame)
        ((eq? m 'draw!) draw!)
        (else
         (error "Wrong message sent - dispatch-flame" m))))
    dispatch-flame))


;-----------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------;
;                    Lava ADT                      ;
;--------------------------------------------------;
(define (make-lava position)
  (let ((drawn? #f) ;Variabele die aan het teken-adt aangeeft of de lava zich al op het scherm bevindt
        (growing-factor (/ 3 canvas-length))) ;De waarde waarmee de lava moet groeien
    
    (define (get-x)
      (car (position 'get-position)))

    (define (get-y)
      (cdr (position 'get-position)))

    (define (set-x! new-x)
      ((position 'set-x!) new-x))

    (define (set-y! new-y)
      ((position 'set-y!) new-y))

    ;Deze procedure zorgt ervoor dat de lava groeit. De y-positie verandert aangezien de lava
    ;naar beneden groeit en moet dus de tile aangepast worden om de veranderingen te kunnen zien.
    (define (lava-erupts! teken-adt)
      (let ((lava-y-pos (get-y)))
        (set-y! (- lava-y-pos growing-factor))
        ((teken-adt 'grow-lava) growing-factor))) 
      
    (define (mario-burned? mario teken-adt)
      (let ((mario-y-pos (mario 'get-y))
            (lava-y-pos (get-y)))
        (if (<= lava-y-pos (- (+ mario-y-pos mario-length) (/ 20 canvas-length)))
            (begin (teken-adt 'resize-lava)
                   (mario 'loose-life!)
                   (mario 'reset!)
                   ((mario 'draw!) teken-adt)
                   #t)
            #f)))

    ;Lava-reset! verkleint terug de lava.
    (define (lava-reset! teken-adt)
      (teken-adt 'resize-lava))

    (define (draw! teken-adt)
      ((teken-adt 'draw-lava) dispatch-lava)
      (when (not drawn?)
        (set! drawn? #t)))

    ;Remove-lava! zorgt dat de lava verdwijnt.
    (define (remove-lava! teken-adt)
      (teken-adt 'remove-lava)
      (set! drawn? #f))

    (define (dispatch-lava m)
      (cond
        ((eq? m 'get-x) (get-x))
        ((eq? m 'get-y) (get-y))
        ((eq? m 'set-x!) set-x!)
        ((eq? m 'set-y!) set-y!)
        ((eq? m 'drawn?) drawn?)
        ((eq? m 'remove-lava!) remove-lava!)
        ((eq? m 'lava-erupts!) lava-erupts!)
        ((eq? m 'mario-burned?) mario-burned?)
        ((eq? m 'lava-reset!) lava-reset!)
        ((eq? m 'draw!) draw!)
        (else
         (error "Wrong message sent - dispatch-lava" m))))
    dispatch-lava))
;-----------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                    Spelmenu ADT                  ;
;--------------------------------------------------;
;Het spelmenu wordt onderverdeelt in twee procedures: main-menu & game-over!.
;Main-menu handelt de menu die in het begin van het spel gezien kan worden,
;Game-over! handelt de menu die op einde van het spel gezien kan worden.

(define (maak-spelmenu)
  (let ((select 0)) ;Aangezien de in de main-menu 4 opties mogelijk zijn, zal de variabele select bijhouden
    ;in welke van de 4 opties de speler zich bevindt. Optie 0 is wanneer de pijl naar "start" wijst,
    ;optie 1 is wanneer de pijl naar "highscores" wijst, optie 2 is wanneer de pijk naar "exit" wisjt,
    ;en optie 3 is wanneer de speler zich in de high scores bevindt.
    
    ;Deze procedure zordt dat de volgende Tile, in de Tile-sequence van de levens,
    ;verschijnen
    (define (loose-life! teken-adt)
      (teken-adt 'loose-life!))

    ;De databank wordt hier als parameter gegeven om de high score (bij het spel zelf) en de high scores (bij de menu)
    ;te laten verschijnen
    (define (main-menu teken-adt m . dispatch-spel&database)
      ;De main-menu moet twee situaties kunnen ondersteunen: - Het starten van het spel
      ;                                                      - Het bekijken van de high scores.
      ;                                                      - Het verlaten van het spel
      ;Deze worden ondersteunt door te kijken waar de speler zich momenteel bevindt op het menu.
      (cond
        ((eq? m 'next)(when (and (not (= select 3))(not (= select 2)))
                        (set! select (+ select 1))
                        ((teken-adt 'main-menu-tile) 'next)))
        ((eq? m 'previous)(when (and (not (= select 3))(not (= select 0)))
                            (set! select (- select 1))
                            ((teken-adt 'main-menu-tile) 'previous)))
        ((eq? m 'exec)
         (cond ((= select 0) ;Als de pijl naar "start" wijst...
                (let ((dispatch (car dispatch-spel&database))
                      (database (car (cdr dispatch-spel&database))))
                  ((teken-adt 'main-menu-tile) 'remove ((database 'ask) "SELECT MAX(Score) FROM Highscores"));-> wordt de high score getekend
                  (dispatch 'niveau-1)));-> en wordt niveau 1 opgestart.
               ((= select 1) ;Als de pijl naar "highscores" wijst...
                (let ((database (car (cdr dispatch-spel&database))))
                  ((teken-adt 'main-menu-tile) 'highscore ((database 'ask) "SELECT * FROM Highscores ORDER BY Score Desc")) ;-> worden alle highscores die zich op de databank bevinden getekend.
                  (set! select 3)))
               ((= select 2)
                (exit))
               ((= select 3)
                (set! select 0)
                ((teken-adt 'main-menu-tile) 'back-to-menu))))
        (else
         (error "Wrong message sent - main-menu: " m))))

    ;Het menu bij het einde van het spel moet twee dingen kunnen doen: - alle elementen die zich op het window bevinden verplaatsen
    ;                                                                  - het spel kunnen herstarten.
    (define (game-over! teken-adt m . lst-characters)
      ;Om het eerste te kunnen doen, wordt de procedure set-off gebruikt die
      ;als parameter een lijst krijgt met alle objecten diet uit het scherm moeten gebracht
      ;worden. Set-off zal dan alle objecten buiten spel brengen.
      (define (set-off lst)
        (define (set-position! character x y)
          ((character 'set-x!) x)
          ((character 'set-y!) y)
          ((character 'draw!) teken-adt))
      
        (when (not (null? lst))
          (let* ((first-character (car lst))
                 (object (car (cdr first-character))))
            (cond
              ((eq? (car first-character) 'mario)
               (object 'reset!)
               ((object 'set-lives!) 3)
               (set-position! object (/ -40 canvas-width) 0)
               (set-off (cdr lst)))
              ((eq? (car first-character) 'pauline)
               (set-position! object (/ -100 canvas-width) 0)
               (set-off (cdr lst)))
              ((eq? (car first-character) 'donkey-kong)
               (set-position! object (/ -120 canvas-width) 0)
               (set-off (cdr lst)))               
              (else
               (error "Wrong lst - set-off: " lst))))))
  
      (cond
        ;Wanneer m gelijk is aan 'end-game, is het spel over en moeten alle;
        ;elementen uit de spel gebracht worden. Dit wordt bezorgt door de procedure set-off
        ((eq? m 'end-game)
         (set-off (car lst-characters))
         ;Het bericht 'over zal in het Teken ADT de tile van de eind-menu toevoegen. In de cadr
         ;van de parameter lst-characters staat dan of de Tile die moet verschijnen de "victory-tile" is of
         ;de "game-over-tile" Tile.
         ((teken-adt 'game-over-tile) 'over (cadr lst-characters)))  ;cadr = victory or defeat symbol                
        ((eq? m 'restart-game)
         ;het bericht 'restart zal in het Teken ADT de tile van de eind-menu
         ;weghalen
         ((teken-adt 'game-over-tile) 'restart))
        ;In alle andere gevallen zit in de parameter m de letter die de speller aan het typen is om zijn score
        ;te "submitten".
        (else
         ((teken-adt 'write-name) (list->string (list m))))))
  
    (define (dispatch-spelmenu m)
      (cond
        ((eq? m 'main-menu) main-menu)
        ((eq? m 'game-over!) game-over!)
        ((eq? m 'loose-life!) loose-life!)
        (else
         (error "Wrong message sent! - dispatch-spelmenu:" m))))

    dispatch-spelmenu))
;-----------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                     Clock ADT                    ;
;--------------------------------------------------;
;De Clock ADT onthoudt hoeveel tijd er nog overblijft vooraleer de spel eindigt.
(define (make-timer min sec)
  
  (define (set-time! m s)
    (set! min m)
    (set! sec s))

  ;Hier wordt string-append gebruikt om ervoor te zorgen dat de tijd in deze manier wordt afgebeeld:
  ; "01:32" en niet "1:32"
  (define (get-time)
    (let ((minutes (if (> min 9)
                       (number->string min)
                       (string-append "0" (number->string min))))
          (seconds (if (> sec 9)
                       (number->string sec)
                       (string-append "0" (number->string sec)))))
      (cons minutes seconds)))

  ;Clock-tick vermindert de tijd
  (define (clock-tick)
    (if (and (not (= min 0))(= sec 0))
        (begin (set! min (- min 1))
               (set! sec 59))
        (set! sec (- sec 1))))

  ;Procedure die aangeeft of het tijd over is
  (define (times-up?)
    (and (= min 0)(= sec 0)))

  ;Teken procedure.
  (define (update-time! teken-adt) 
    ((teken-adt 'draw-time) dispatch-timer))

  (define (dispatch-timer m)
    (cond
      ((eq? m 'set-time!) set-time!)
      ((eq? m 'get-time)(get-time))
      ((eq? m 'clock-tick)(clock-tick))
      ((eq? m 'times-up?)(times-up?))
      ((eq? m 'update-time!) update-time!)
      (else
       (error "Wrong message sent - dispatch-timer" m))))
  dispatch-timer)

;-----------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                   Databank ADT                   ;
;--------------------------------------------------;
;Om de hoogste scores te kunnen bij houden werd er gebruikt gemaakt van een databank
;De databank voert twee operaties uit, namekelijj het opvragen van gegevens en het
;wijzigen van de databank.
(define (make-database name-database)
  (let ((database '()))

    (define (ask query)
      (set! database (sqlite3-connect #:database name-database #:mode 'read-only))
      (define result (query-rows database query))
      (disconnect database)
      result)

    (define (change! query)
      (set! database (sqlite3-connect #:database name-database #:mode 'read/write))
      (query-exec database query)
      (when (> (vector-ref (car (query-rows database "SELECT COUNT(Player) FROM Highscores")) 0) 10)
        (query-exec database "DELETE FROM Highscores WHERE Score = (SELECT MIN(Score) FROM Highscores);"))
      (disconnect database))

    (define (dispatch-database m)
      (cond
        ((eq? m 'ask) ask)
        ((eq? m 'change!) change!)
        (else
         (error "Wrong message sent - dispatch-database" m))))
    dispatch-database))
      
;----------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------;
;                    Score ADT                     ;
;--------------------------------------------------;
;De Score ADT onthoudt de punten
(define (make-score)
  (let ((score 0))
    
    (define (add-points points-to-add teken-adt)
      (set! score (+ score points-to-add))
      ((teken-adt 'update-score) score))

    (define (reset-score!)
      (set! score 0))

    (define (dispatch-score m)
      (cond
        ((eq? m 'get-score) score)
        ((eq? m 'reset-score!) (reset-score!))
        ((eq? m 'add-points) add-points)
        (else
         (error "Wrong message sent - dispatch-score" m))))
    dispatch-score))
;----------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------;
;                     Spel ADT                     ;
;--------------------------------------------------;
;Het Spel ADT bestaat uit

(define (spel-adt)

  (let (
        ;-----------------------------------------;
        ; Aanmaak van de objecten die zich al bij ;
        ;     het begin van het spel bevinden     ;
        ;-----------------------------------------;
        (teken-adt (maak-teken-adt "Mario" canvas-width canvas-length))
        (spelmenu (maak-spelmenu))
        (mario (maak-mario (maak-position 0 0)))
        (pauline (maak-pauline (maak-position 0 0)))
        (donkey-kong (maak-donkey-kong (maak-position 0 0)))
        (database (make-database "db/Highscores.db"))
        (timer (make-timer 0 0))
        (score (make-score))
        
        ;-----------------------------------------------;
        ; Aanmaak van de variabelen die bij het starten ;
        ; van de verschillende niveaus aangetast zullen ;
        ;                    worden                     ;
        ;-----------------------------------------------;
        (platforms '())
        (ladders '())
        (ghosts '())
        (barrels '())
        (lava '())
        (flames '())
        (player '())
        
        ;----------------------------------------------;
        ; Definieren van de variabelen die door de lus ;
        ;   van alle niveaus moeten aangetast worden   ;
        ;----------------------------------------------;
        (left-keydown? #f) ;Left-keydown? geeft aan of de linkertoets ingedrukt is of niet. Dit variabele is
        ;van belang om Mario te animeren bij het stappen naar links.
        (right-keydown? #f) ;Right-keydown? geeft aan of de rechtertoets ingedrukt is of niet. Dit variabele is
        ;van belang om Mario te animeren bij het stappen naar rechts.
        (up-right? #f);Up-right? en up-left? geven de toestemming aan het spellus om Mario, diagonaal
        (up-left? #f) ;Te laten springen
        (climbing? #f);Climbin?g geeft aan of Mario aan het klimmen is of niet. Dit variabele is van belang
        ;om Mario te animeren bij het klimmen van de ladders.
        (collision? #f) ;Collision? geeft aan of Mario obstakels heeft aangeraakt.
        (elapsed-time 0) ;Tijd die vergaan is
        (donkey-kong-time 0)
        (pauline-time 0) 
        (mario-time 0) 
        (up-time 0) 
        (diagonal-time 0))

    ;-------------------------------------;
    ;                                     ;
    ; Hulpfuncties om de de verschillende ;
    ;       elementen te positioneren     ;
    ;                                     ;
    ;-------------------------------------;
    (define (set-position! character x y)
      ((character 'set-x!) x)
      ((character 'set-y!) y)
      ((character 'draw!) teken-adt))
  
    (define (positionate-elements lst)
      (define (draw-platforms)
        (let loop ((y-pos (- actual-canvas-length platform-length))
                   (i 0))
          (if (not (>= i (length platforms)))
              (let ((platform (list-ref platforms i)))
                (cond
                  ((= (platform 'get-type) 3)
                   ((platform 'draw!) teken-adt))
                  ((and (even? i)(not (= i 0))(not (= (platform 'get-type) 2)))
                   ((platform 'set-x!) (/ 65 canvas-width))
                   ((platform 'set-y!) y-pos)
                   ((platform 'draw!) teken-adt))
                  (else
                   ((platform 'set-y!) y-pos)
                   ((platform 'draw!) teken-adt)))
                (loop (- y-pos (/ 85 canvas-length))(+ 1 i)))  ;(+ ladder-length platform-length)
              #f)))
    
      (define (draw-ladders)
        (let loop ((i 0))
          (if (not (>= i (vector-length ladders)))
              (let* ((ladder-info (vector-ref ladders i))
                     (ladder (vector-ref ladder-info 2))
                     (level (vector-ref ladder-info 0))
                     (ladder-x (vector-ref ladder-info 1)))
                (set-position! ladder ladder-x (- ((list-ref platforms (- level 1)) 'get-y) ladder-length))
                (loop (+ i 1)))
              #f)))
      
      (define (draw-ghosts list-of-ghosts)
        (if (null? list-of-ghosts)
            (positionate-elements (cdr lst))
            (let* ((first-ghost-element (car list-of-ghosts))
                   (level (car first-ghost-element))
                   (ghost (cdr first-ghost-element)))
              ((ghost 'set-level!) level)
              ((ghost 'set-x!) ((list-ref platforms (- level 1)) 'get-x))
              ((ghost 'create-ghost) teken-adt)
              ((ghost 'draw!) teken-adt)
              (draw-ghosts (cdr list-of-ghosts)))))


      (define (draw-flames flame-list position-list)
        (if (and (null? flame-list)(null? position-list))
            (positionate-elements (cdr lst))
            (let* ((first-el (car flame-list))
                   (level (car first-el))
                   (first-flame (cdr first-el))
                   (first-position (car position-list))
                   (x-pos (car first-position)))
              ((first-flame 'set-level!) level)
              ((first-flame 'create-flame) teken-adt)
              ((first-flame 'set-x!) x-pos)
              ((first-flame 'draw!) teken-adt)
              (draw-flames (cdr flame-list) (cdr position-list)))))

      (draw-platforms)
      (draw-ladders)
      
      (when (not (null? lst))
        (let* ((first-list (car lst))
               (character (car first-list)))
          (cond ((eq? character 'ghosts)
                 (draw-ghosts (car (cdr first-list))))
                ((eq? character 'flames)
                 (draw-flames flames (cadr first-list)))
                (else
                 (let ((x-position (car (car (cdr first-list))))
                       (y-position (cdr (car (cdr first-list)))))
                   (cond
                     ((eq? character 'mario)
                      (set-position! mario x-position y-position)
                      (positionate-elements (cdr lst)))
                     ((eq? character 'pauline)
                      (set-position! pauline x-position y-position)
                      (positionate-elements (cdr lst)))
                     ((eq? character 'donkey-kong)
                      (set-position! donkey-kong x-position y-position)
                      (positionate-elements (cdr lst)))
                     ((eq? character 'lava)
                      (set-position! lava x-position y-position)
                      (positionate-elements (cdr lst)))
                     (else
                      (error "Character not known:" character)))))))))

    ;De procedure reset! gaat alle variabelen terug naar hun initiële staat brengen.
    (define (reset!)
      (set! left-keydown? #f)
      (set! right-keydown? #f)
      (set! up-right? #f)
      (set! up-left? #f)
      (set! climbing? #f)
      (set! collision? #f))

    (define (key-detection x)
      (cond
        ;Wanneer de rechtertoets ingedrukt wordt, en wanneer Mario niet aan het springen is, wordt right-keydown?
        ;naar #t gezet om aan te geven dat de rechtertoets ingedrukt werd, en wordt Mario naar rechts verplaatst.
        ((eq? x 'right)
         (when (not (mario 'up?))
           (set! right-keydown? #t)
           ((mario 'move-right!) platforms)
           ((mario 'draw!) teken-adt)))
        ((eq? x 'left)
         ;Wanneer de rechtertoets ingedrukt wordt, en wanneer Mario niet aan het springen is, wordt right-keydown?
         ;naar #t gezet om aan te geven dat de rechtertoets ingedrukt werd, en wordt Mario naar rechts verplaatst.
         (when (not (mario 'up?))
           (set! left-keydown? #t) 
           ((mario 'move-left!) platforms)
           ((mario 'draw!) teken-adt)))
        ;Wanneer de boventoets ingedrukt wordt, wordt climbing? naar #t gezet om het klimmen van Mario te animeren.
        ;Daarna, wordt er aan Mario gevraagdr om te klimmen.
        ((eq? x 'up)
         (set! climbing? #t)
         ((mario 'climb-up!) ladders)
         ((mario 'draw!) teken-adt))
        ;Zelfde als hierboven, maar in de tegengestelde richting.
        ((eq? x 'down)
         (set! climbing? #t)
         ((mario 'climb-down!) ladders)
         ((mario 'draw!) teken-adt))
        ;Wanneer de spatiebalk ingedrukt wordt, wordt er bekeken of Mario al niet naar links of rechts aan het lopen
        ;was. Indien dat het geval was, wordt up-left? of up-right? naar #t gezet om Mario diagonaal te laten
        ;springen. Uiteindelijk, gaat Mario springen.
        ((eq? x #\space)
         (cond
           (left-keydown? (set! up-left? #t))
           (right-keydown? (set! up-right? #t)))
         (mario 'jump!))
        ;Wanneer er geen toets ingedrukt wordt, moet de juiste animatie gestopt worden 
        ((eq? x 'release)
         (when left-keydown?
           (set! left-keydown? #f))
         (when right-keydown?
           (set! right-keydown? #f))
         (when climbing?
           (set! climbing? #f))
         (when (not (mario 'up?))
           ((mario 'draw!) teken-adt 'reset)))))

    (define (basic-animations x)
      ;Basic-animations bevat alle animaties die doorheen alle niveaus moeten gebeuren. Deze procedure wordt
      ;in elke loop doorgegeven.
      
      (if (> donkey-kong-time 500)
          (begin ((donkey-kong 'draw!) teken-adt 'next)
                 (set! donkey-kong-time 0))
          (set! donkey-kong-time (+ donkey-kong-time x)))

      (if (> pauline-time 1000)
          (begin ((pauline 'draw!) teken-adt 'next)
                 (set! pauline-time 0))
          (set! pauline-time (+ pauline-time x)))

      ;Wanneer de rechter- of linkertoets ingedrukt is, en dat Mario niet
      ;op een ladder zit, moet er elke 200 milliseconden d volgende Tile in
      ;de Tile-sequence verschijnen.
      (when (and (or right-keydown? left-keydown?)(not (mario 'on-ladder?)))
        (if (> mario-time 200)
            ;Eerst wordt er bekeken of Mario in de lucht is. Als dat het geval is moet de 
            ;volgende Tile in de Tile-sequence niet verschijnen maar moet Mario enkel
            ;hertekend worden.
            (begin (if (mario 'up?)
                       ((mario 'draw!) teken-adt)
                       ((mario 'draw!) teken-adt 'next))
                   (set! mario-time 0))
            (set! mario-time (+ mario-time x))))

      (when (and climbing? (mario 'on-ladder?))
        (if (> mario-time 200)
            (begin ((mario 'draw!) teken-adt 'next)
                   (set! mario-time 0))
            (set! mario-time (+ mario-time x))))

      ;Als Mario aan het springen is, wordt elke 15 millieseconden zijn Tile naar boven
      ;getrokken. Als hij dan terug naar beneden is, worden up-right? en up-left? naar #f.
      (if (mario 'may-up?)
          (if (> up-time 15)
              (begin (mario 'jump!)
                     (set! up-time 0)
                     ((mario 'draw!) teken-adt))
              (set! up-time (+ up-time x)))
          (cond
            (up-right? (set! up-right? #f))
            (up-left? (set! up-left? #f))))

      ;Up-right? en up-left? worden naar #t gezet wanneer Mario aan het lopen is en er opeens
      ;wordt besloten om te springen. Het doel is om Mario diagonaal te laten springen.
      ;Om dit te doen, moet Mario naar links/recht bewegegen gedurend het springen. 
      (when up-right?
        (if (> diagonal-time 2)
            (begin (mario 'move-right!)
                   (set! diagonal-time 0)
                   ((mario 'draw!) teken-adt))
            (set! diagonal-time (+ diagonal-time x))))
              
      (when up-left?
        (if (> diagonal-time 2)
            (begin (mario 'move-left!)
                   (set! diagonal-time 0)
                   ((mario 'draw!) teken-adt))
            (set! diagonal-time (+ diagonal-time x))))

      ;Wanneer er een colission is, moet Mario een leven verliezen en moeten alle
      ;variabelen die aangepast werden tijdens het spel terug naar hun initiele staat gebracht worden
      ;aan de hand van de procedure reset!
      (when collision?
        ((spelmenu 'loose-life!) teken-adt)
        (reset!))

      ;Wanneer Mario geen levens meer heeft, moeten alle elementen uit het spel gebracht worden,
      ;dit gebeurt door aan de spelmenu een lijst te geven met alle elementen erin, en de bericht
      ;'defeat door te geven om de "game-over-tile" te laten verschijnen.
      (when (= (mario 'get-lives) 0)
        ((spelmenu 'game-over!) teken-adt 'end-game (list (list 'mario mario)
                                                          (list 'pauline pauline)
                                                          (list 'donkey-kong donkey-kong)) 'defeat)
        ((teken-adt 'update-loop) (lambda (x) x))
        ((teken-adt 'onkeydown) end-key-detection))
      
      (if (>= elapsed-time 1000)
          (begin (timer 'clock-tick)
                 ((timer 'update-time!) teken-adt)
                 (set! elapsed-time 0))
          (set! elapsed-time (+ elapsed-time x)))

      (when (timer 'times-up?)
        ((mario 'set-lives!) 0)))


    ;-----------------------------------------------------------------------------------------------------------------

    ;-------------------------------------;
    ;               Niveau 1              ;
    ;-------------------------------------;
    (define (niveau-1)
      (set! ladders (vector (vector 1 (/ 110 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 2 (/ 400 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 3 (/ 300 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 3 (/ 120 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 4 (/ 80 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 4 (/ 350 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 5 (/ 420 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 6 (/ 275 canvas-width) (make-ladder (maak-position 0 0)))))
      
      (set! platforms (list (make-platform (maak-position 0 0) 2)
                            (make-platform (maak-position 0 0) 1)
                            (make-platform (maak-position 0 0) 1)
                            (make-platform (maak-position 0 0) 1)
                            (make-platform (maak-position 0 0) 1)
                            (make-platform (maak-position 0 0) 1)
                            (make-platform (maak-position (/ 190 canvas-width) (/ 143 canvas-length)) 3)))
      
      (set! ghosts (list (cons 3 (make-ghost (maak-position 0 0)))))
      
      ;---------------------------------------;
      ;                                       ;
      ; Tekenen van de verschillende objecten ;
      ;                                       ;
      ;---------------------------------------;
      ((timer 'set-time!) 2 0)
      ((timer 'update-time!) teken-adt)
      ;Character-positions is een lijst bestaande uit lijsten waarbij er telkens gezegd wordt,
      ;aan welke element van het spel we de posities willen gaan aanpassen, en wat de aanpassingen
      ;moeten zijn. Deze lijst wordt aan de procedure positionate-elements gegeven, die dan alle
      ;elementen gaat plaatsen en tekenen.
      (define character-positions (list (list 'mario (cons 0 (- actual-canvas-length mario-length platform-length)))
                                        (list 'pauline (cons (/ 185 canvas-width) (/ 91 canvas-length)))
                                        (list 'donkey-kong (cons (/ 60 canvas-width) (/ 148 canvas-length)))
                                        (list 'ghosts ghosts)))

      (positionate-elements character-positions)

     
      ;Hier wordt een supplementaire Tile toegevoegt die enkel "versiering" als nut hebben
      ((teken-adt 'add-tile!) (/ 5 canvas-width) (donkey-kong 'get-y) "img/barells.png" "img/barells_mask.png")

      ;---------------------------------------;
      ;                  Lus                  ;
      ;---------------------------------------;

      (define (start)
        ;Variabelen die de tijd handelen
        (let ((barrel-time 0)
              (creation-time 0)
              (ghost-time 0)
              (pull-time 0)
              (cleaning-time 0)
              (point-verification 0)
              (verify-point? #f))
        
          (define (game-loop x)

            ;Elke vijf seconde wordt een nieuwe barrel aangemaakt
            (if (> creation-time 5000)
                (let ((barrel (maak-barrel (maak-position 0 0)))
                      (x-pos (/ 85 canvas-width))
                      (y-pos (/ 200 canvas-length)))
                  ((barrel 'create-barrel) teken-adt)
                  ((barrel 'set-x!) x-pos)
                  ((barrel 'set-y!) y-pos)
                  ((barrel 'draw!) teken-adt)
                  (set! barrels (cons barrel barrels))
                  (set! creation-time 0))
                (set! creation-time (+ creation-time x)))

            
            (if (>= barrel-time 3)
                (begin (for-each (lambda (barrel)
                                   ;Er wordt bekeken of Mario zich boven een barrel bevindt, en dat er
                                   ;geen "punten validatie" aan het gebeuren is.
                                   (cond ((and ((barrel 'over-me?) mario)(not verify-point?))
                                          ((barrel 'move!) platforms)
                                          ((barrel 'draw!) teken-adt)
                                          (set! verify-point? #t)) ;-> als Mario zich boven een ton bevindt, moet de
                                         ;punt gevalideerd worden.
                                         (((barrel 'collision?) mario teken-adt)
                                          (set! point-verification 0)
                                          (set! verify-point? #f)
                                          (set! collision? #t)
                                          (set! barrels '())
                                          ;Bij collision met een barrel moeten de spoken terug naar hun plaats
                                          ;worden gebracht.
                                          (for-each (lambda (ghost)
                                                      (let ((ghost-level (ghost 'get-level)))
                                                        ((ghost 'set-x!) ((list-ref platforms (- ghost-level 1)) 'get-x))
                                                        ((ghost 'draw!) teken-adt)))
                                                    (map (lambda (g)
                                                           (cdr g))
                                                         ghosts)))
                                         ;Als de barrel buiten spel is dan wordt die verwijdert.
                                         ((barrel 'out?)
                                          ((barrel 'remove-barrel) teken-adt)
                                          (set! barrels (drop-right barrels 1))) 
                                         (else
                                          ((barrel 'move!) platforms)
                                          ((barrel 'draw!) teken-adt))))
                                 barrels)
                       (set! barrel-time 0))
                (set! barrel-time (+ barrel-time x)))

            ;Deze deel van de lus zorgt ervoor dat er bekeken wordt of Mario wel degelijk over een
            ;barrel is gesprongen. Dit wordt gedaan door te kijken of er na 2 seconden Mario een obstakel
            ;heeft aangeraakt (zowel een spook als een barrel). Indien Mario een obstakel heet aangeraakt
            ;wordt het punt niet gevalideerd, anders wel.
            (when verify-point?
              (if (and (> point-verification 1500)(not (mario 'on-ladder?)))
                  (cond
                    ((not collision?)
                     (set! point-verification 0)
                     (set! verify-point? #f)
                     ((score 'add-points) 100 teken-adt))
                    ((collision?
                      (set! point-verification 0)
                      (set! verify-point? #f)
                      (set! barrels '()))))
                  (set! point-verification (+ point-verification x))))
            
            ;Hier wordt er elke 30 millieseconde bekeken of er een collision is met een spook.
            (if (> ghost-time 30)
                (for-each (lambda (ghost)
                            (let ((ghost-level (ghost 'get-level)))
                              (if ((ghost 'collision?) mario teken-adt)
                                  (begin (set! barrels '())
                                         (set! point-verification 0)
                                         (set! verify-point? #f)
                                         (set! collision? #t)
                                         ;Bij collision worden alle spoken terug naar hun plaats gebracht
                                         (for-each (lambda (ghost)
                                                     ((ghost 'set-x!) ((list-ref platforms (- ghost-level 1)) 'get-x))
                                                     ((ghost 'draw!) teken-adt))
                                                   (map (lambda (g)
                                                          (cdr g))
                                                        ghosts)))
                                  (begin ((ghost 'move!) platforms)
                                         ((ghost 'draw!) teken-adt 'next)
                                         (set! ghost-time 0)))))
                          (map (lambda (g)
                                 (cdr g))
                               ghosts))
                (set! ghost-time (+ ghost-time x)))

            (when (mario 'has-to-go-down?)
              (if (> pull-time 5)
                  (begin (set! pull-time 0)
                         ((mario 'pull-down!) platforms)
                         ((mario 'draw!) teken-adt))
                  (set! pull-time (+ pull-time x))))

            (basic-animations x)

            ;Als Pauline gered werd, start het niveau 2
            (when ((pauline 'saved?) mario)
              (set! ghosts '())
              (mario 'reset!)
              (dispatch-spel-adt 'niveau-2))

            )
      
          ;Zetten van de callbacks.
          ((teken-adt 'update-loop) game-loop)
          ((teken-adt 'onkeydown) key-detection)))

      (start)) 
    
    ;-------------------------------------;
    ;               Niveau 2              ;
    ;-------------------------------------;
    
    (define (niveau-2)
      (teken-adt 'clean-up) ;Het Teken-ADT kuist de scherm op
      (set! ladders (vector (vector 1 (/ 110 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 2 (/ 400 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 3 (/ 300 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 3 (/ 120 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 4 (/ 80 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 4 (/ 350 canvas-width) (make-ladder (maak-position 0 0)))
                            (vector 5 (/ 420 canvas-width) (make-ladder (maak-position 0 0)))))
      
      (set! platforms (list (make-platform (maak-position 0 0) 2)
                            (make-platform (maak-position 0 0) 2)
                            (make-platform (maak-position 0 0) 2)
                            (make-platform (maak-position 0 0) 2)
                            (make-platform (maak-position 0 0) 2)
                            (make-platform (maak-position 0 0) 2)))
      
      (set! lava (make-lava (maak-position 0 0)))
      
      (set! flames (list (cons 1 (make-flame (maak-position 0 0)))
                         (cons 2 (make-flame (maak-position 0 0)))
                         (cons 2 (make-flame (maak-position 0 0)))
                         (cons 3 (make-flame (maak-position 0 0)))
                         (cons 3 (make-flame (maak-position 0 0)))
                         (cons 5 (make-flame (maak-position 0 0)))
                         (cons 5 (make-flame (maak-position 0 0)))))
      
      ((timer 'set-time!) 2 30)
      ((timer 'update-time!) teken-adt)

      (define character-positions (list (list 'mario (cons 0 (- actual-canvas-length mario-length platform-length)))
                                        (list 'pauline (cons (/ 185 canvas-width) (/ 176 canvas-length)))
                                        (list 'donkey-kong (cons (/ 60 canvas-width) (/ 148 canvas-length)))
                                        (list 'lava (cons (/ 0 canvas-width) (- actual-canvas-length (/ 8 canvas-length))))
                                        (list 'flames (list (cons (/ 400 canvas-width) 0)(cons 0 0)(cons (/ 500 canvas-width) 0) (cons 0 0)(cons (/ 500 canvas-width) 0)(cons 0 0)(cons (/ 475 canvas-width) 0)))))

      (positionate-elements character-positions)
      
      
      (define (start)
        (let ((growing-time 0)
              (flame-time 0)
              (flame-animation 0)
              (point-verification 0)
              (verify-point? #f))
          
          (define (game-loop x)
          
            (if (> growing-time 2000)
                (begin (if ((lava 'mario-burned?) mario teken-adt)
                           (begin ((lava 'set-y!) (- actual-canvas-length (/ 8 canvas-length)))
                                  ((lava 'draw!) teken-adt)
                                  (set! growing-time 0)
                                  (for-each (lambda (flame x-pos)
                                              ((flame 'set-x!) x-pos)
                                              ((flame 'draw!) teken-adt))
                                            (map (lambda(f)
                                                   (cdr f))
                                                 flames)
                                            (list (cons (/ 400 canvas-width) 0)(cons 0 0)(cons (/ 500 canvas-width) 0) (cons 0 0)(cons (/ 500 canvas-width) 0)(cons 0 0)(cons (/ 475 canvas-width) 0)))
                                  (set! collision? #t))
                           (begin ((lava 'lava-erupts!) teken-adt)
                                  ((lava 'draw!) teken-adt)
                                  (set! growing-time 0))))
                (set! growing-time (+ growing-time x)))

            ;Elke 500 millieseconden wordt er bekeken of Mario zich boven een vlam bevindt of als Mario
            ;een vlam heeft aangeraakt.
            (if (> flame-time 300)
                (begin (for-each (lambda(flame)
                                   (cond
                                     ((and ((flame 'over-me?) mario)(not verify-point?))
                                      (set! verify-point? #t)
                                      ((flame 'move!) platforms)
                                      ((flame 'draw!) teken-adt 'next))
                                     (((flame 'collision?) mario teken-adt)
                                      ((lava 'set-y!) (- actual-canvas-length (/ 8 canvas-length)))
                                      ((lava 'lava-reset!) teken-adt)
                                      ((lava 'draw!) teken-adt)
                                      (set! verify-point? #f)
                                      (set! point-verification 0)
                                      (for-each (lambda (flame x-pos)
                                                  ((flame 'set-x!) x-pos)
                                                  ((flame 'draw!) teken-adt))
                                                (map (lambda(f)
                                                       (cdr f))
                                                     flames)
                                                (list (/ 400 canvas-width) 0  (/ 500 canvas-width) 0 (/ 500 canvas-width) 0 (/ 475 canvas-width)))
                                      (set! collision? #t))
                                     (else
                                      ((flame 'move!) platforms)
                                      ((flame 'draw!) teken-adt 'next))))
                                 (map (lambda(f)
                                        (cdr f))
                                      flames))                 
                       (set! flame-time 0))
                (set! flame-time (+ flame-time x)))

            (if (> flame-animation 1000)
                (begin (for-each (lambda (flame)
                                   ((flame 'move!) platforms 'next))
                                 (map (lambda (f)
                                        (cdr f))
                                      flames))
                       (set! flame-animation 0))
                (set! flame-time (+ flame-time x)))

            (when verify-point?
              (if (and (> point-verification 2000)(not (mario 'on-ladder?)))
                  (cond
                    ((not collision?)
                     (set! point-verification 0)
                     (set! verify-point? #f)
                     ((score 'add-points) 100 teken-adt))
                    ((collision?
                      (set! point-verification 0)
                      (set! verify-point? #f))))
                  (set! point-verification (+ point-verification x))))
            
            (when ((pauline 'saved?) mario)
              ((spelmenu 'game-over!) teken-adt 'end-game (list (list 'mario mario)
                                                                (list 'pauline pauline)
                                                                (list 'donkey-kong donkey-kong)) 'victory)
              ((lava 'remove-lava!) teken-adt)
              (set! lava '())
              (set! flames '())
              (set! verify-point? #f)
              (set! point-verification #f)
              ((teken-adt 'update-loop) (lambda (x) x))
              ((teken-adt 'onkeydown) end-key-detection))
                             
              
            (basic-animations x))
        
          ((teken-adt 'update-loop) game-loop)
          ((teken-adt 'onkeydown) key-detection)))
      (start))

    ;-------------------------------------;
    ;  Toetsenbordfunctie voor main-menu  ;
    ;-------------------------------------;  

    (define (start-key-detection m)
      (cond ((eq? m 'right)
             ((spelmenu 'main-menu) teken-adt 'next))
            ((eq? m 'left)
             ((spelmenu 'main-menu) teken-adt 'previous))
            ((eq? m #\space)
             ((spelmenu 'main-menu) teken-adt 'exec dispatch-spel-adt database))))
    
    ;-------------------------------------;
    ;  Toetsenbordfunctie voor einde van  ;
    ;              het spel               ;
    ;-------------------------------------; 
    (define (end-key-detection key)
      (define (add-to-list lst key);Hulpfunctie
        (append lst (list key)))
      
      (cond
        ;Indien de speler geen naam heeft ingevoerd bij het submitten van de score wordt "Unknown"
        ;in de databank gezet
        ((and (eq? key #\space)(null? player))
         ((database 'change!) (string-append "INSERT INTO Highscores Values('" (symbol->string 'Unknown) "', " (number->string (score 'get-score)) ");"))
         ((spelmenu 'game-over!) teken-adt 'restart-game)
         ((teken-adt 'onkeydown) start-key-detection))
        ;Indien de speler zijn score submit, wordt de score met zijn naam in de databank ingevoerd.
        ((eq? key #\space)
         ((database 'change!) (string-append "INSERT INTO Highscores Values('" (list->string player) "', " (number->string (score 'get-score)) ");"))
         (set! player '())
         (score 'reset-score!)
         ((spelmenu 'game-over!) teken-adt 'restart-game)
         ((teken-adt 'onkeydown) start-key-detection))
        ;Anders, indien er nog plaats is, worden de getijpte letters op het scherm afgebeeld.
        ((and (not (= (length player) 7))(char? key))
         (set! player (add-to-list player key))
         ((spelmenu 'game-over!) teken-adt key))))
    
    (define (dispatch-spel-adt m)
      (cond ((eq? m 'start)((teken-adt 'onkeydown) start-key-detection))
            ((eq? m 'niveau-1)(niveau-1))
            ((eq? m 'niveau-2)(niveau-2))
            (else 
             (error "Wrong message sent - dispatch-spel-adt:" m))))

    dispatch-spel-adt))

;----------------------;
;                      ;
; Starten van het spel ;
;                      ;
;----------------------;
(define spel (spel-adt))
(spel 'start)