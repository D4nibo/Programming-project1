#lang racket
(provide canvas-length
         canvas-width
         actual-canvas-length
         platform-length
         platform1-width
         platform2-width
         platform3-width
         ladder-length
         donkey-kong-length
         pauline-length
         mario-length
         ghost-length
         flame-length)

(define canvas-length 680) ;Indien er gewenst wordt de vensterhoogte te veranderen, moet dit variabelen verandert worden.
(define canvas-width 540) ;Indien er gewenst wordt de vensterlengte te veranderen, moet dit variabelen gewijzigd worden.
(define actual-canvas-length (/ (- canvas-length 10)  canvas-length)) ;Kleine berekening om de hoogte van de spel zelf te hebben.
(define platform-length (/ 17 canvas-length)) ;Dit variabele bevat de lengte van de platormen ("Floor.png"). Indien er een andere beeld gekozen wordt, dient dit variabele aangepast te worden.
(define platform1-width (/ 457 canvas-width))
(define platform2-width (/ 524 canvas-width))
(define platform3-width (/ 113 canvas-width))
(define ladder-length (/ 68 canvas-length)) ;Dit variabele bevat de lengte van de ladders ("ladder.png"). Indien er een andere beeld gekozen wordt, dient dit variabele aangepast te worden.
(define donkey-kong-length (/ 80 canvas-length)) ;Dit variabele bevat de lengte van de Donkey Kong ("Kong1.png" en "Kong3.png"). Indien er een andere beeld gekozen wordt, dient dit variabele aangepast te worden.
(define pauline-length (/ 52 canvas-length)) ;Dit variabele bevat de lengte van de Pauline ("pauline.png"). Indien er een andere beeld gekozen wordt, dient dit variabele aangepast te worden.
(define mario-length (/ 40 canvas-length)) ;Dit variabele bevat de hoogte van de Mario ("marioRight.png"). Indien er een andere beeld gekozen wordt, dient dit variabele aangepast te worden.
(define ghost-length (/ 60 canvas-length))
(define flame-length (/ 25 canvas-length))
