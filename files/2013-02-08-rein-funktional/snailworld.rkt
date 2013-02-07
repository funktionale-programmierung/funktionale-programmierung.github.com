#lang racket/base

(require 2htdp/universe
         2htdp/image)

; Eine Position besteht aus:
; - X-Koordinate
; - Y-Koordinate
(struct pos (x y))

(define p1 (pos 5 6))    ; Position mit X=5, Y=6
(define p2 (pos 100 3))  ; Position mit X=100, Y=3
(define p3 (pos 150 20)) ; Position mit X=150, Y=3

; Ein Delta besteht aus:
; - Delta in X-Richtung
; - Delta in Y-Richtung
(struct delta (x y))

(define d1 (delta 1 3))  ; Delta mit X=1, Y=3
(define d2 (delta -2 3)) ; Delta mit X=-2, Y=3
(define d3 (delta -1 0)) ; Delta mit X=-1, Y=0

; Position in Richtung bewegen
; move: pos delta -> pos
(define move
  (lambda (p d)
    (pos (+ (pos-x p) (delta-x d))
         (+ (pos-y p) (delta-y d)))))

; Eine Schnecke hat:
; - Position
; - Bewegungsrichtung
(struct snail (pos dir))

(define s1 (snail p1 d1))
(define s2 (snail p2 d2))
(define s3 (snail p3 d3))

; Schnecke in gegebene Richtung bewegen
; move-snail-in-dir: snail delta -> snail
(define move-snail-in-dir
  (lambda (s d)
    (snail (move (snail-pos s) d)
           d)))

; Schnecke in ihrer Richtung bewegen
; move-snail: snail -> snail
(define move-snail
  (lambda (s)
    (move-snail-in-dir s (snail-dir s))))

; Eine Schneckenwelt besteht aus:
; - Schnecken
(struct snail-world (snails))

(define sw1 (snail-world (list s1 s2 s3)))

; Schnecke malen
; draw-snail: snail scene -> scene
(define draw-snail
  (lambda (s scene)
    (place-image (circle 5 "solid" "grey")
                 (pos-x (snail-pos s))
                 (pos-y (snail-pos s))
                 scene)))

(define width 640)
(define height 480)

; Schneckenwelt malen
; draw-snail-world: snail-world -> scene
(define draw-snail-world
  (lambda (sw)
    (foldl draw-snail
           (empty-scene width height)
           (snail-world-snails sw))))

; Schneckenwelt bewegen
; next-snail-world: snail-world -> snail-world
(define next-snail-world
  (lambda (sw)
    (snail-world
     (map move-snail
          (snail-world-snails sw)))))

(big-bang sw1
          (on-tick next-snail-world 0.2)
          (to-draw draw-snail-world width height))
