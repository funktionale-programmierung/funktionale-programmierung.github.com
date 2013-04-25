#lang racket/base

(require 2htdp/image)
(require 2htdp/universe)

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

; Delta zwischen zwei  Positionen berechnen
; pos-delta: pos pos -> delta
(define pos-delta
  (lambda (p1 p2)
    (delta (- (pos-x p1) (pos-x p2))
           (- (pos-y p1) (pos-y p2)))))

; Quadrat einer Zahl
; sqr: number number -> number
(define sqr
  (lambda (x)
    (* x x)))

; Distanz eines Deltas
; distance: delta -> number
(define distance
  (lambda (d)
    (sqrt (+ (sqr (delta-x d)) (sqr (delta-y d))))))  

; Alternative Richtungen zur Bewegungsrechnung berechnen
; alternative-directions: delta -> (list-of delta)
(define alternative-directions
  (lambda (d)
    (let ((dx (delta-x d))
          (dy (delta-y d)))
      (list (delta dx dy)
            (delta dy dx)
            (delta (- dx) dy)
            (delta dx (- dy))
            (delta (- dx) (- dy))
            (delta (- dy) dx)
            (delta dy (- dx))
            (delta (- dy) (- dx))))))

; Eine Schnecke hat:
; - Identität
; - Position
; - Bewegungsrichtung
(struct snail (id pos dir))

(define s1 (snail 1 p1 d1))
(define s2 (snail 2 p2 d2))
(define s3 (snail 3 p3 d3))

; Schnecke in gegebene Richtung bewegen
; move-snail-in-dir: snail delta -> snail
(define move-snail-in-dir
  (lambda (s d)
    (snail (snail-id s)
           (move (snail-pos s) d)
           d)))

; Schnecke in ihrer Richtung bewegen
; Einfache Version:
; move-snail: snail -> snail
#;(define move-snail
  (lambda (s)
    (move-snail-in-dir s (snail-dir s))))

; move-snail: snail (list-of slimes) -> snail
(define move-snail
  (lambda (s slimes)
    (let ((d
           (ormap (lambda (d)
                    (if (slime-in-direction? s slimes d)
                        #false
                        d))
                  (alternative-directions (snail-dir s)))))
      (if d
          (move-snail-in-dir s d)
          s))))

; Liegt Schleim in einer bestimmten Richtung von der Schecke?
; slime-in-direction?: snail (list-of slime) delta -> boolean
(define slime-in-direction?
  (lambda (s slimes d)
     (ormap (lambda (sl)
               (and (not (equal? (snail-id s) (slime-id sl)))
                    (pos-in-slime? (move (snail-pos s) d) sl)))
            slimes)))

; Schnecke malen
; draw-snail: snail scene -> scene
(define draw-snail
  (lambda (s scene)
    (place-image (circle snail-radius "solid" "grey")
                 (pos-x (snail-pos s))
                 (pos-y (snail-pos s))
                 scene)))

; Eine Schleimspur besteht aus:
; - der Identität der zugehörigen Schnecke
; - der Position der Schleimspur
(struct slime (id pos))

; Ist eine Position innerhalb einer Schleimspur?
; pos-in-slime: pos slime -> boolean
(define pos-in-slime?
  (lambda (pos sl)
    (< (distance (pos-delta pos
                            (slime-pos sl)))
       (* 2 snail-radius))))

; Schleimspur malen
; draw-slime: slime scene -> scene
(define draw-slime
  (lambda (sl scene)
    (let ((p (slime-pos sl)))
      (place-image (circle snail-radius "solid" "green")
                   (pos-x p)
                   (pos-y p)
                   scene))))

; Radius einer Schnecke
(define snail-radius 5)

; Eine Schneckenwelt besteht aus:
; - Schnecken
; - Schleimspuren
(struct snail-world (snails slimes))

(define sw1 (snail-world (list s1 s2 s3) '()))

(define width 180)
(define height 150)

; Schneckenwelt malen
; draw-snail-world: snail-world -> scene
(define draw-snail-world
  (lambda (sw)
    (let* ((sc1
            (foldl draw-snail
                   (empty-scene width height)
                   (snail-world-snails sw)))
           (sc2
            (foldl draw-slime sc1
                   (snail-world-slimes sw))))
      sc2)))

; Schneckenwelt bewegen
; next-snail-world: snail-world -> snail-world
(define next-snail-world
  (lambda (sw)
    (let ((snails (snail-world-snails sw))
          (slimes (snail-world-slimes sw)))
      (snail-world
       (map (lambda (s)
              (move-snail s slimes))
            snails)
       (append (map (lambda (s)
                      (slime 
                       (snail-id s) (snail-pos s)))
                    snails)
               (snail-world-slimes sw))))))

(big-bang sw1
          (on-tick next-snail-world 0.2)
          (to-draw draw-snail-world width height))
