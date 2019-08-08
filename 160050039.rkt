#lang racket
(require "declarations.rkt")
(provide buildTree calcForces moveparticles)

;;;;;;;;;;;;;;;;;;;;;;;-- buildTree--;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mass-calculator particles)
  (if (null? particles) 0.0
      (+ (particle-mass (car particles)) (mass-calculator (cdr particles)))))
(define (posn-calculator particles)
  (define (helper particles)
    (if (null? particles) 0.0
        (+ (* (particle-mass (car particles)) (vec-x (particle-posn (car particles))))
           (helper (cdr particles)))))
  (define (helper2 particles)
    (if (null? particles) 0.0
        (+ (* (particle-mass (car particles)) (vec-y (particle-posn (car particles))))
           (helper2 (cdr particles)))))
  (define x (mass-calculator particles))
    
  (vec (/ (helper particles) x) (/ (helper2 particles) x)))
(define (buildTree initialArea particles)
  (cond [(= (length particles) 1)
         (let* ([a (car particles)])
           (gnode (particle-mass a) (particle-posn a)
                     '()))]
        [#t (let* ([res (filtering initialArea particles)])
              (gnode (mass-calculator particles) (posn-calculator particles)
                     (generate res)))]))

(define (generate res)
  (cond [(null? res) '()]
        [#t (let* ([a (cdar res)])
              (if (null? a) (cons (gnode 0.0 (vec 0.0 0.0) '()) (generate (cdr res)))
                      (cons (buildTree (caar res) a) (generate (cdr res)))))]))
(define (filter-particles particles condi l1 l2)
  (if (null? particles) (cons l1 l2)
      (let* ([a (car particles)]
             [b (particle-posn a)])
        (if (condi (vec-x b) (vec-y b)) (filter-particles (cdr particles) condi (cons a l1)  l2)
            (filter-particles (cdr particles) condi l1 (cons a l2))))))
(define (filtering initialArea particles)
  (let* ([llx (bbox-llx initialArea)]
         [lly (bbox-lly initialArea)]
         [rux (bbox-rux initialArea)]
         [ruy (bbox-ruy initialArea)]
         [bx (/ (+ llx rux) 2)]
         [by (/ (+ lly ruy) 2)]
         [c1 (lambda (x y) (if (and (and (< x bx) (>= x llx))
                                    (and (<= y ruy) (>= y by))) #t #f))]
         [c2 (lambda (x y) (if (and (and (<= x rux) (>= x bx))
                                    (and (<= y ruy) (>= y by))) #t #f))]
         [c3 (lambda (x y) (if (and (and (>= x llx) (< x bx))
                                    (and (>= y lly) (< y by))) #t #f))]
         [c4 (lambda (x y) (if (and (and (<= x rux) (>= x bx))
                                    (and (< y by) (>= y lly))) #t #f))]
         [b1 (bbox llx by bx ruy)]
         [b2 (bbox bx by rux ruy)]
         [b3 (bbox llx lly bx by)]
         [b4 (bbox bx lly rux by)]
         [x (filter-particles particles c1 '() '())]
         [a (car x)]
         [y (filter-particles (cdr x) c2 '() '())]
         [b (car y)]
         [z (filter-particles (cdr y) c3 '() '())]
         [c (car z)]
         [z2 (filter-particles (cdr z) c4 '() '())]
         [d (car z2)])
    (if (not (null? (cdr z2))) "error"
        (list (cons b1 a) (cons b2 b) (cons b3 c) (cons b4 d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; --- buildTree --- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;; --- calcForces --- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (calcForces initialArea tree particles)
  (if (null? particles) '()
      (cons (force (car particles) tree (side initialArea))
            (calcForces initialArea tree (cdr particles)))))
(define (force p tree side)
  (cond  [(= (gnode-mass tree) 0.0) (vec 0.0 0.0)]
         [(null? (gnode-subtrees tree)) (forceVal p (gnode-mass tree) (gnode-posn tree))]
         [(near? p tree side) (forcefromAll p (gnode-subtrees tree) side)]
         [#t (forceVal p (gnode-mass tree) (gnode-posn tree))]))
      
(define (side area)
  (abs (- (bbox-rux area) (bbox-llx area))))
(define (forcefromAll p l side)
  (if (null? l) (vec 0.0 0.0)
      (add (force p (car l) (/ side 2)) (forcefromAll p (cdr l) side))))
(define (forceVal p m2 pos2)
  (let* ([m1 (particle-mass p)]
         [pos1 (particle-posn p)]
         [d (distance pos1 pos2)]
         [x (- (vec-x pos2) (vec-x pos1))]
         [y (- (vec-y pos2) (vec-y pos1))])
    (if (and (= m1 m2) (= d 0)) (vec 0.0 0.0)
        (let* ([c (/ (* g m1 m2 ) (* d d d))])
          (vec (* c x) (* c y))))))
(define (distance pos1 pos2)
  (let* ([a (- (vec-x pos1) (vec-x pos2))]
         [b (- (vec-y pos1) (vec-y pos2))]
         [x (* a a)]
         [y (* b b)]
         [ans (+ x y)])
    (sqrt ans)))
(define (near? p tree side)
  (if (> (/ (distance (gnode-posn tree) (particle-posn p)) side) theta) #f #t))
(define (add a b)
  (vec (+ (vec-x a) (vec-x b))
       (+ (vec-y a) (vec-y b))))

;;;;;;;;;;;;;;;;;;;;;;; --- calcForces --- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;; --- moveparticles --- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (moveparticles particles forces)
  (zipwith fn particles forces))
(define (fn p f)
  (let* ([m (particle-mass p)]
         [pos (particle-posn p)]
         [vel (particle-velocity p)]
         [x (vec-x pos)]
         [y (vec-y pos)]
         [vx (vec-x vel)]
         [vy (vec-y vel)]
         [t timeslice]
         [ax (/ (vec-x f) m)]
         [ay (/ (vec-y f) m)])
    (particle (particle-mass p)
              (vec (+ x (* vx t) (* 0.5 ax t t))
                   (+ y (* vy t) (* 0.5 ay t t)))
              (vec (+ vx (* ax t))
                   (+ vy (* ay t))))))

;;;;;;;;;;;;;;;;;;;;;; --- moveparticles --- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;