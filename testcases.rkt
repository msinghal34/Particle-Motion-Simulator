#lang racket
(require "declarations.rkt")
(provide testList1 testList2 testList3 testList4)
(define testList1 
  (list
      (particle 800.0 (vec 300 500) (vec 0.0 0.0))
      (particle  40.0 (vec 100 500) (vec 0.0 0.0))  
      (particle 200.0 (vec 500 500) (vec 30.0 0.0))
      (particle 1000.0 (vec 600 500) (vec 0.0 15.0))
      (particle  40.0 (vec 100 100) (vec 10.0 0.0))  
      (particle 200.0 (vec 500 50) (vec 5.0 0.0))
      (particle 700.0 (vec 300 200) (vec 30.0 0.0))
      (particle 440.0 (vec 100 55) (vec 0.0 15.0))  
      (particle 200.0 (vec 50 500) (vec 0.0 30.0))
      (particle 50.0 (vec 300 150) (vec 0.0 0.0))))

(define testList2 
  (list
   (particle 40.0 (vec 300 600) (vec 30.0 0.0))
   (particle 50.0 (vec 300 50) (vec 0.0 0.0))
   (particle 100.0 (vec  300 300) (vec -20 0.0))
   (particle 400.0 (vec 30 25.0) (vec -20 0.0))
   (particle 50.0 (vec 100 50) (vec 0 -20))
   (particle 750.0 (vec 100 500) (vec 0.0 15))
   (particle 50.0 (vec 300 65) (vec 25 0.0))
   (particle 1000.0 (vec 300 100) (vec 15 0))
   (particle 700.0 (vec 90 500) (vec 0.0 -20))
   (particle 55.0 (vec 300 90) (vec 5 0.0))))
   
(define testList3 
  (list
   (particle 400.0 (vec 300 600) (vec 30.0 0.0))
   (particle 500.0 (vec 300 500) (vec 100.0 0.0))
   (particle 100.0 (vec  300 30) (vec -20 0.0))
   (particle 1000.0 (vec 300 250.0) (vec -20 0.0))
   (particle 550.0 (vec 100 500) (vec 0 -40))
   (particle 1000.0 (vec 400 500) (vec 0.0 15))
   (particle 500.0 (vec 30 650) (vec 25 0.0))
   (particle 1000.0 (vec 30 100) (vec 75 0))
   (particle 270.0 (vec 200 50) (vec 0.0 -20))
   (particle 550.0 (vec 300 90) (vec 5 0.0))))

(define testList4
  (list
   (particle 40000.0 (vec 100 500) (vec 0.0 0.0))
   (particle 200.0 (vec 500 500) (vec -10.0 30.0))))

