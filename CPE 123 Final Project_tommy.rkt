;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |CPE 123 Final Project_tommy|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define walls-widthr (- 900 16))
(define walls-lengthr (- 500 16))

(define-struct paddle [posn moving direction])
(define-struct ball [posn deltax deltay angle counter])
(define-struct ws [p1 p2 ball])

;a lob is a list of balls:
;'()
;(cons ball list-of-balls)

;ballBounce should:
;ball->ball
;use map
;(map (ballBounce ws) (

(define (ballsMove ws)
  (map ballMove (ws-ball ws)))
(define (ballsBounce ws)
  (map ballBounce (ws-ball ws)))



;p1 and p2 are inststances of paddle
(define (ballBounce b)
  (cond [(or  (and  (>= (posn-x (ball-posn b)) walls-widthr) (<= (posn-y (ball-posn   b)) 0))
              (and (>= (posn-x (ball-posn b)) walls-widthr) (>= (posn-y (ball-posn   b)) walls-lengthr))
              (and (<= (posn-x (ball-posn b)) 0)            (>= (posn-y (ball-posn   b)) walls-lengthr))
              (and (<= (posn-x (ball-posn  b)) 0)            (<= (posn-y (ball-posn  b)) 0)))
          (make-ball (ball-posn  b) (* -1 (ball-deltax  b)) (* -1 (ball-deltay  b)) (ball-angle  b) (+ 1 (ball-counter  b)))] 
        [(>= (posn-x (ball-posn  b)) walls-widthr)
          (make-ball (ball-posn  b) (* -1 (ball-deltax  b)) (ball-deltay  b) (ball-angle  b) (+ 1 (ball-counter  b)))] 
        [(<= (posn-x (ball-posn  b)) 0)
           (make-ball (ball-posn  b) (* -1 (ball-deltax  b)) (ball-deltay  b) (ball-angle  b) (+ 1 (ball-counter  b)))]
        [(>= (posn-y (ball-posn  b)) walls-lengthr)
           (make-ball (ball-posn  b) (ball-deltax   b) (* -1 (ball-deltay  b))(ball-angle  b) (+ 1 (ball-counter  b)))]
        [(<= (posn-y (ball-posn  b)) 0)
           (make-ball (ball-posn  b) (ball-deltax   b) (* -1 (ball-deltay  b))(ball-angle  b) (+ 1 (ball-counter  b)))]
        [else (make-ball (ball-posn  b) (ball-deltax   b) (ball-deltay  b) (ball-angle  b) (+ 1 (ball-counter  b)))]
        ))
;lets draw the ball

(define field (empty-scene 900 500))
(define gp (circle 12 "solid" "red"))
(place-image gp 100 100 field)

;Here I drawing an the ball on the canvas
;the function places the ball at the position at the current position
; added to the current velocity (deltax/deltay) of the ball.

;ballMove changes the ws so drawEmpty can move the ball
(define (ballMove b)   
  (make-ball
   (make-posn
    (+ (posn-x (ball-posn  b))
       (ball-deltax  b))
    (+ (posn-y (ball-posn  b))
       (ball-deltay  b)))
   (ball-deltax b)
   (ball-deltay  b)
   (ball-angle  b)
   (+ 1 (ball-counter b)))) 



(define (update-ball ws)
  (cond
    [(even? (ball-counter  (first (ws-ball ws)))) (make-ws (ws-p1 ws) (ws-p2 ws)(ballsMove ws))]
    [(odd? (ball-counter  (first (ws-ball ws)))) (make-ws (ws-p1 ws) (ws-p2 ws) (ballsBounce ws))]))


