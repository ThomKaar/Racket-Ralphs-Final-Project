;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |CPE 123 Final Project_tommy|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define walls-widthr (- 900 16))
(define walls-lengthr (- 500 16))
(define paddle-width 10)
(define paddle-height 30)
(define screen-width 900)
(define screen-height 500)
(define init-dx 10)
(define init-dy 10)

(define-struct paddle [posn moving direction])
(define-struct ball [posn deltax deltay angle counter])
(define-struct ws [p1 p2 ball])



(define initial-state
  (make-ws (make-paddle (make-posn (/ screen-width 4) (-(/ screen-height 2))) #false 0) (make-paddle (make-posn (/ screen-width (/ 4 3)) (-(/ screen-height 2))) #false 0) (cons (make-ball (make-posn 450 250) init-dx init-dy 0 1) '())))

(define (key-down-handler ws ke)
  (cond [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "w"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 0) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "d"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 1) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "s"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 2) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "a"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 3) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "up"))
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 0) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "right"))
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 1) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "down"))
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 2) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "left"))
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 3) (ws-ball ws))]
        [else ws]))

(define (key-up-handler ws ke)
  (cond [(and (paddle-moving (ws-p1 ws)) (key=? ke "w"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 0) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "d"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 1) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "s"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 2) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "a"))
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 3) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "up"))
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 0) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "right"))
         (make-ws (ws-p1 ws)(make-paddle  (paddle-posn (ws-p2 ws)) #false 1) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "down"))
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 2) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "left"))
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 3) (ws-ball ws))]
        [else ws]))

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
(define (ballsDraw ws)
  (map ballDraw (ws-ball ws)))



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
    [(odd? (ball-counter   (first (ws-ball ws)))) (ballsMove  ws)]
    [(even? (ball-counter  (first (ws-ball ws)))) (ballsBounce ws)]))

(define (game-tick ws)
  (make-ws (update-p1 ws) (update-p2 ws) (update-ball ws)))

(define (update-p1 ws)
  (cond [(paddle-moving (ws-p1 ws))
         (cond [(= 0 (paddle-direction (ws-p1 ws)))
                (make-paddle (make-posn (posn-x (paddle-posn (ws-p1 ws)))
                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p1 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p1 ws))))]
                                              [else (posn-y (paddle-posn (ws-p1 ws)))]))
                             (paddle-moving (ws-p1 ws))
                             (paddle-direction (ws-p1 ws)))]
                [(= 1 (paddle-direction (ws-p1 ws)))
                 (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p1 ws))) (/ paddle-width 2)) (/ screen-width 2))
                                                         (+ 5 (posn-x (paddle-posn (ws-p1 ws))))]
                                                        [else (posn-x (paddle-posn (ws-p1 ws)))])
                                                  (posn-y (paddle-posn (ws-p1 ws))))
                                       (paddle-moving (ws-p1 ws))
                                       (paddle-direction (ws-p1 ws)))]
                [(= 2 (paddle-direction (ws-p1 ws)))
                 (make-paddle (make-posn (posn-x (paddle-posn (ws-p1 ws)))
                                         (cond [(< (+ (abs (posn-y (paddle-posn (ws-p1 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p1 ws))) 5)]
                                               [else (posn-y (paddle-posn (ws-p1 ws)))]))
                              (paddle-moving (ws-p1 ws))
                              (paddle-direction (ws-p1 ws)))]
                [(= 3 (paddle-direction (ws-p1 ws)))
                 (make-paddle (make-posn (cond [(< 0 (- (posn-x (paddle-posn (ws-p1 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p1 ws))) 5)]
                                              [else (posn-x (paddle-posn (ws-p1 ws)))])
                                        (posn-y (paddle-posn (ws-p1 ws))))
                             (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))])]
        [else (ws-p1 ws)]))

                

(define (update-p2 ws)
  (cond [(paddle-moving (ws-p2 ws))
         (cond [(= 0 (paddle-direction (ws-p2 ws)))
                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p2 ws))))]
                                              [else (posn-y (paddle-posn (ws-p2 ws)))]))
                             (paddle-moving (ws-p2 ws))
                             (paddle-direction (ws-p2 ws)))]
               [(= 1 (paddle-direction (ws-p2 ws)))
                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                               (+ 5 (posn-x (paddle-posn (ws-p2 ws))))]
                                              [else (posn-x (paddle-posn (ws-p2 ws)))])
                                        (posn-y (paddle-posn (ws-p2 ws))))
                             (paddle-moving (ws-p2 ws))
                             (paddle-direction (ws-p2 ws)))]
               [(= 2 (paddle-direction (ws-p2 ws)))
                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 5)]
                                              [else (posn-y (paddle-posn (ws-p2 ws)))]))
                             (paddle-moving (ws-p2 ws))
                             (paddle-direction (ws-p2 ws)))]
               [(= 3 (paddle-direction (ws-p2 ws)))
                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 5)]
                                              [else (posn-x (paddle-posn (ws-p2 ws)))])
                                        (posn-y (paddle-posn (ws-p2 ws))))
                             (paddle-moving (ws-p2 ws))
                             (paddle-direction (ws-p2 ws)))])]
        [else (ws-p2 ws)]))


(define (ballDraw b)
  (make-posn (posn-x (ball-posn b)) (posn-y (ball-posn b))))
(define (gpDraw b)
  (cond [(empty? b) '()]
        [else (cons (circle 12 "solid" "red") (gpDraw (rest b)))]))
(define (render ws)
  (place-images (append (list
                              (rectangle paddle-width paddle-height "solid" "black")
                              (rectangle paddle-width paddle-height "solid" "black"))
                             (gpDraw (ws-ball ws)))
                (append (list (make-posn (posn-x (paddle-posn (ws-p1 ws))) (- 0 (posn-y (paddle-posn (ws-p1 ws)))))
                      (make-posn (posn-x (paddle-posn (ws-p2 ws))) (- 0 (posn-y (paddle-posn (ws-p2 ws))))))
                      (ballsDraw ws))
                      
                (rectangle screen-width screen-height "outline" "white")))

(big-bang initial-state
          [to-draw render]
          [on-key key-down-handler]
          [on-release key-up-handler]
          [on-tick game-tick])
