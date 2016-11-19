;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; a paddle is a struct
; (make-paddle (posn boolean number))
(define-struct paddle [posn moving direction])
; a ball is a struct
; (make-ball (posn number number number number))
(define-struct ball [posn deltax deltay angle counter])
; p1 and p2 are inststances of paddle
; ball is an instance of ball
(define-struct ws [p1 p2 ball])



; width of scene excluding the borders
(define walls-widthr (- 900 16))
; height of scene excluding the borders
(define walls-lengthr (- 500 16))
; width of paddle
(define paddle-width 10)
; height of paddle
(define paddle-height 60)
; draws ball
(define gp (circle 12 "solid" "red"))


; width of scene
(define screen-width 900)
; height of scene
(define screen-height 500)
; initial value of deltax of ball
(define init-dx 10)
; initial value of deltay of ball
(define init-dy 10)


; initial state of the world
(define initial-state
  (make-ws (make-paddle (make-posn (/ screen-width 4) (-(/ screen-height 2))) #false 0)
           (make-paddle (make-posn (/ screen-width (/ 4 3)) (-(/ screen-height 2))) #false 0)
           (cons (make-ball (make-posn 450 250) init-dx init-dy 0 1) '())))

; a ke is one of "w", "a", "s", "d", "up", "down", "left", "right"

; moves the paddle in the horizontal and vertical direction when the specific key is pressed
; ws ke -> ws
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
; stops moving the paddle when a key is stopped being pressed
; ws ke -> ws
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



; computes the position of the ball when it collides with the walls
; ws -> ws
(define (ballBounce b)
  (cond [(or  (and  (>= (posn-x (ball-posn b)) walls-widthr)
                    (<= (posn-y (ball-posn   b)) 0))
              (and (>= (posn-x (ball-posn b)) walls-widthr)
                   (>= (posn-y (ball-posn   b)) walls-lengthr))
              (and (<= (posn-x (ball-posn b)) 0)
                   (>= (posn-y (ball-posn   b)) walls-lengthr))
              (and (<= (posn-x (ball-posn  b)) 0)
                   (<= (posn-y (ball-posn  b)) 0)))
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

; updates the world state whenever the ball moves or bounces against the wall
; ws -> ws
(define (update-ball ws)
  (cond
    [(odd? (ball-counter   (first (ws-ball ws)))) (ballsMove  ws)]
    [(even? (ball-counter  (first (ws-ball ws)))) (ballsBounce ws)]))

; updates movement of p1
; ws -> ws
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

                
; updates movement of p2
; ws -> ws
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
; updates ball movement and the movement of paddles p1 and p2
; ws -> ws
(define (game-tick ws)
  (make-ws (update-p1 ws) (update-p2 ws) (update-ball ws)))


(define (ballDraw b)
  (make-posn (posn-x (ball-posn b)) (posn-y (ball-posn b))))
(define (gpDraw b)
  (cond [(empty? b) '()]
        [else (cons (circle 12 "solid" "red") (gpDraw (rest b)))]))

; places paddles p1, p2 and ball onto the empty scene
(define (render ws)
  (place-images (append (list
                              (rectangle paddle-width paddle-height "solid" "black")
                              (rectangle paddle-width paddle-height "solid" "black"))
                             (gpDraw (ws-ball ws)))
                (append (list (make-posn (posn-x (paddle-posn (ws-p1 ws))) (- 0 (posn-y (paddle-posn (ws-p1 ws)))))
                      (make-posn (posn-x (paddle-posn (ws-p2 ws))) (- 0 (posn-y (paddle-posn (ws-p2 ws))))))
                      (ballsDraw ws))
                      
                (rectangle screen-width screen-height "outline" "white")))
; starts the world from the initial state
(big-bang initial-state
          [to-draw render]
          [on-key key-down-handler]
          [on-release key-up-handler]
          [on-tick game-tick])
