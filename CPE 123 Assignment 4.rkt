;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |CPE 123 Assignment 4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)




 ;a paddle is a struct
; (make-paddle (posn boolean number))
(define-struct paddle [posn moving direction])
; a ball is a struct
; (make-ball (posn number number number number))
(define-struct ball [posn deltax deltay angle counter])
; p1 and p2 are inststances of paddle
; ball is an instance of ball
(define-struct ws [p1 p2 ball])



; width of scene excluding the borders
(define walls-widthr (- 900 26))
; height of scene excluding the borders
(define walls-lengthr (- 500 26))
; width of paddle
(define paddle-width 10)
; height of paddle
(define paddle-height 60)
; draws ball
(define gp (circle 12 "solid" "red"))
;the  basic acceleration of the ball in px/tick^2
(define baccel 3)


; width of scene
(define screen-width 900)
; height of scene
(define screen-height 500)
; initial value of deltax of ball
(define init-dx 10)
; initial value of deltay of ball
(define init-dy 10)


;Dominique made this and it is super cool
(define paddleIMG
  (rectangle paddle-width paddle-height "solid" "white"))
(define ballIMG
  (circle 12 "solid" "red"))

; power-ups
; arpeggio: creates 3 more balls on the field, returns to 1 ball on goal
 ; he made this too

; ballspeedup: increases ball travel velocity, increases pitch of collisions
(define ballvelocityup
  (circle 23 "solid" "blue"))

; paddleslow: lowers opponent's paddle movement speed
 ; he is very skilled at photoshop




; initial state of the world
(define initial-state
  (make-ws (make-paddle (make-posn (/ screen-width 4) (-(/ screen-height 2))) #false 0)
           (make-paddle (make-posn (/ screen-width (/ 4 3)) (-(/ screen-height 2))) #false 0)
           (cons (make-ball (make-posn 800 400) init-dx init-dy 0 1) '())))
;450 250
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



;ballsMove runs ballMove for all of the balls in list-of-balls
;ws-> list-of-balls
(define (ballsMove ws)
  (map ballMove (ws-ball ws)))

;ballsBounce runs ballbounce for all of the ball in list-of-balls
;ws-> list-of-balls
(define (ballsBounce ws)
  (map ballBounce (ws-ball ws)))

;ballsDraw runs ballDraw for all of the balls in the list-of-balls
;ws->list-of-balls
(define (ballsDraw ws)
  (map ballDraw (ws-ball ws)))


; computes the position of the ball when it collides with the walls
; ws -> ws
(define (ballBounce b)
  (cond [(or  (and  (>= (posn-x (ball-posn b)) walls-widthr)
                    (<= (posn-y (ball-posn b)) 26))
              (and (>= (posn-x (ball-posn b)) walls-widthr)
                   (>= (posn-y (ball-posn b)) walls-lengthr))
              (and (<= (posn-x (ball-posn b)) 26)
                   (>= (posn-y (ball-posn b)) walls-lengthr))
              (and (<= (posn-x (ball-posn b)) 26)
                   (<= (posn-y (ball-posn b)) 26)))
         (make-ball (ball-posn  b) (* -1 (+ baccel (ball-deltax b))) (* -1 (+ baccel(ball-deltay  b))) (ball-angle  b) (+ 1 (ball-counter  b)))]
        
        [(>= (posn-x (ball-posn  b)) walls-widthr)
         (make-ball (make-posn walls-widthr (posn-y (ball-posn b))) (* -1 (+ baccel (ball-deltax b))) (ball-deltay  b) (ball-angle  b) (+ 1 (ball-counter  b)))] ;change
        
        [(and (<= (posn-x (ball-posn  b)) 26) (< 0 (ball-deltay b)))
         (make-ball (make-posn 26 (posn-y (ball-posn b))) (+ (* -1 (ball-deltax b)) baccel) (ball-deltay b) (ball-angle b) (+ 1 (ball-counter b)))]

        [(and (<= (posn-x (ball-posn b)) 26) (> 0 (ball-deltay b)))
         (make-ball (make-posn 26 (posn-y (ball-posn b))) (+ (* -1 (ball-deltax b)) baccel) (ball-deltay b) (ball-angle b) (+ 1 (ball-counter b)))]

        [(>= (posn-y (ball-posn  b)) walls-lengthr)
         (make-ball (make-posn (posn-x (ball-posn b)) walls-lengthr) (ball-deltax b) (* -1 (+ baccel (ball-deltay  b))) (ball-angle  b) (+ 1 (ball-counter  b)))]
        
        [(<= (posn-y (ball-posn  b)) 26)
         (make-ball (make-posn (posn-x (ball-posn b)) 26) (ball-deltax b) (- (* -1 (ball-deltay  b)) baccel) (ball-angle  b) (+ 1 (ball-counter  b)))]
        
        [else (make-ball (ball-posn  b) (ball-deltax   b) (ball-deltay  b) (ball-angle  b) (+ 1 (ball-counter  b)))]
        ))


;ballMove changes the ws so render can move the ball
;it takes the current position of the ball
;and adds the current deltax of the ball
;the sum is the new position
;ball->ball
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

;update-ball needs to do both move the ball and check if the ball needs to bounce,
;if it needs to bounce, make the ball bounce
;to do this we created another part of the structure ball named 'counter'
;the counter is a number and so the ball will move when the counter is odd (then add one to the counter)
; and bounce the ball against a wall (and add one to the counter) if the ball is touching a wall
;ws-> ball
(define (update-ball ws)
  (cond
    [(odd? (ball-counter   (first (ws-ball ws)))) (ballsMove  ws)]
    [(even? (ball-counter  (first (ws-ball ws)))) (ballsBounce ws)]))
;game-tick combines three functions into one so our game abides by the big-bang structure.
;ws -> ws
(define (game-tick ws)
  (make-ws (update-p1 ws) (update-p2 ws) (update-ball ws)))
;update-p1, changes the position of the left player's paddle
;ws-> p1
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

                
;update-p2 changes the posn of the right player's paddle
;ws-> p2
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



;ballDraw updates the x and y position for a single ball in a list
;list-of-balls -> posn
(define (ballDraw b)
  (make-posn (posn-x (ball-posn b)) (posn-y (ball-posn b))))
;gpDraw draws the amount of balls on the screen that are the current game calls for
;list-of-balls -> img 
(define (gpDraw b)
  (cond [(empty? b) '()]
        [else (cons ballIMG (gpDraw (rest b)))]))
;render draws the entire game
;ws -> ws
(define (render ws)
  (place-images (append (list
                              paddleIMG
                              paddleIMG)
                             (gpDraw (ws-ball ws)))
                (append (list
                            (make-posn (posn-x (paddle-posn (ws-p1 ws)))
                                       (- 0 (posn-y (paddle-posn (ws-p1 ws)))))
                            (make-posn
                                       (posn-x (paddle-posn (ws-p2 ws)))
                                       (- 0 (posn-y (paddle-posn (ws-p2 ws))))))
                            (ballsDraw ws))
                      
                background))

(big-bang initial-state
          [to-draw render]
          [on-key key-down-handler]
          [on-release key-up-handler]
          [on-tick game-tick])


;ball paddle1 paddle2 -> ball
(define NUM1 (sqrt 3))

;DONt USE THIS YET IT ISNT DONE

(define (paddle-bounce b p1 p2)
  (cond
    [(and (= (posn-x (ball-posn b)) (posn-x (paddle-posn p1))) (= (posn-y (ball-posn b)) (posn-y (paddle-posn p1))))
      (make-ball (make-posn (posn-x (ball-posn b)) (posn-y (ball-posn b))) (* -1 (ball-deltax b)) (ball-deltay b)(ball-angle b) (+ 1 (ball-counter b)))]

    [(and (= (posn-x (ball-posn b)) (posn-x (paddle-posn p1))) (and (< (posn-y (ball-posn b) (posn-y (paddle-posn p1)))) (>= (posn-y (ball-posn b)) (+ (* 0.25 (posn-y (paddle-posn p1))) (posn-y (paddle-posn p1))))))
      (make-ball (make-posn (posn-x (ball-posn b)) (posn-y (ball-posn b))) (* (* -1 NUM1) (ball-deltax b)) (abs (ball-deltay b)) (ball-angle b) (+ 1 (ball-counter b)))]

    [(and (= (posn-x (ball-posn b)) (posn-x (paddle-posn p1))) (and (< (posn-y (ball-posn b) ) (+ (* 0.25 (posn-y (paddle-posn p1))) (posn-y (paddle-posn p1)))) (>= (posn-y (ball-posn b)) (+ (/ paddle-heigth 2) (posn-y (paddle-posn p1))))))
      (make-ball (make-posn (posn-x (ball-posn b)) (posn-y (ball-posn b))) (* -1 (ball-deltax b)) (* NUM1 (abs (ball-deltay b))) (ball-angle b) (+ 1 (ball-counter b)))]
    [(and (= (posn-x (ball-posn b)) (posn-x (paddle-posn p1))) (and (< (posn-y (ball-posn b) (posn-y (paddle-posn p1)))) (>= (posn-y (ball-posn b)) (+ (* 0.25 (posn-y (paddle-posn p1))) (posn-y (paddle-posn p1))))))]
    [(and (= (posn-x (ball-posn b)) (posn-x (paddle-posn p1))))]
    [(and (= (posn-x (ball-posn b)) (posn-x (paddle-posn p1))))]
    [(and (= (posn-x (ball-posn b)) (posn-x (paddle-posn p1))))]))
  