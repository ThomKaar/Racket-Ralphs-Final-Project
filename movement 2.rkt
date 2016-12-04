;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |movement 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define-struct paddle [posn movingVer directionVer movingHor directionHor])
;; a paddle is a
;; (make-paddle posn bool number bool number)
;; where the both numbers are 1 or -1,
;; representing either the positive
;; direction (up, right) or the negative
;; (down, left).

(define-struct ws [p1 p2])
;; a ws is a
;; (make-ws paddle paddle)

(define paddle-width 10)
(define paddle-height 60)
(define screen-width 900)
(define screen-height 500)
(define wall-offset 10)
(define center-offset 6)

(define initial-state
  (make-ws (make-paddle (make-posn (/ screen-width 4) (-(/ screen-height 2))) #false 0 #false 0) (make-paddle (make-posn (/ screen-width (/ 4 3)) (-(/ screen-height 2))) #false 0 #false 0)))

(define (key-down-handler ws ke)
  (cond [(key=? ke "w")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 1 (paddle-movingHor (ws-p1 ws)) (paddle-directionHor (ws-p1 ws))) (ws-p2 ws))]
        [(key=? ke "d")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) (paddle-movingVer (ws-p1 ws)) (paddle-directionVer (ws-p1 ws)) #true 1) (ws-p2 ws))]
        [(key=? ke "s")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true -1 (paddle-movingHor (ws-p1 ws)) (paddle-directionHor (ws-p1 ws))) (ws-p2 ws))]
        [(key=? ke "a")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) (paddle-movingVer (ws-p1 ws)) (paddle-directionVer (ws-p1 ws)) #true -1) (ws-p2 ws))]
        [(key=? ke "up")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 1 (paddle-movingHor (ws-p2 ws)) (paddle-directionHor (ws-p2 ws))))]
        [(key=? ke "right")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) (paddle-movingVer (ws-p2 ws)) (paddle-directionVer (ws-p2 ws)) #true 1))]
        [(key=? ke "down")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true -1 (paddle-movingHor (ws-p2 ws)) (paddle-directionHor (ws-p2 ws))))]
        [(key=? ke "left")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) (paddle-movingVer (ws-p2 ws)) (paddle-directionVer (ws-p2 ws)) #true -1))]
        [else ws]))

(define (key-up-handler ws ke)
  (cond [(key=? ke "w")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false (paddle-directionVer (ws-p1 ws)) (paddle-movingHor (ws-p1 ws)) (paddle-directionHor (ws-p1 ws))) (ws-p2 ws))]
        [(key=? ke "d")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) (paddle-movingVer (ws-p1 ws)) (paddle-directionVer (ws-p1 ws)) #false (paddle-directionHor (ws-p1 ws))) (ws-p2 ws))]
        [(key=? ke "s")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false (paddle-directionVer (ws-p1 ws)) (paddle-movingHor (ws-p1 ws)) (paddle-directionHor (ws-p1 ws))) (ws-p2 ws))]
        [(key=? ke "a")
         (make-ws (make-paddle (paddle-posn (ws-p1 ws)) (paddle-movingVer (ws-p1 ws)) (paddle-directionVer (ws-p1 ws)) #false (paddle-directionHor (ws-p1 ws))) (ws-p2 ws))]
        [(key=? ke "up")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false (paddle-directionVer (ws-p2 ws)) (paddle-movingHor (ws-p2 ws)) (paddle-directionHor (ws-p2 ws))))]
        [(key=? ke "right")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) (paddle-movingVer (ws-p2 ws)) (paddle-directionVer (ws-p2 ws)) #false (paddle-directionHor (ws-p2 ws))))]
        [(key=? ke "down")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false (paddle-directionVer (ws-p2 ws)) (paddle-movingHor (ws-p2 ws)) (paddle-directionHor (ws-p2 ws))))]
        [(key=? ke "left")
         (make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) (paddle-movingVer (ws-p2 ws)) (paddle-directionVer (ws-p2 ws)) #false (paddle-directionHor (ws-p2 ws))))]
        [else ws]))

(define (calcYp1 p)
  (cond [(and (paddle-movingVer p) (= (paddle-directionVer p) 1))
         (cond [(< (+ 0 wall-offset) (abs (+ (posn-y (paddle-posn p)) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn p)))]
               [else (posn-y (paddle-posn p))])]
        [(and (paddle-movingVer p) (= (paddle-directionVer p) -1))
         (cond [(< (+ (abs (posn-y (paddle-posn p))) (/ paddle-height 2)) (- screen-height wall-offset)) (- (posn-y (paddle-posn p)) 5)]
               [else (posn-y (paddle-posn p))])]
        [else (posn-y (paddle-posn p))]
        ))

(define (calcXp1 p)
  (cond [(and (paddle-movingHor p) (= (paddle-directionHor p) 1))
         (cond [(< (+ (posn-x (paddle-posn p)) (/ paddle-width 2)) (- (/ screen-width 2) center-offset)) (+ 5 (posn-x (paddle-posn p)))]
               [else (posn-x (paddle-posn p))])]
        [(and (paddle-movingHor p) (= (paddle-directionHor p) -1))
         (cond [(< (+ 0 wall-offset) (- (posn-x (paddle-posn p)) (/ paddle-width 2))) (- (posn-x (paddle-posn p)) 5)]
               [else (posn-x (paddle-posn p))])]
        [else (posn-x (paddle-posn p))]
        ))

(define (update-p1 ws)
  (make-paddle (make-posn (calcXp1 (ws-p1 ws)) (calcYp1 (ws-p1 ws))) (paddle-movingVer (ws-p1 ws)) (paddle-directionVer (ws-p1 ws)) (paddle-movingHor (ws-p1 ws)) (paddle-directionHor (ws-p1 ws))))

(define (calcYp2 p)
  (cond [(and (paddle-movingVer p) (= (paddle-directionVer p) 1))
         (cond [(< (+ wall-offset 0) (abs (+ (posn-y (paddle-posn p)) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn p)))]
               [else (posn-y (paddle-posn p))])]
        [(and (paddle-movingVer p) (= (paddle-directionVer p) -1))
         (cond [(< (+ (abs (posn-y (paddle-posn p))) (/ paddle-height 2)) (- screen-height wall-offset)) (- (posn-y (paddle-posn p)) 5)]
               [else (posn-y (paddle-posn p))])]
        [else (posn-y (paddle-posn p))]
        ))

(define (calcXp2 p)
  (cond [(and (paddle-movingHor p) (= (paddle-directionHor p) 1))
         (cond [(< (+ (posn-x (paddle-posn p)) (/ paddle-width 2)) (- screen-width wall-offset)) (+ 5 (posn-x (paddle-posn p)))]
               [else (posn-x (paddle-posn p))])]
        [(and (paddle-movingHor p) (= (paddle-directionHor p) -1))
         (cond [(< (+ (/ screen-width 2) center-offset) (- (posn-x (paddle-posn p)) (/ paddle-width 2))) (- (posn-x (paddle-posn p)) 5)]
               [else (posn-x (paddle-posn p))])]
        [else (posn-x (paddle-posn p))]
        ))

(define (update-p2 ws)
  (make-paddle (make-posn (calcXp2 (ws-p2 ws)) (calcYp2 (ws-p2 ws))) (paddle-movingVer (ws-p2 ws)) (paddle-directionVer (ws-p2 ws)) (paddle-movingHor (ws-p2 ws)) (paddle-directionHor (ws-p2 ws))))

(define (game-tick ws)
  (make-ws (update-p1 ws) (update-p2 ws)))

(define (render ws)
  (place-images (list (rectangle paddle-width paddle-height "solid" "black")
                      (rectangle paddle-width paddle-height "solid" "black"))
                (list (make-posn (posn-x (paddle-posn (ws-p1 ws))) (- 0 (posn-y (paddle-posn (ws-p1 ws)))))
                       (make-posn (posn-x (paddle-posn (ws-p2 ws))) (- 0 (posn-y (paddle-posn (ws-p2 ws))))))
                (rectangle screen-width screen-height "outline" "white")))

(big-bang initial-state
          [to-draw render]
          [on-key key-down-handler]
          [on-release key-up-handler]
          [on-tick game-tick])