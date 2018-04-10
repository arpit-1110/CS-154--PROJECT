#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;define constants here (e puck-radius stiker-radius left right)

(define e 0.8)
(define puck-radius 25)
(define striker-radius 30)
(define left 300)
(define right 700)



(define state-list (list 'x-player 'y-player 'prev-x-player 'prev-y-player 'x-puck 'y-puck
                         'vel-x-puck 'vel-y-puck 'x-bot 'y-bot 'vel-x-bot 'vel-y-bot))

(define l (map (λ (x) (cons x 0)) state-list))

(define state (make-hash l))
(hash-set! state 'x-player 500)
(hash-set! state 'y-player 620)
(hash-set! state 'x-puck 500)
(hash-set! state 'y-puck 350)
(hash-set! state 'x-bot 500)
(hash-set! state 'y-bot 80)
 

(define (collision x-striker y-striker x-puck y-puck vel-x-striker vel-y-striker vel-x-puck vel-y-puck )
  (let* ([angle (atan (/ (- y-puck y-striker) (- x-puck x-striker)))]
         [sine (sin angle)]
         [cosine (cos angle)]
         [vlpuck (+ (* vel-x-puck cosine) (* vel-y-puck sine))]
         [vlstriker (+ (* vel-x-striker cosine) (* vel-y-striker sine))]
         [vflpuck (+ (* e (- vlstriker vlpuck)) vlstriker)]
         [vxpf (- (+ (* sine sine vel-x-puck) (* cosine vflpuck)) (* vel-y-puck sine cosine))]
         [vypf (- (+ (* vel-y-puck cosine cosine) (* sine vflpuck)) (* vel-x-puck sine cosine))])
  (list vxpf vypf)))


(define (mouse2x x)
  (cond [(<= x 100) 300]
        [(and (> x 100) (< x 900)) (+ 300 (/ (- x 100) 2))]
        [(>= x 900) 700]))

(define (mouse2y y)
  (cond [(<= y 50) 350]
        [(and (> y 50) (< y 650)) (+ 350 (/ (- y 50) 2))]
        [(>= y 650) 650]))


(define (collide? xs ys xp yp d)
  (let* ([ydif (- yp ys)]
         [xdif (- xp xs)]
         [d1 (sqrt (+ (* ydif ydif) (* xdif xdif)))])
    (if (< d1 d)  #t #f)))




(define (addz st)
  (set! st (+ st 10))
  (display st))

(define (mousehandler st x y me)
   (begin ;(hash-set! st 'prev-x-player (hash-ref st 'x-player))
          ;(hash-set! st 'prev-x-player (hash-ref st 'x-player))
          (hash-set! st 'x-player (mouse2x x))
          (hash-set! st 'y-player (mouse2y y))
   st))

(define (update st)
 
  (let* ((x-player (hash-ref st 'x-player))
         (y-player (hash-ref st 'y-player))
         (x-bot (hash-ref st 'x-bot))
         (y-bot (hash-ref st 'y-bot))
         (x-puck (hash-ref st 'x-puck))
         (y-puck (hash-ref st 'y-puck))
         (vel-x-puck (hash-ref st 'vel-x-puck))
         (vel-y-puck (hash-ref st 'vel-y-puck))
         (vel-x-bot (hash-ref st 'vel-x-bot))
         (vel-y-bot (hash-ref st 'vel-y-bot))
         (vel-x-player (* (- (hash-ref st 'x-player) (hash-ref st 'prev-x-player)) 28))
         (vel-y-player (* (- (hash-ref st 'y-player) (hash-ref st 'prev-y-player)) 28)))
    
 (begin (hash-set! st 'prev-x-player (hash-ref st 'x-player))
        (hash-set! st 'prev-y-player (hash-ref st 'y-player))
        (hash-set! st 'x-puck (+ x-puck (/ vel-x-puck 28)))
        (hash-set! st 'y-puck (+ y-puck (/ vel-y-puck 28)))
         
        
        (cond [(collide? x-player y-player x-puck y-puck (+ puck-radius striker-radius))
             (let* ((col-list (collision x-player y-player x-puck y-puck vel-x-player vel-y-player vel-x-puck vel-y-puck )))
               (begin (hash-set! st 'vel-x-puck (car col-list))
                       (hash-set! st 'vel-y-puck (cadr col-list))))]
              [(collide? x-bot y-bot x-puck y-puck (+ puck-radius striker-radius))
               (let* ((col-list (collision x-bot y-bot x-puck y-puck vel-x-bot vel-y-bot vel-x-puck vel-y-puck )))
                (begin (hash-set! st 'vel-x-puck (car col-list))
                       (hash-set! st 'vel-y-puck (cadr col-list)))) ]
              [(or (<= x-puck (+ left puck-radius)) (>= x-puck (- right puck-radius)))
                   (hash-set! st 'vel-x-puck (* -1 (hash-ref st 'vel-x-puck)))]
              [(or (<= y-puck (+ 50 puck-radius)) (>= y-puck (- 650 puck-radius)))
                   (hash-set! st 'vel-y-puck (* -1 (hash-ref st 'vel-y-puck)))]))
    st))



(define (my-board st)
  (let* ((x-player (hash-ref st 'x-player))
         (y-player (hash-ref st 'y-player))
         (x-bot (hash-ref st 'x-bot))
         (y-bot (hash-ref st 'y-bot))
         (x-puck (hash-ref st 'x-puck))
         (y-puck (hash-ref st 'y-puck)))
               (place-image (rectangle 1000 700 "outline" "black") 500 350
               (place-image (rectangle 400 600 "outline" "red" ) 500 350              
               (place-image (circle 30 "solid" "blue") x-player y-player
               (place-image (circle 30 "solid" "indigo") x-bot y-bot
               (place-image (circle 25 "solid" "indigo") x-puck y-puck             
               (place-image (rectangle 200 20 "solid" "yellow" ) 500 60
               (place-image (rectangle 200 20 "solid" "yellow" ) 500 640
               (overlay (circle 25 "solid" (color 211 211 211 200))
                        (overlay (circle 50 "outline" "blue" ) (add-line (rectangle 1000 700 "outline" "white") 300 350 700 350 "marron"))))))))))))

              
        






(big-bang state
          [on-mouse mousehandler]
          [on-tick update 1/1000]
          [to-draw my-board])



