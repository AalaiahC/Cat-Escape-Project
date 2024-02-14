 

;CONSTANTS
(define WIDTH 300)
(define HEIGHT 500)
(define bigrat .)
(define ROCKET-IMG (scale 1/5 bigrat))
(define E-SCENE (empty-scene WIDTH HEIGHT 'burlywood))
(define bigalien .)
(define ALIEN-IMG (scale 1/4 bigalien))
(define ROCKET-DELTA-X 5)
(define ROCKET-DELTA-Y 5)
(define-struct alien (posn dir speed))

(define ALIEN1 (make-alien (make-posn (/ WIDTH 2) (/ (image-height ALIEN-IMG) 2)) "right" 10))
(define ALIEN2 (make-alien (make-posn 200 100) "left" 15))
(define ALIEN3 (make-alien (make-posn 200 200) "right" 9))

(define INIT-LOA (list ALIEN1 ALIEN2 ALIEN3))

; a rocket is a posn
(define INIT-ROCKET (make-posn 250 (- HEIGHT (/ (image-height ROCKET-IMG)2))))

(define-struct world (rocket aliens))

(define INIT-WORLD (make-world INIT-ROCKET INIT-LOA))

(define RIGHT-EDGE (- (sub1 WIDTH) (/ (image-width ALIEN-IMG) 2)))
(define LEFT-EDGE (/ (image-width ALIEN-IMG) 2))

;draw-world: world-->scene
;Purpose:To draw the world
(define (draw-world w)
  (local [;draw-rocket: rocket scene-->scene
          ;Purpose:To draw the given rocket in the given scene
          (define (draw-rocket a-rocket scn)
            (place-image ROCKET-IMG (posn-x a-rocket)(posn-y a-rocket) scn))
          ;draw-aliens: loa scene-->scene
          ;Purpose:To draw the aliens on the scene
          (define (draw-aliens a-loa scn)
            (local [;draw-alien: alien scene-->scene
                    ;Purpose:To draw the given alien in the given scene
                    (define (draw-alien an-alien scn)
                      ;INVENTORY
                      ;(posn-x an-alien) a number for the x-coordinate of an-alien
                      ;(posn-y an-alien) a number for the y-coordinate of an-alien
                      (place-image ALIEN-IMG (posn-x (alien-posn an-alien)) (posn-y (alien-posn an-alien)) scn))] 
              ;INVENTORY
              ;(first a-loa) is the first alien in a-loa
              ;(rest a-loa) is a-loa without its first alien
              ;(empty? a-loa) returns true if a-loa is empty
              ;(f-on-loa (rest a-loa)) recursivley processes a-loa
              (cond 
                [(empty? a-loa) scn]
                [else (draw-alien (first a-loa) (draw-aliens (rest a-loa) scn))])))
          ]

    (draw-rocket (world-rocket w) (draw-aliens (world-aliens w) E-SCENE))
    ))
;alien-hit?: alien-shot --> boolean
;Purpose: To determine if the shot has hit the given alien 
(define (alien-hit? a-rocket an-alien)
  ;INVENTORY
  ;if a-shot is a posn
  ;(posn-x a-rocket) is a number for the x-coordinate of a rocket
  ;(posn-y a-rocket) is a number for the y-coordinate of a rocket
  (and (<= (abs (- (posn-x a-rocket) (posn-x (alien-posn an-alien))))
           (/ (image-width ALIEN-IMG) 2))
       (<= (abs (- (posn-y a-rocket) (posn-y (alien-posn an-alien))))
           (/ (image-width ALIEN-IMG) 2))))


;move-rocket: rocket string-->rocket
;Purpose:To move the rocket in the given direction
(define (move-rocket a-rocket direction)
  (cond [(and (string=? direction "right")
              (< (+ (posn-x a-rocket) ROCKET-DELTA-X) (- WIDTH 18)))
         (make-posn (+ (posn-x a-rocket) ROCKET-DELTA-X) (posn-y a-rocket))]
        [(and (string=? direction "left")
              (> (- (posn-x a-rocket) ROCKET-DELTA-X) 19))
         (make-posn (- (posn-x a-rocket) ROCKET-DELTA-X) (posn-y a-rocket))]
        [else a-rocket]))

;any-alien-hit?: rocket a-loa --> boolean
;Purpose: To determine if a rocket has hit an alien
(define (any-alien-hit? rocket a-loa)                     
  (ormap (λ (a)(alien-hit? rocket a)) a-loa))

; change-direction: alien -> alien
; takes in an alien and changes its direction if necessary
(define (change-direction a)
  (cond [(and (string=? (alien-dir a) "left")
              (<= (posn-x (alien-posn a)) LEFT-EDGE))
         (make-alien (alien-posn a) "right" (alien-speed a))]
        [(and (string=? (alien-dir a) "right")
              (>= (posn-x (alien-posn a)) RIGHT-EDGE))
         (make-alien (alien-posn a) "left" (alien-speed a))]
        [else a]))

; change-direction
(define (change-directions a-loa)
  (map change-direction a-loa))

(define (move-aliens a-loa)
  (local [;move-alien: alien-->alien
          ;Purpose:To move the alien in the game
          (define (move-alien an-alien)
            ;INVENTORY
            ;an-alien is a posn
            ;(posn-x an-alien) a number for the x-coordinate of an-alien
            ;(posn-y an-alien) a number for the y-coordinate of an-alien
            (cond [(string=? (alien-dir an-alien) "right")
                   (make-alien (make-posn (+ (posn-x (alien-posn an-alien)) (alien-speed an-alien))
                                          (posn-y (alien-posn an-alien)))
                               (alien-dir an-alien)
                               (alien-speed an-alien))]
                  [(string=? (alien-dir an-alien) "left")
                   (make-alien(make-posn (- (posn-x (alien-posn an-alien)) (alien-speed an-alien))
                                         (posn-y (alien-posn an-alien)))
                              (alien-dir an-alien)
                              (alien-speed an-alien))]
                  [else an-alien]))]
    ;INVENTORY
    ;(first a-loa) is the first alien in a-loa
    ;(rest a-loa) is a-loa without its first alien
    ;(empty? a-loa) returns true if a-loa is empty
    ;(f-on-loa (rest a-loa)) recursivley processes a-loa
    (map (λ (a) (move-alien a)) a-loa)))

;create-new-world: world -> world
;Purpose: To update the world 
(define (create-new-world a-world)
  (local [;move-aliens: loa -->loa
          ;Purpose:To move the aliens
          (define (move-aliens a-loa)
  (local [;move-alien: alien-->alien
          ;Purpose:To move the alien in the game
          (define (move-alien an-alien)
            ;INVENTORY
            ;an-alien is a posn
            ;(posn-x an-alien) a number for the x-coordinate of an-alien
            ;(posn-y an-alien) a number for the y-coordinate of an-alien
            (cond [(string=? (alien-dir an-alien) "right")
                   (make-alien (make-posn (+ (posn-x (alien-posn an-alien)) (alien-speed an-alien))
                                          (posn-y (alien-posn an-alien)))
                               (alien-dir an-alien)
                               (alien-speed an-alien))]
                  [(string=? (alien-dir an-alien) "left")
                   (make-alien(make-posn (- (posn-x (alien-posn an-alien)) (alien-speed an-alien))
                              (posn-y (alien-posn an-alien)))
                              (alien-dir an-alien)
                               (alien-speed an-alien))]
                  [else an-alien]))]
    ;INVENTORY
    ;(first a-loa) is the first alien in a-loa
    ;(rest a-loa) is a-loa without its first alien
    ;(empty? a-loa) returns true if a-loa is empty
    ;(f-on-loa (rest a-loa)) recursivley processes a-loa
    (map (λ (a) (move-alien a)) a-loa)))
          ;shmoovin: a-rocket direction -> a-rocket
          ;purpose: to move the rocket
          (define (shmoovin a-rocket a-loa)
            (cond [(any-alien-hit? a-rocket a-loa) INIT-ROCKET]
                  [(<= (posn-y a-rocket)(/(image-height ROCKET-IMG) 2))
                   (make-posn (posn-x a-rocket) (/(image-height ROCKET-IMG) 2))]
                  [else (make-posn (posn-x a-rocket)
                                   (- (posn-y a-rocket) ROCKET-DELTA-Y))]))]  
    ;INVENTORY
    ;(world-rocket a-world) is a rocket
    ;(world-aliens a-world) is a loa
    ;(world-dir a-world) a string for the direction of the alien 
    ;(world-los a-world) is a posn for the shot
    (make-world
     (shmoovin (world-rocket a-world) (world-aliens a-world))
   (move-aliens (change-directions (world-aliens a-world))))))

;process-key: world KeyEvent -> world
;Purpose: To process keyboard input 
(define (process-key a-world k)
(make-world (move-rocket (world-rocket a-world) k)
                        (world-aliens a-world)))

;game-over?: world -> boolean
;Purpose: To determine if the game is over
(define (game-over? a-world)
  (local [;rocket-reached-end?: a-rocket -> boolean
          ;purpose: to determine if the rocket has reached the top of the scene
          (define (rocket-escaped? a-rocket)
            (<= (posn-y a-rocket)(/(image-height ROCKET-IMG) 2)))]
    (rocket-escaped? (world-rocket a-world))))

;compute-last-scene: world -> scene
;Purpose:To display ending image 
(define (compute-last-scene a-world)
  ;INVENTORY
  ;(world-rocket a-world) is a rocket
  ;(world-aliena a-world) is a loa
  ;(world-dir a-world) a string for the direction of the alien 
  ;(world-los a-world) is either false or a posn for the shot
  (cond [(game-over? a-world)
         (place-image (text "CHEESE AQUIRED" 36 "green") (/ WIDTH 2) (/ HEIGHT 2) (draw-world a-world))]
        [else
         (place-image (text "CHEESE LOST" 36 "red") (/ WIDTH 2) (/ HEIGHT 2) (draw-world a-world))]))


(check-expect (game-over? B-WORLD) true)

(define B-WORLD (make-world (make-posn 250 0) INIT-LOA))
     
(big-bang
    INIT-WORLD
  
  (on-tick create-new-world)
  (on-draw draw-world)
  (on-key process-key)
  (stop-when game-over? compute-last-scene))
