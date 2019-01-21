;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  
(define INVADER-Y-SPEED 1.5)
(define INVADER-ACCELERATION 1)
(define TANK-SPEED 2) 
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define GAME-OVER (text "GAME OVER" 20 "black"))
                               
(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "black")              
              -5 6
              (overlay (star 6 "solid" "red")
                       (ellipse 20 10 "solid" "black"))))            

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "blue")       
                       (ellipse 35 10 "solid" "black"))     
              8 -12
              (above (rectangle 5 6 "solid" "black")       
                     (rectangle 20 7 "solid" "blue"))))   

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (overlay (circle 2 "solid" "black")
                         (ellipse 5 15 "solid" "red")))


;; Data Definitions:

(define-struct game (invaders missiles t))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   
(define T1 (make-tank 50 1))            
(define T2 (make-tank 50 -1))           

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           
(define I2 (make-invader 150 HEIGHT -10))       
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) 

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ListOfInvader is one of:
;; - empty
;; (cons Invader ListOfInvader)
;; interp. an arbitrary number of invaders

(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (list I1 I2 I3))
#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. an arbitrary number of missiles

(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (list M1 M2 M3))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


;; =================
;; Functions:

;; Game -> Game
;; start the game with (main G0)
;; <No test for main function>

(define (main g)
  (big-bang g                            
            (on-tick   next-game)        
            (to-draw   render-game)                      
            (stop-when game-over? game-over) 
            (on-key    handle-key)))     

;; Game -> Game
;; produce the next game
(check-random (next-game (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (make-game (next-invader empty empty)
                         empty
                         (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-random (next-game (make-game (list (make-invader 150 100 12))
                                    (list (make-missile 150 300))
                                    (make-tank 50 1)))
              (make-game (next-invader (list (make-invader 150 100 12))
                                       (list (make-missile 150 300)))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))  
                         (make-tank (+ 50 TANK-SPEED) 1)))
(check-random (next-game (make-game (list (make-invader 150 100 12) (make-invader 80 250 -6))
                                    (list (make-missile 200 100) (make-missile 150 100))
                                    (make-tank 50 -1)))
              (make-game (next-invader (list (make-invader 150 100 12) (make-invader 80 250 -6))
                                       (list (make-missile 200 100) (make-missile 150 100)))
                         (list (make-missile 200 (- 100 MISSILE-SPEED)))
                         (make-tank (- 50 TANK-SPEED) -1)))
(check-random (next-game G3)
              (make-game (next-invader (list I1 I2)
                                       (list M1 M2))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ 50 TANK-SPEED) 1)))

;(define (next-game g) G0) ; stub
;<Template from Game but corrected>
#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-t g))))

(define (next-game g)
  (make-game (next-invader (game-invaders g)
                           (game-missiles g))
             (next-missile (game-invaders g)
                           (game-missiles g))
             (next-tank (game-t g))))

;; ListOfInvader ListOfMissile-> ListOfInvader
;; Produce the next ListOfInvader after a tick
(check-random (next-invader empty empty) (generate-invader empty))
(check-random (next-invader (list I1 I2) (list M2)) (generate-invader (list (make-invader (+ 150 -10) (+ HEIGHT 10) -10))))
(check-random (next-invader (list I1) (list M1 M2)) (generate-invader empty))
(check-random (next-invader (list I1 I2 I3) (list M1 M2)) (generate-invader (list (make-invader (+ 150 -10)(+ HEIGHT 10) -10)
                                                                                  (make-invader (+ 150 10)(+ (+ HEIGHT 10) 10) 10))))
 
;(define (next-invader loinvader lom) loinvader) ; stub

(define (next-invader loinvader lom)
  (generate-invader (move-invader (remove-invaders loinvader lom))))

;; ListOfInvader -> ListOfInvader
;; Generate new invader ramdomly in time and x position, and add to the list of invaders. 
(check-random (generate-invader empty) (append empty (random-invader INVADE-RATE)))
(check-random (generate-invader (list I1 I2 I3)) (append (list I1 I2 I3) (random-invader INVADE-RATE)))

;(define (generate-invader loinvader) loinvader) ; stub 

(define (generate-invader loinvader)
  (append loinvader (random-invader INVADE-RATE)))

;; Natural -> ListOfInvader
;; Create ramdomly one new invader
(check-random (random-invader INVADE-RATE) (if (< (random 1000) INVADE-RATE)
                                               (cons (make-invader (random WIDTH) 0 (* (random-direction INVADE-RATE) INVADER-X-SPEED)) empty)
                                               empty))

;(define (random-invader n) empty) ; stub 

(define (random-invader n)
  (if (< (random 1000) n)
      (cons (make-invader (random WIDTH) 0 (* (random-direction n) INVADER-X-SPEED)) empty)
      empty))

;; Natural -> Integer
;; Create randomly number 1 or -1
(check-random (random-direction INVADE-RATE) (if (< (random INVADE-RATE) (/ INVADE-RATE 2))
                                                 1
                                                 -1))
;(define (random-direction n) -1) ; stub

(define (random-direction n)
  (if (< (random n) (/ n 2))
      1
      -1))

;; ListOfInvader -> ListOfInvader
;; Move the invader INVADER-SPEED pixels per tick
(check-expect (move-invader empty) empty)
(check-expect (move-invader (list (make-invader 100 150 INVADER-X-SPEED)
                                  (make-invader 80 200 (- INVADER-X-SPEED))))
              (list (make-invader (+ 100 INVADER-X-SPEED) (+ 150 INVADER-X-SPEED) INVADER-X-SPEED)
                    (make-invader (+ 80 (- INVADER-X-SPEED)) (+ 200 INVADER-X-SPEED) (- INVADER-X-SPEED))))
(check-expect (move-invader (list (make-invader WIDTH 0 INVADER-X-SPEED)))
              (list (make-invader WIDTH 0 (- (+ INVADER-X-SPEED INVADER-ACCELERATION)))))
(check-expect (move-invader (list (make-invader 0 0 (- INVADER-X-SPEED))))
              (list (make-invader 0 0 (+ INVADER-X-SPEED INVADER-ACCELERATION))))
(check-expect (move-invader (list (make-invader WIDTH 0 (- INVADER-X-SPEED))))
              (list (make-invader (- WIDTH INVADER-X-SPEED) (+ 0 INVADER-X-SPEED) (- INVADER-X-SPEED))))
(check-expect (move-invader (list (make-invader 0 0 INVADER-X-SPEED)))
              (list (make-invader (+ 0 INVADER-X-SPEED) (+ 0 INVADER-X-SPEED) INVADER-X-SPEED)))
(check-expect (move-invader (list (make-invader 100 140 INVADER-X-SPEED)
                                  (make-invader WIDTH 200 INVADER-X-SPEED)
                                  (make-invader 0 100 (- INVADER-X-SPEED))))
              (list (make-invader (+ 100 INVADER-X-SPEED) (+ 140 INVADER-X-SPEED) INVADER-X-SPEED)
                    (make-invader WIDTH 200 (- (+ INVADER-X-SPEED INVADER-ACCELERATION)))
                    (make-invader 0 100 (+ INVADER-X-SPEED INVADER-ACCELERATION))))

;(define (move-invader loinvader) loinvader) ; stub
;<Template from ListOfInvader>
#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

(define (move-invader loinvader)
  (cond [(empty? loinvader) empty]
        [else
         (if (needs-new-velocity? (first loinvader))
             (cons (make-invader (invader-x (first loinvader))
                                 (invader-y (first loinvader))
                                 (new-velocity (invader-dx (first loinvader))))
                   (move-invader (rest loinvader)))
             (cons (make-invader (+ (invader-x (first loinvader)) (invader-dx (first loinvader)))
                                 (+ (invader-y (first loinvader)) (abs (invader-dx (first loinvader))))
                                 (invader-dx (first loinvader)))
                   (move-invader (rest loinvader))))]))

;; Invader -> Boolean
;; Return true if the invader is close to the edge and its velocity is pointing at that edge
(check-expect (needs-new-velocity? (make-invader 0 100 INVADER-X-SPEED)) false)
(check-expect (needs-new-velocity? (make-invader WIDTH 135 (- INVADER-X-SPEED))) false)
(check-expect (needs-new-velocity? (make-invader 0 122 (- INVADER-X-SPEED))) true)
(check-expect (needs-new-velocity? (make-invader WIDTH 133 INVADER-X-SPEED)) true)
(check-expect (needs-new-velocity? (make-invader (- WIDTH (image-width INVADER)) 290 INVADER-X-SPEED)) true)
(check-expect (needs-new-velocity? (make-invader (+ 0 (image-width INVADER)) 233 (- INVADER-X-SPEED))) true)

;(define (needs-new-velocity? invader) false) ; stub
;<Template from Invader> 
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define (needs-new-velocity? invader)
  (or (and (<= (invader-x invader) (image-width INVADER))
           (negative? (invader-dx invader)))
      (and (>= (invader-x invader) (- WIDTH (image-width INVADER)))
           (positive? (invader-dx invader)))))


;; Number -> Number
;; Change invader velocity in direcction and intensity by INVADER-ACCELERATION
(check-expect (new-velocity 1.5) (- (+ 1.5 INVADER-ACCELERATION)))
(check-expect (new-velocity -3.5) (+ 3.5 INVADER-ACCELERATION))

;(define (new-velocity i) -1.5) ; stub
(define (new-velocity i)
  (if (negative? i)
      (+ (abs i) INVADER-ACCELERATION)
      (- (+ i INVADER-ACCELERATION))))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; Remove invader when its position is <= HIT-RANGE from any missile
(check-expect (remove-invaders empty empty) empty)
(check-expect (remove-invaders empty LOM2) empty)
(check-expect (remove-invaders LOI1 empty) LOI1)
(check-expect (remove-invaders (list (make-invader 100 200 3))
                               (list (make-missile 50 70)))
              (list (make-invader 100 200 3)))
(check-expect (remove-invaders (list (make-invader 100 200 3)
                                     (make-invader 200 200 -3))
                               (list (make-missile 100 200)
                                     (make-missile 150 150)
                                     (make-missile 50 50)))
              (list (make-invader 200 200 -3)))

;(define (remove-invader loinvader lom) loinvader) ; stub
;<Template from ListOfMissile>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (remove-invaders loinvader lom)
  (cond [(empty? lom) loinvader]
        [else
         (remove-invaders (remove-invader loinvader (first lom))
                          (rest lom))]))

;; ListOfInvader Missile -> ListOfInvader
;; Remove invader hit by Missile
(check-expect (remove-invader empty M1) empty)
(check-expect (remove-invader (list (make-invader 100 100 4))
                              (make-missile 40 40))
              (list (make-invader 100 100 4)))
(check-expect (remove-invader LOI2 M2) (list I2 I3))

;(define (remove-invader loinvader missile) loinvader) ; stub
;<Template from ListOfInvader
#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

(define (remove-invader loinvader missile)
  (cond [(empty? loinvader) empty]
        [else
         (if (are-close? (first loinvader) missile)
             (rest loinvader)
             (cons (first loinvader)
                   (remove-invader (rest loinvader) missile)))]))

;; Invader Missile -> Boolean
;; True if the distance between the invader and missile is <= HIT-RANGE
(check-expect (are-close? (make-invader 100 200 3) (make-missile 200 100)) false)
(check-expect (are-close? (make-invader 100 200 3) (make-missile 100 200)) true)
(check-expect (are-close? (make-invader 100 200 3) (make-missile (- 100 HIT-RANGE) (- 200 HIT-RANGE))) true)
(check-expect (are-close? (make-invader 100 200 3) (make-missile (+ 100 HIT-RANGE) (- 200 HIT-RANGE))) true)
(check-expect (are-close? (make-invader 100 200 3) (make-missile (- 100 HIT-RANGE) (+ 200 HIT-RANGE))) true)
(check-expect (are-close? (make-invader 100 200 3) (make-missile (+ 100 HIT-RANGE) (+ 200 HIT-RANGE))) true)

;(define (are-close? invader missile) false) ; stub
;<Template from Invader and Missile>
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define (are-close? invader missile)
 (and (<= (abs (- (invader-x invader) (missile-x missile))) HIT-RANGE)
      (<= (abs (- (invader-y invader) (missile-y missile))) HIT-RANGE)))
  

;; ListOfInvader ListOfMissile -> ListOfMissile
;; Produce the next list of Missile after a tick
(check-expect (next-missile empty empty) empty)
(check-expect (next-missile LOI2 empty) empty)
(check-expect (next-missile (list I1 I2) (list M1 M2))
              (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))))   

;(define (next-missile loinvader lom) lom) ; stub

(define (next-missile loinvader lom)
  (move-missile (remove-missiles loinvader lom)))

;; ListOfMissile -> ListOfMissile
;; Advance all missile MISSILE-SPEED per tick
(check-expect (move-missile empty) empty)
(check-expect (move-missile (list (make-missile 20 200)(make-missile 30 (- MISSILE-SPEED))))
              (list (make-missile 20 (- 200 MISSILE-SPEED))))

;(define (move-missile lom) lom) ; stub
;<Template from ListOfMissile>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (move-missile lom)
  (cond [(empty? lom) empty]
        [else
         (if (negative? (missile-y (first lom)))
             (move-missile (rest lom))
             (cons (make-missile (missile-x (first lom))
                                 (- (missile-y (first lom)) MISSILE-SPEED))
                   (move-missile (rest lom))))]))

;; ListOfInvader ListOfMissile -> ListOfMissile
;; Remove all missile when hit a invader
(check-expect (remove-missiles empty empty) empty)
(check-expect (remove-missiles empty (list M1 M2)) (list M1 M2))
(check-expect (remove-missiles (list I1 I2) empty) empty)
(check-expect (remove-missiles (list I1 I2) (list M1 M2)) (list M1))

;(define (remove-missile loinvader lom) lom) ; stub
;<Template from ListOfInvader>
#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

(define (remove-missiles loinvader lom)
  (cond [(empty? loinvader) lom]
        [else
         (remove-missiles (rest loinvader)
                          (remove-missile lom (first loinvader)))]))

;; ListOfMissile Invader -> ListOfInvader
;; Remove the missile that hit the invader
(check-expect (remove-missile empty I1) empty)
(check-expect (remove-missile LOM2 (make-invader 0 0 3)) LOM2)
(check-expect (remove-missile LOM2 I1) (list M1 M3))

;(define (remove-missile lom invader) lom) ; stub
;<Template from ListOfMissile>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (remove-missile lom invader)
  (cond [(empty? lom) empty]
        [else
         (if (are-close? invader (first lom))
             (rest lom)
             (cons (first lom)
                   (remove-missile (rest lom) invader)))]))

;; Tank -> Tank
;; Produce the next tank 
(check-expect (next-tank (make-tank 50 1)) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (next-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (next-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (next-tank (make-tank 0 -1)) (make-tank 0 -1))
(check-expect (next-tank (make-tank WIDTH -1)) (make-tank (- WIDTH TANK-SPEED) -1))
(check-expect (next-tank (make-tank 0 1)) (make-tank (+ 0 TANK-SPEED) 1))

;(define (next-tank t) t) ; stub
;<Template from Tank>
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))

(define (next-tank t)
  (if (at-edge? t)
      t
      (move-tank t)))

;; Tank -> Boolen
;; True if tank is at any edge and its velocity is facing the edge.
(check-expect (at-edge? T1) false)
(check-expect (at-edge? (make-tank (- WIDTH (/ (image-width TANK) 2)) 1)) true)
(check-expect (at-edge? (make-tank (/ (image-width TANK) 2) -1)) true)
 
;(define (at-edge? t) false)
;<Template from Tank>
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))

(define (at-edge? t)
  (or (and (<= (tank-x t) (/ (image-width TANK) 2))
           (negative? (tank-dir t)))
      (and (>= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))
           (positive? (tank-dir t)))))

;; Tank -> Tank
;; Move tank x position TANK-SPEED in dir direction
(check-expect (move-tank T1) (make-tank (+ 50 (* 1 TANK-SPEED)) 1))
(check-expect (move-tank T2) (make-tank (+ 50 (* -1 TANK-SPEED)) -1))

;(define (move-tank t) t)
;<Template from Tank>
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))

(define (move-tank t)
  (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED))
             (tank-dir t)))

;; Game -> Image
;; render game onto BACKGROUND
(check-expect (render-game G0)
              (place-image TANK (tank-x T0) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-game G2)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image MISSILE (missile-x M1) (missile-y M1)
                                        (place-image TANK (tank-x T1) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))))
(check-expect (render-game G3)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image INVADER (invader-x I2) (invader-y I2)
                                        (place-image MISSILE (missile-x M1) (missile-y M1)
                                                     (place-image MISSILE (missile-x M2) (missile-y M2)
                                                                  (place-image TANK (tank-x T1) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))))))

;(define (render-game g) BACKGROUND) ; stub
;<Template from Game but corrected>
#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-t g))))

(define (render-game g)
  (render-loinvader (game-invaders g)
                    (render-lom (game-missiles g)
                                (render-tank (game-t g)
                                             BACKGROUND))))
;; ListOfInvader Image -> Image
;; Render the list of invader on img
(check-expect (render-loinvader empty BACKGROUND)
              BACKGROUND)
(check-expect (render-loinvader LOI2 BACKGROUND)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image INVADER (invader-x I2) (invader-y I2)
                                        (place-image INVADER (invader-x I3) (invader-y I3) BACKGROUND))))

;(define (render-loinvader loinvader img) img) ; stub
;<Template from ListOfInvader>
#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

(define (render-loinvader loinvader img)
  (cond [(empty? loinvader) img]
        [else
         (place-image INVADER
                      (invader-x (first loinvader))
                      (invader-y (first loinvader))
                      (render-loinvader (rest loinvader) img))]))

;; ListOfMissile Image -> Image
;; Render the list of missiles on img
(check-expect (render-lom empty BACKGROUND)
              BACKGROUND)
(check-expect (render-lom LOM2 BACKGROUND)
              (place-image MISSILE (missile-x M1)(missile-y M1)
                           (place-image MISSILE (missile-x M2) (missile-y M2)
                                        (place-image MISSILE (missile-x M3) (missile-y M3) BACKGROUND))))

;(define (render-lom lom img) img) ; stub
;<Template from ListOfMissile>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-lom (rest lom) img))]))

;; Tank Image -> Image
;; Render tank image on img
(check-expect (render-tank T1 BACKGROUND)
              (place-image TANK
                           (tank-x T1)
                           (- HEIGHT (/ (image-height TANK) 2))
                           BACKGROUND))

;(define (render-tank t img) img) ; stub
;<Template from Tank>
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))

(define (render-tank t img)
  (place-image TANK
               (tank-x t)
               (- HEIGHT (/ (image-height TANK) 2))
               img))

;; Game -> Boolean
;; Return true when any invader has landed
(check-expect (game-over? G0) false)
(check-expect (game-over? (make-game (list I1) (list M1) T1)) false)
(check-expect (game-over? G3) true)

;(define (invaders-landed? g) false) ; stub
;<Template from Game but corrected>
#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-t g))))

(define (game-over? g)
  (invaders-landed? (game-invaders g)))

;; ListOfInvader -> Boolean
;; Return true if for any invader (invader-y >= (HEIGHT - (image-height INVADER) / 2)
(check-expect (invaders-landed? empty) false)
(check-expect (invaders-landed? LOI1) false)
(check-expect (invaders-landed? LOI2) true)

;(define (invaders-landed? loinvader) false) ; stub
;<Template from ListOfInvader>
#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

(define (invaders-landed? loinvader)
  (cond [(empty? loinvader) false]
        [else
         (if (>= (invader-y (first loinvader)) (- HEIGHT (/ (image-height INVADER) 2)))
             true
             (invaders-landed? (rest loinvader)))]))

;; Game -> Image
;; Return the last game image with GAME OVER render in the center
(check-expect (game-over G0) (place-image GAME-OVER
                                          (/ WIDTH 2)
                                          (/ HEIGHT 2)
                                          (render-game G0)))
                                      
(check-expect (game-over G3) (place-image GAME-OVER
                                          (/ WIDTH 2)
                                          (/ HEIGHT 2)
                                          (render-game G3)))

;(define (game-over g) BACKGROUND) ; stub
(define (game-over g)
  (place-image GAME-OVER
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render-game g)))

;; Game KeyEvent -> Game
;; Pressing left arrow set tank-dir in -1. Pressing right arrow set tank-dir in 1. Pressing spacebar shoot a missile
(check-expect (handle-key G1 "right") G1)
(check-expect (handle-key G1 "left")
              (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 -1)) "right")
              (make-game empty empty (make-tank 50 1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 -1)) "left")
              (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key G0 " ")
              (make-game empty
                         (list (make-missile (tank-x (game-t G0))
                                             (- HEIGHT (image-height TANK))))
                         (game-t G0)))

;(define (handle-key g ke) g) ; stub
;<Template from Game but corrected>
#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-t g))))

(define (handle-key g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-t g)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-t g)) 1))]
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (cons (make-missile (tank-x (game-t g)) (- HEIGHT (image-height TANK)))
                          (game-missiles g))
                    (game-t g))]
        [else g]))

(main G0)