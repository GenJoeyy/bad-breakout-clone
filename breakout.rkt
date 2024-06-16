;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname breakout) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; ein Vektor ist eine struktur: (make-posn Number Number)
; interp. der Vektor vom Ursprung zu der angegebenen Position
(define V00 (make-posn 0 0))
(define V01 (make-posn 0 1))
(define V10 (make-posn 1 0))
(define V11 (make-posn 1 1))

; Vektor Vektor -> Vektor
; Berechnet einen neuen Vektor durch
; komponentenweise Addition von v1 und v2
(define (vec-add v1 v2)
  (make-posn (+ (posn-x v1) (posn-x v2)) (+ (posn-y v1) (posn-y v2))))

(check-expect (vec-add V00 V00) V00)

; Vektor Vektor -> Vektor
; Berechnet einen neuen Vektor durch
; komponentenweise Subtraktion von v1 und v2
(define (vec-sub v1 v2)
  (make-posn (- (posn-x v1) (posn-x v2)) (- (posn-y v1) (posn-y v2))))

(check-expect (vec-sub V00 V00) V00)

; Vektor Number -> Vektor
; Berechnet einen neuen Vektor durch
; komponentenweise Skalarmultiplikation von v und s
(define (vec-skal-mult v s)
  (make-posn (* (posn-x v) s) (* (posn-y v) s)))

(check-expect (vec-skal-mult V00 1) V00)

; Vektor Vektor -> Number
; Berechnet das Kreuzprodukt zweier Vektoren
(define (vec-dot-prod v1 v2)
  (+ (* (posn-x v1) (posn-x v2)) (* (posn-y v1) (posn-y v2))))

(check-expect (vec-dot-prod V10 V11) 1)

; Vektor -> Vektor
; Berechnet den normierten Vektor für v
(define (vec-norm v)
  (make-posn (/ (posn-x v) (sqrt (+ (sqr (posn-x v)) (sqr (posn-y v)))))
             (/ (posn-y v) (sqrt (+ (sqr (posn-x v)) (sqr (posn-y v)))))))

(check-expect (vec-norm V01) V01)

; Struktur für einen Stein
(define-struct brick (number posn color))

; Struktur für einen Balken
(define-struct bar (posn))

; Struktur für einen Ball
(define-struct ball (posn direction velocity))

; Struktur für den WorldState
(define-struct worldstate (bricks ball bar current-scene game-running current-score current-key))

(define WINDOW_HEIGHT 500)
(define WINDOW_WIDTH (round (* WINDOW_HEIGHT 1.6)))

(define CENTER (make-posn (/ WINDOW_WIDTH 2) (/ WINDOW_HEIGHT 2)))

(define BAR_SPEED (round (/ WINDOW_WIDTH 90)))
(define BALL_SPEED (round (/ WINDOW_WIDTH 220)))

; Graphische Konstanten
(define MT (empty-scene WINDOW_WIDTH WINDOW_HEIGHT))
(define MT_BLACK
  (place-image (rectangle WINDOW_WIDTH WINDOW_HEIGHT "solid" "black")
               (posn-x CENTER)
               (posn-y CENTER)
               MT))

(define BALL_RADIUS (round (/ WINDOW_HEIGHT 100)))
(define BALL (circle BALL_RADIUS "solid" "white"))

(define BOTTOM_BORDER (+ WINDOW_HEIGHT BALL_RADIUS))
(define TOP_BORDER (* BALL_RADIUS -1))
(define LEFT_BORDER (* BALL_RADIUS -1))
(define RIGHT_BORDER (+ WINDOW_WIDTH BALL_RADIUS))

(define BAR_WIDTH (round (/ WINDOW_WIDTH 5)))
(define BAR_HEIGHT (round (/ WINDOW_HEIGHT 70)))
(define BAR (rectangle BAR_WIDTH BAR_HEIGHT "solid" "darkgrey"))

(define NUMBER_OF_BRICKS_IN_A_ROW 8)

(define BRICK_WIDTH (round (/ WINDOW_WIDTH NUMBER_OF_BRICKS_IN_A_ROW)))
(define BRICK_HEIGHT (round (/ WINDOW_HEIGHT 23)))

; Color -> Image
; Generiert das Image für einen Brick mit der
; spezifizierten Farbe und den festgelegten Maßen
(define (get-brick color)
  (rectangle (- BRICK_WIDTH 2) (- BRICK_HEIGHT 2) "solid" color))

(define OVERLAY_WIDTH (round (/ WINDOW_WIDTH 3)))
(define OVERLAY_HEIGHT (round (/ WINDOW_HEIGHT 4)))

; String -> Image
; Generiert das Overlay für den Start-Screen mit
; dem spezifizierten Text
(define (overlay-text text)
  (place-image text
               (/ OVERLAY_WIDTH 2)
               (/ OVERLAY_HEIGHT 2)
               (rectangle OVERLAY_WIDTH OVERLAY_HEIGHT "solid" (color 190 190 190 230))))

(define START_SCENE
  (overlay-text (text/font "Press any key to\nstart the game..."
                           (round (/ OVERLAY_HEIGHT 5))
                           "white"
                           "Gill Sans"
                           'swiss
                           'normal
                           'bold
                           false)))

(define TOP_GAP (* BRICK_HEIGHT 4))
(define BRICK_COLORS (list "red" "orange" "yellow" "green" "blue" "indigo" "lightpurple"))

; Number -> Image
; Generiert den Rainbow auf schwarzem Grund
; i sollte beim Aufruf mit 0 initialisiert werden
(define (generate-rainbow i)
  (if (= i (length BRICK_COLORS))
      MT_BLACK
      (place-image (rectangle WINDOW_WIDTH BRICK_HEIGHT "solid" (list-ref BRICK_COLORS i))
                   (posn-x CENTER)
                   (+ TOP_GAP (+ (/ BRICK_HEIGHT 2) (* i BRICK_HEIGHT)))
                   (generate-rainbow (add1 i)))))

(define RAINBOW_HEIGHT (* (length BRICK_COLORS) BRICK_HEIGHT))

; String Number Number Number Number Image -> Image
; Schreibt einen Text mit Schatten auf ein Bild
; Textgröße, Position auf dem Bild, Schattengröße (offset auf x-Achse) und das
; Bild müssen angegeben werden
(define (place-text-with-shadow text size x y shadow-size scene)
  (place-image
   (text/font text (round size) "white" "Gill Sans" 'swiss 'normal 'bold false)
   (- x shadow-size)
   y
   (place-image (text/font text (round size) "black" "Gill Sans" 'swiss 'normal 'bold false)
                (+ x shadow-size)
                y
                scene)))

; String -> Image
; Ruft place-text-with-shadow sowie generate-rainbow auf um
; einen Regenbogen mit Text zu generieren
(define (place-text-on-rainbow text)
  (place-text-with-shadow text
                          (/ RAINBOW_HEIGHT 1.4)
                          (/ WINDOW_WIDTH 2)
                          (+ TOP_GAP (/ RAINBOW_HEIGHT 2))
                          2
                          (generate-rainbow 0)))

(define TICKET_HEIGHT (* RAINBOW_HEIGHT 0.8))
(define TICKET_WIDTH (* TICKET_HEIGHT 2.4))
(define TICKET_COLOR (color 0 140 250 255))
(define EDGES_RADIUS (round (* TICKET_HEIGHT 0.1)))

; Number Boolean -> Image
; Generiert ein Bild eines Tickets mit dem Endscore
(define (ticket-img score won?)
  (place-text-with-shadow
   (string-append "Score: " (number->string score))
   (* TICKET_HEIGHT 0.3)
   (/ TICKET_WIDTH 2)
   (- (/ TICKET_HEIGHT 2) (* TICKET_HEIGHT 0.15))
   1
   (place-text-with-shadow
    (if won? "Well done!" "Try again...")
    (* TICKET_HEIGHT 0.2)
    (/ TICKET_WIDTH 2)
    (+ (/ TICKET_HEIGHT 2) (* TICKET_HEIGHT 0.17))
    1
    (place-image
     (rectangle (* TICKET_WIDTH 0.85) (* TICKET_HEIGHT 0.8) "outline" "white")
     (/ TICKET_WIDTH 2)
     (/ TICKET_HEIGHT 2)
     (place-image
      (circle EDGES_RADIUS "solid" "black")
      0
      0
      (place-image
       (circle EDGES_RADIUS "solid" "black")
       0
       TICKET_HEIGHT
       (place-image (circle EDGES_RADIUS "solid" "black")
                    TICKET_WIDTH
                    0
                    (place-image (circle EDGES_RADIUS "solid" "black")
                                 TICKET_WIDTH
                                 TICKET_HEIGHT
                                 (rectangle TICKET_WIDTH TICKET_HEIGHT "solid" TICKET_COLOR)))))))))

; String Number Boolean -> Image
; Generiert die Endeszene
(define (end-scene text score won?)
  (place-image (ticket-img score won?)
               (posn-x CENTER)
               (+ (+ (+ RAINBOW_HEIGHT TOP_GAP) (/ TICKET_HEIGHT 2)) BRICK_HEIGHT)
               (place-text-on-rainbow text)))

; Number -> Image
(define (won-scene score)
  (end-scene "YOU WON!" score #t))

; Number -> Image
(define (game-over-scene score)
  (end-scene "GAME OVER" score #f))

(define SCOREBOARD_WIDTH (* WINDOW_WIDTH 0.16))
(define SCOREBOARD_HEIGHT (* TOP_GAP 0.6))
(define SCOREBOARD (rectangle SCOREBOARD_WIDTH SCOREBOARD_HEIGHT "solid" (color 0 0 0 210)))

; Number -> Image
; Generiert ein Bild, welches den aktuellen Score während des Spiels anzeigt
(define (scoreboard-with-text score)
  (place-image (text/font (string-append "SCORE: " (number->string score))
                          (round (* SCOREBOARD_HEIGHT 0.35))
                          "white"
                          "Gill Sans"
                          'swiss
                          'normal
                          'bold
                          false)
               (/ SCOREBOARD_WIDTH 2)
               (/ SCOREBOARD_HEIGHT 2)
               SCOREBOARD))

; Initialer Worldstate
(define INITIAL_BARPOSN (make-posn (posn-x CENTER) (- WINDOW_HEIGHT (* BAR_HEIGHT 4))))
(define INITIAL_BALLPOSN (make-posn (posn-x CENTER) (- (posn-y INITIAL_BARPOSN) (* BALL_RADIUS 3))))

(define BAR_LEFT_BORDER (- (posn-x INITIAL_BALLPOSN) (/ BAR_WIDTH 2)))
(define BAR_TOP_BORDER (- (posn-y INITIAL_BARPOSN) (/ BAR_HEIGHT 2)))

(define RANDOM_BALL_START_DIRECTION
  (vec-sub (make-posn (+ BAR_LEFT_BORDER (random BAR_WIDTH)) BAR_TOP_BORDER) INITIAL_BALLPOSN))

; Number -> Number
; Nimmt den Index eines Bricks und gibt den Index seiner Reihe aus
(define (get-brick-row number)
  (floor (/ number NUMBER_OF_BRICKS_IN_A_ROW)))

; Number -> Number
; Nimmt den Index eines Bricks und gibt den Index seiner Spalte aus
(define (get-brick-col number)
  (modulo number NUMBER_OF_BRICKS_IN_A_ROW))

; Number -> Brick
; Erstellt einen Brick und berechnet seine Position anhand des Index
(define (generate-brick number)
  (make-brick number
              (make-posn (+ (* BRICK_WIDTH (get-brick-col number)) (/ BRICK_WIDTH 2))
                         (+ (+ (* BRICK_HEIGHT (get-brick-row number)) (/ BRICK_HEIGHT 2)) TOP_GAP))
              (list-ref BRICK_COLORS (get-brick-row number))))

; Number -> List
; Rekursive Methode, welche die initiale Liste an Bricks erstellt
(define (generate-all-bricks-rec index)
  (if (>= index (* NUMBER_OF_BRICKS_IN_A_ROW (length BRICK_COLORS)))
      '()
      (cons (generate-brick index) (generate-all-bricks-rec (add1 index)))))

(define ALL_BRICKS (generate-all-bricks-rec 0))

(define INITIAL_WORLDSTATE
  (make-worldstate ALL_BRICKS
                   (make-ball INITIAL_BALLPOSN RANDOM_BALL_START_DIRECTION BALL_SPEED)
                   (make-bar INITIAL_BARPOSN)
                   MT_BLACK
                   false
                   0
                   "none"))

(define SCORE_PER_ROW (list 200 150 125 80 45 20 5))

; Brick -> Number
; Gibt den Score eines Bricks bei dessen Zerstörung an
(define (get-score brick)
  (if (null? brick) 0 (list-ref SCORE_PER_ROW (get-brick-row (brick-number brick)))))

; Ball Image -> Image
; Fügt den Ball auf dem Image ein
(define (render-ball ball scene)
  (place-image BALL (posn-x (ball-posn ball)) (posn-y (ball-posn ball)) scene))

; Bar Image -> Image
; Fügt die Bar auf dem Image ein
(define (render-bar bar scene)
  (place-image BAR (posn-x (bar-posn bar)) (posn-y (bar-posn bar)) scene))

; Brick Image -> Image
; Fügt den Brick auf dem Image ein
(define (render-single-brick brick scene)
  (place-image (get-brick (brick-color brick))
               (posn-x (brick-posn brick))
               (posn-y (brick-posn brick))
               scene))

; List Image Number -> Image
; Rekursive Hilfsmethode
(define (render-bricks-rec bricks scene index)
  (if (>= index (length bricks))
      scene
      (render-bricks-rec bricks (render-single-brick (list-ref bricks index) scene) (add1 index))))

; List Image -> Image
; Fügt alle Bricks auf dem Image ein
(define (render-bricks bricks scene)
  (render-bricks-rec bricks scene 0))

; Number Image -> Image
; Fügt den aktuellen Score auf dem Image ein
(define (render-scoreboard score scene)
  (place-image (scoreboard-with-text score) (posn-x CENTER) (/ SCOREBOARD_HEIGHT 2) scene))

; Worldstate -> Image
; Ruft alle Hilfsmethoden auf, um den aktuellen Worldstate graphisch darzustellen
(define (render-world worldstate)
  (render-scoreboard (worldstate-current-score worldstate)
                     (render-bricks (worldstate-bricks worldstate)
                                    (render-bar (worldstate-bar worldstate)
                                                (render-ball (worldstate-ball worldstate)
                                                             MT_BLACK)))))

; Worldstate -> Image
; Rendert den Startscreen oder den aktuellen Worldstate
(define (render worldstate)
  (if (worldstate-game-running worldstate)
      (render-world worldstate)
      (place-image START_SCENE (posn-x CENTER) (posn-y CENTER) (render-world worldstate))))

; Worldstate Image -> Image
; Rendert den spezifizierten Endscreen
(define (render-end-scene worldstate image)
  (place-image image (posn-x CENTER) (posn-y CENTER) (render worldstate)))

; Worldstate -> Image
(define (render-game-over worldstate)
  (render-end-scene worldstate (game-over-scene (worldstate-current-score worldstate))))

; Worldstate -> Image
(define (render-won worldstate)
  (render-end-scene worldstate (won-scene (worldstate-current-score worldstate))))

; WorldState -> Boolean
; Überprüft, ob das Spiel gewonnen wurde indem alle Bricks zerstört wurden
(define (is-game-won worldstate)
  (equal? 0 (length (worldstate-bricks worldstate))))

(check-expect
 (is-game-won (make-worldstate '() (make-ball V00 V00 0) (make-bar V00) MT_BLACK false 0 "none"))
 true)
(check-expect (is-game-won (make-worldstate (list (make-brick V00 "red"))
                                            (make-ball V00 V00 0)
                                            (make-bar V00)
                                            MT_BLACK
                                            false
                                            0
                                            "none"))
              false)

; WorldState -> Boolean
; Überprüft, ob das Spiel verloren wurde
(define (is-game-lost ballposn)
  (or (or (> (posn-y ballposn) BOTTOM_BORDER) (< (posn-y ballposn) TOP_BORDER))
      (or (> (posn-x ballposn) RIGHT_BORDER) (< (posn-y ballposn) LEFT_BORDER))))

; Worldstate -> Image
; Entscheided, ob Spiel gewonnen oder verloren wurde und gibt das entsprechende Image aus
(define (render-proper-end-scene worldstate)
  (cond
    [(is-game-lost (ball-posn (worldstate-ball worldstate))) (render-game-over worldstate)]
    [(is-game-won worldstate) (render-won worldstate)]
    [else (render worldstate)]))

; Worldstate Number -> Number
; Moves the bar by amount x, making sure it doesnt leave the window
(define (move-bar-boundry-safe worldstate x)
  (max (/ BAR_WIDTH 2)
       (min (- WINDOW_WIDTH (/ BAR_WIDTH 2)) (+ (posn-x (bar-posn (worldstate-bar worldstate))) x))))

; Keyevent -> Boolean
(define (is-left-key key)
  (if (key-event? key) (or (key=? key "left") (key=? key "a")) #f))

; Keyevent -> Boolean
(define (is-right-key key)
  (if (key-event? key) (or (key=? key "right") (key=? key "d")) #f))

; Worldstate -> Bar
; Bewegt die Bar basierend auf dem aktuell gedrückten Key
(define (move-bar worldstate)
  (make-bar (make-posn (move-bar-boundry-safe
                        worldstate
                        (cond
                          [(is-left-key (worldstate-current-key worldstate)) (* BAR_SPEED -1)]
                          [(is-right-key (worldstate-current-key worldstate)) BAR_SPEED]
                          [else 0]))
                       (posn-y (bar-posn (worldstate-bar worldstate))))))

; Worldstate -> Worldstate
; Startet das Spiel
(define (start-game worldstate)
  (make-worldstate (worldstate-bricks worldstate)
                   (worldstate-ball worldstate)
                   (worldstate-bar worldstate)
                   (worldstate-current-scene worldstate)
                   true
                   (worldstate-current-score worldstate)
                   (worldstate-current-key worldstate)))

; Worldstate Keyevent -> Worldstate
; Speichert den gedrückten Key im Worldstate ab
(define (keyHandler worldstate key)
  (if (worldstate-game-running worldstate)
      (make-worldstate (worldstate-bricks worldstate)
                       (worldstate-ball worldstate)
                       (worldstate-bar worldstate)
                       (worldstate-current-scene worldstate)
                       (worldstate-game-running worldstate)
                       (worldstate-current-score worldstate)
                       key)
      (if (key-event? key)
          (if (not (or (is-left-key key) (is-right-key key))) (start-game worldstate) worldstate)
          worldstate)))

; Worldstate Keyevent -> Worldstate
; Löscht den losgelassenen Key aus dem Worldstate, falls dieser gedrückt war
(define (release-key worldstate released-key)
  (make-worldstate (worldstate-bricks worldstate)
                   (worldstate-ball worldstate)
                   (worldstate-bar worldstate)
                   (worldstate-current-scene worldstate)
                   (worldstate-game-running worldstate)
                   (worldstate-current-score worldstate)
                   (if (not (key-event? (worldstate-current-key worldstate)))
                       "none"
                       (if (key=? released-key (worldstate-current-key worldstate))
                           "none"
                           (worldstate-current-key worldstate)))))

; Worldstate -> Boolean
; Überprüft, ob das Spiel beendet ist
(define (end-of-the-world worldstate)
  (or (is-game-won worldstate) (is-game-lost (ball-posn (worldstate-ball worldstate)))))

; Struktur für eine Bounding Box
(define-struct bounding-box (min-x max-x min-y max-y))

; Ball -> BoundingBox
(define (ball-box ball)
  (make-bounding-box (- (posn-x (ball-posn ball)) BALL_RADIUS)
                     (+ (posn-x (ball-posn ball)) BALL_RADIUS)
                     (- (posn-y (ball-posn ball)) BALL_RADIUS)
                     (+ (posn-y (ball-posn ball)) BALL_RADIUS)))

; Bar -> BoundingBox
(define (bar-box worldstate)
  (make-bounding-box (- (posn-x (bar-posn (worldstate-bar worldstate))) (/ BAR_WIDTH 2))
                     (+ (posn-x (bar-posn (worldstate-bar worldstate))) (/ BAR_WIDTH 2))
                     (- (posn-y (bar-posn (worldstate-bar worldstate))) (/ BAR_HEIGHT 2))
                     (+ (posn-y (bar-posn (worldstate-bar worldstate))) (/ BAR_HEIGHT 2))))

; Brick -> BoundingBox
(define (brick-box brick)
  (make-bounding-box (- (posn-x (brick-posn brick)) (/ BRICK_WIDTH 2))
                     (+ (posn-x (brick-posn brick)) (/ BRICK_WIDTH 2))
                     (- (posn-y (brick-posn brick)) (/ BRICK_HEIGHT 2))
                     (+ (posn-y (brick-posn brick)) (/ BRICK_HEIGHT 2))))

; BoundingBox BoundingBox -> Boolean
; Überprüft, ob zwei BoundingBoxen kollidieren
(define (collision? box1 box2)
  (and (and (< (bounding-box-min-x box1) (bounding-box-max-x box2))
            (< (bounding-box-min-x box2) (bounding-box-max-x box1)))
       (and (< (bounding-box-min-y box1) (bounding-box-max-y box2))
            (< (bounding-box-min-y box2) (bounding-box-max-y box1)))))

(check-expect (collision? (make-bounding-box 0 2 0 2) (make-bounding-box 1 3 1 3)) true)
(check-expect (collision? (make-bounding-box 0 1 0 1) (make-bounding-box 2 3 2 3)) false)
(check-expect (collision? (make-bounding-box 0 2 0 2) (make-bounding-box 2 3 2 3)) false)

; WorldState BoundingBox -> Boolean
; Überprüft, ob der Ball mit der BoundingBox kollidiert
(define (ball-collision? worldstate box)
  (collision? (ball-box (worldstate-ball worldstate)) box))

(check-expect
 (ball-collision?
  (make-worldstate '() (make-ball (make-posn 1 1) V00 0) (make-bar V00) MT_BLACK false 0 "none")
  (make-bounding-box 0 2 0 2))
 true)
(check-expect
 (ball-collision?
  (make-worldstate '() (make-ball (make-posn 3 3) V00 0) (make-bar V00) MT_BLACK false 0 "none")
  (make-bounding-box 0 2 0 2))
 false)

(define (ball-collision-with-brick? worldstate list)
  (cond
    [(empty? list) false]
    [(ball-collision? worldstate (brick-box (first list))) true]
    [else (ball-collision-with-brick? worldstate (rest list))]))

; Vektor Vektor Number -> Ball
; Bewegt den Ball um die Richtung und Geschwindigkeit
(define (move-ball pos dir vel)
  (make-ball (vec-add pos (vec-skal-mult (vec-norm dir) vel)) dir vel))

(check-expect (move-ball V00 V01 1) (make-ball V01 V01 1))
(check-expect (move-ball V00 V10 2) (make-ball (make-posn 2 0) V10 2))
(check-expect
 (move-ball V11 V11 1)
 (make-ball (make-posn (+ (posn-x V11) (/ 1 (sqrt 2))) (+ (posn-y V11) (/ 1 (sqrt 2)))) V11 1))

; Vektor -> Vektor
; Gibt den neuen Richtungsvektor nach einer Kollision mit der Decke an
(define (reflect-on-horizontal-wall dir)
  (vec-sub dir (vec-skal-mult V01 (* 2 (vec-dot-prod dir V01)))))

; Vektor -> Vektor
; Gibt den neuen Richtungsvektor nach einer Kollision mit einer Wand an
(define (reflect-on-vertical-wall dir)
  (vec-sub dir (vec-skal-mult V10 (* 2 (vec-dot-prod dir V10)))))

; Vektor -> Vektor
; Gibt den neuen Richtungsvektor nach einer Kollision mit einem Brick oder der Bar an
(define (reflect-on-hit ball hit-item)
  (vec-norm (vec-sub ball hit-item)))

; Worldstate String Brick -> Ball
; Ändert die Direction des Balles im Falle einer Kollision
; Im Falle eines Bricks muss dieser mit übergeben werden
(define (reflect-ball worldstate direction brick?)
  (cond
    [(equal? direction "bar")
     (move-ball (ball-posn (worldstate-ball worldstate))
                (reflect-on-hit (ball-posn (worldstate-ball worldstate))
                                (bar-posn (worldstate-bar worldstate)))
                (ball-velocity (worldstate-ball worldstate)))]
    [(equal? direction "brick")
     (move-ball (ball-posn (worldstate-ball worldstate))
                (reflect-on-hit (ball-posn (worldstate-ball worldstate)) (brick-posn brick?))
                (ball-velocity (worldstate-ball worldstate)))]
    [(equal? direction "horizontal")
     (move-ball (ball-posn (worldstate-ball worldstate))
                (reflect-on-horizontal-wall (ball-direction (worldstate-ball worldstate)))
                (ball-velocity (worldstate-ball worldstate)))]
    [(equal? direction "vertical")
     (move-ball (ball-posn (worldstate-ball worldstate))
                (reflect-on-vertical-wall (ball-direction (worldstate-ball worldstate)))
                (ball-velocity (worldstate-ball worldstate)))]
    [else
     (move-ball (ball-posn (worldstate-ball worldstate))
                (ball-direction (worldstate-ball worldstate))
                (ball-velocity (worldstate-ball worldstate)))]))

; Worldstate -> Ball
; Überprüft auf Kollision und ruft im Falle nötige Funktionen auf
(define (check-direction-and-move-ball worldstate)
  (cond
    [(ball-collision? worldstate (bar-box worldstate)) (reflect-ball worldstate "bar" "")]
    [(ball-collision? worldstate (make-bounding-box 0 WINDOW_WIDTH -50 0))
     (reflect-ball worldstate "horizontal" "")]
    [(or (ball-collision? worldstate (make-bounding-box -50 0 0 WINDOW_HEIGHT))
         (ball-collision? worldstate
                          (make-bounding-box WINDOW_WIDTH (+ 50 WINDOW_WIDTH) 0 WINDOW_HEIGHT)))
     (reflect-ball worldstate "vertical" "")]
    [else (reflect-ball worldstate "" "")]))

; Worldstate List Ball -> Brick
(define (find-collision-brick worldstate list ball)
  (cond
    [(null? list) false]
    [(ball-collision? worldstate (brick-box (first list))) (first list)]
    [else (find-collision-brick worldstate (rest list) ball)]))

; Worldstate List -> List
(define (remove-collision-brick worldstate bricks)
  (cond
    [(null? bricks) bricks]
    [(ball-collision? worldstate (brick-box (first bricks))) (rest bricks)]
    [else (cons (first bricks) (remove-collision-brick worldstate (rest bricks)))]))

; Worldstate Brick -> Worldstate
; Bewegt die Bar, reflektiert den Ball und entfernt den getroffenen Brick
(define (get-worldstate-for-collision worldstate collision-brick)
  (make-worldstate (remove-collision-brick worldstate (worldstate-bricks worldstate))
                   (reflect-ball worldstate "brick" collision-brick)
                   (move-bar worldstate)
                   (worldstate-current-scene worldstate)
                   (worldstate-game-running worldstate)
                   (+ (worldstate-current-score worldstate) (get-score collision-brick))
                   (worldstate-current-key worldstate)))

; Worldstate -> Worldstate
; Bewegt die Bar
(define (get-worldstate-without-collision worldstate)
  (make-worldstate (worldstate-bricks worldstate)
                   (check-direction-and-move-ball worldstate)
                   (move-bar worldstate)
                   (worldstate-current-scene worldstate)
                   (worldstate-game-running worldstate)
                   (worldstate-current-score worldstate)
                   (worldstate-current-key worldstate)))

; Ball -> Boolean
; Überprüft, ob der Ball sich im Bereich der Bricks befindet
; Diese Funktion wird genutzt um unnötige rekursive Aufrufe einzusparen und eine
; höhere Effizienz sowie weniger Lag zu erreichen
(define (collision-possible? ball)
  (< (posn-y (ball-posn ball)) (+ BALL_RADIUS (+ TOP_GAP (* BRICK_HEIGHT (length BRICK_COLORS))))))

; Worldstate -> Worldstate
(define (tick worldstate)
  (if (worldstate-game-running worldstate)
      (if (collision-possible? (worldstate-ball worldstate))
          (if (ball-collision-with-brick? worldstate (worldstate-bricks worldstate))
              (get-worldstate-for-collision worldstate
                                            (find-collision-brick worldstate
                                                                  (worldstate-bricks worldstate)
                                                                  (worldstate-ball worldstate)))
              (get-worldstate-without-collision worldstate))
          (get-worldstate-without-collision worldstate))
      worldstate))

(big-bang INITIAL_WORLDSTATE
          (on-tick tick 0.01)
          (to-draw render)
          (on-key keyHandler)
          (on-release release-key)
          (stop-when end-of-the-world render-proper-end-scene)
          (close-on-stop false)
          (name "Breakout v1.0"))
