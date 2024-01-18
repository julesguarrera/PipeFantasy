;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; Pipe-dream: PART 2 Official Solution v1.1
;; NOTE if using this solution for part 3:
;; - Do not delete lines that starts with ;;! when editing this solution.
;; - If this solution needs to be modified, we will make
;;   an announcement on Piazza in the post titled "Homework 11 Clarifications".
;;   So check Piazza often!
;; - Some tests are omitted from this file. You do not have to fill the tests in
;;   if you are just using these functions elsewhere in your code. However, you have to write tests for all
;;   the functions you modify, excluding the ones that returns an image.

;; Complete Task 7 here, after you have finished all other tasks.
;; (pipe-fantasy game-test)

(define TILE-SIZE 100)
(define PIPE-WIDTH 30)
(define GRID-DIM 6)

;; A pipe is a (make-pipe bool bool bool bool)
(define-struct pipe (top bottom left right))
;; representation of a pipe tile.
;; top, bot, left, right
;; represents if the corresponding direction is open for flow.
;; Any combination of trueness for top, bot, left, right are allowed except
;; when three of them are true.
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TB (make-pipe #true #true #false #false))

(define START-PIPES
  (map
   (lambda (pipe-dirs) (apply make-pipe pipe-dirs))
   (list
    (list #true #false #false #false)
    (list #false #true #false #false)
    (list #false #false #true #false)
    (list #false #false #false #true))))

;; Double checked in the original game and found out that there is no T shaped pipes.
(define PASS-THROUGH-PIPES
  (map
   (lambda (pipe-dirs) (apply make-pipe pipe-dirs))
   (list
    (list #true #true #false #false)
    (list #true #false #true #false)
    (list #true #false #false #true)
    (list #false #true #true #false)
    (list #false #true #false #true)
    (list #false #false #true #true)
    (list #true #true #true #true))))

;; Let's have all types of pipes in a list!
(define ALL-PIPES (append PASS-THROUGH-PIPES START-PIPES))

;; direction is one of:
;; - "top"
;; - "bottom"
;; - "left"
;; - "right"

;; reverse-direction: direction -> direction
;; produce the reverse direction of the given direction
(define (reverse-direction dir)
  (cond
    [(string=? dir "top") "bottom"]
    [(string=? dir "bottom") "top"]
    [(string=? dir "left") "right"]
    [(string=? dir "right") "left"]))

;; pipe-flow-into: pipe direction -> [Optional direction]
;; can a pipe allow liquid flowing into the tile, recieving the liquid from the given direction?
;; if so, produce the new direction it is going.
(define (pipe-can-flow pipe dir)
  (cond
    [(and (string=? dir "top")
          (pipe-top pipe))
     (cond
       [(pipe-bottom pipe) "bottom"]
       [(pipe-left pipe) "left"]
       [(pipe-right pipe) "right"])]
    [(and (string=? dir "bottom")
          (pipe-bottom pipe)) 
     (cond
       [(pipe-top pipe) "top"]
       [(pipe-left pipe) "left"]
       [(pipe-right pipe) "right"])]
    [(and (string=? dir "left")
          (pipe-left pipe))
     (cond
       [(pipe-right pipe) "right"]
       [(pipe-top pipe) "top"]
       [(pipe-bottom pipe) "bottom"])]
    [(and (string=? dir "right")
          (pipe-right pipe))
     (cond
       [(pipe-left pipe) "left"]
       [(pipe-top pipe) "top"]
       [(pipe-bottom pipe) "bottom"])]
    [else #false]))

(check-expect (pipe-can-flow PIPE-TL "top") "left")
(check-expect (pipe-can-flow PIPE-TL "bottom") #false)


;; my-member?: (list Integer Integer) [list-of (list Integer Integer)] -> boolean
          ;; produce true if maybe-elem is in lox
(check-expect (my-member? (list 1 2) (list (list 1 2))) #true)
(check-expect (my-member? (list 3 2) (list (list 4 1))) #false)
(check-expect (my-member? (list 0 2) (list (list 2 1))) #false)

(define (my-member? maybe-elem lox)
  (and (not (empty? lox))
       (or (and (= (first maybe-elem)
                   (first (first lox)))
                (= (second maybe-elem)
                   (second (first lox))))
           (my-member? maybe-elem (rest lox)))))

;; pipe-image: pipe Number Number boolean gooflow integer integer list posinteger posinteger -> image
;; make an image of the pipe, given size of tile and width of pipe.
;; assuming size of tile is always larger than size of pipe.
;; if the pipe is filled, the pipe is green, otherwise it is black.


; *** the issue here is that the pipe is taking in the direction that the goo
; is flowing in the current gamestate, need to find a way to only take in that specific pipe's
; direction. def need to incorporate gooflow somewhere. cant have the direction constantly updating bc
; it will keep changing the direction of the cross pipe's goo

; maybe take in pipe location along w gf and compare coords w gf coords and return the gf direction if true (helper)
; then compare the direction

; also need to change recross? so that it doesnt constantly update from false to true and vice versa
; since that means the cross pipe will only be filled for one tick and then will revert
; might just scrap recross? and find another way to check if the pipe already has goo in it.
; maybe incorporate something like my-member? from task 4 to check if there is already goo
; and if so, return a cross pipe full of goo
; check if gf is in rest gf using my-member?
; if true, return full cross-pipe

(define (pipe-image pipe tile-size pipe-width filled? gf x y)
  (local
    [(define (dir gf x y)
       (cond
         [(or (empty? gf) (boolean? (first gf))) #false]
         [(cons? gf)
          (if
           (and (= (first (first gf)) x) (= (second (first gf)) y))
           (third (first gf))
           (dir (rest gf) x y))]))
     (define (recross? gf x y)
       (cond
         [(or (empty? gf) (boolean? (first gf))) #false]
         [(cons? gf)
          (if
           (and (= (first (first gf)) x) (= (second (first gf)) y))
           (my-member? (first gf) (rest gf))
           (recross? (rest gf) x y))]))
     (define (pipe-image/custom-color pipe tile-size pipe-width bg-color pipe-color)
       (local
         [(define mid-left-value (quotient (- tile-size pipe-width) 2))
          (define longer-end-size (quotient (+ tile-size pipe-width) 2))
          ;; vertical-pipe: color -> image
          ;; make a vertical pipe component of the given color
          (define (vertical-pipe color)
            (rectangle pipe-width longer-end-size "solid" color))
          ;; horizontal-pipe: color -> image
          ;; make a horizontal pipe component of the given color
          (define (horizontal-pipe color)
            (rectangle longer-end-size pipe-width "solid" color))
          (define pipe-offsets-x (list mid-left-value mid-left-value 0 mid-left-value))
          (define pipe-offsets-y (list 0 mid-left-value mid-left-value mid-left-value))
          (define empty-cross
           (foldl
          (lambda
              (x y img result-img)
            (underlay/xy result-img x y img))
          (rectangle tile-size tile-size "solid" bg-color)
          pipe-offsets-x
          pipe-offsets-y
          (list (vertical-pipe "black") (vertical-pipe "black") (horizontal-pipe "black") (horizontal-pipe "black"))))

          ;; pipe-sub-images is the list of image components of the pipe.
          ;; it will be 
          ;; (list top-image bottom-image left-image right-image)
          ;; where each image is either a colored pipe or a transparent image.
          ;; depending on if the pipe is open in that direction.
          (define (pipe-sub-imgs pipe gf x y)
            (if
             (and
              (and (pipe-top pipe) (and (pipe-bottom pipe) (and (pipe-left pipe) (pipe-right pipe))))
              (not (boolean? (dir gf x y))))
             (cond
               [(recross? gf x y)
                (list (vertical-pipe pipe-color) (vertical-pipe pipe-color) (horizontal-pipe pipe-color) (horizontal-pipe pipe-color))]
               [(or (string=? (dir gf x y) "left") (string=? (dir gf x y) "right"))
                (list (vertical-pipe "transparent") (vertical-pipe "transparent") (horizontal-pipe pipe-color) (horizontal-pipe pipe-color))]
               [(or (string=? (dir gf x y) "top") (string=? (dir gf x y) "bottom"))
                (list (vertical-pipe pipe-color) (vertical-pipe pipe-color) (horizontal-pipe "transparent") (horizontal-pipe "transparent"))])
             (map (lambda (kind pipe-fn) (kind (if (pipe-fn pipe) pipe-color "transparent")))
                 (list vertical-pipe vertical-pipe horizontal-pipe horizontal-pipe) ;; kind
                 (list pipe-top pipe-bottom pipe-left pipe-right)))) ;; pipe-fn
          ]

         ;; combine the pipe sub images together.
         ;; note that how foldl, like map, can accept multiple lists.
         (if
          (and (not (recross? gf x y))
           (and
              (and (pipe-top pipe) (and (pipe-bottom pipe) (and (pipe-left pipe) (pipe-right pipe))))
              (not (boolean? (dir gf x y)))))
          
          (foldl
          (lambda
              (x y img result-img)
            (underlay/xy result-img x y img))
          empty-cross
          pipe-offsets-x
          pipe-offsets-y
          (pipe-sub-imgs pipe gf x y))
          
          (foldl
           (lambda
               (x y img result-img)
             (underlay/xy result-img x y img))
           (rectangle tile-size tile-size "solid" bg-color)
           pipe-offsets-x
           pipe-offsets-y
           (pipe-sub-imgs pipe gf x y)))))]
    (pipe-image/custom-color pipe tile-size pipe-width "grey" (if filled? "green" "black"))))

;; A grid is a (make-grid Integer (listof (list Integer Integer pipe))
(define-struct grid (dim cells))
;; dim is the dimension of the square grid.
;; cells is the representation of cells.
;; with each element is (list x-coordinate y-coordinate pipe-representation)
;; elements in the list must be sorted by the order of x-coor and y-coor
;; and (x-coor, y-coor) is unique in cells.
;; length of cells must be less than (dim * dim)

(define STARTING-GRID (make-grid GRID-DIM empty))

;; Note: The following implementation for place-pipe and pipe-at is more than what is needed:
;; it makes sure that pipes in grid-cells are in order of (row, col), with row being the primary key.

;; place-pipe: grid pipe Integer Integer -> grid
;; produce the grid after placing pipe on it at grid location (grid-x, grid-y)
(define (place-pipe grid pipe grid-x grid-y)
  (local
    [(define (insert-pipe-at-pos cells)
       (cond
         [(empty? cells) (list (list grid-x grid-y pipe))]
         [(and (= grid-x (first (first cells)))
               (= grid-y (second (first cells))))
          (cons (list grid-x grid-y pipe) (rest cells))]
         [(or (< grid-x (first (first cells)))
              (and
               (= grid-x (first (first cells)))
               (< grid-y (second (first cells)))))
          (cons (list grid-x grid-y pipe) cells)]
         [else
          (cons (first cells) (insert-pipe-at-pos (rest cells)))]))]
    (make-grid
     (grid-dim grid)
     (insert-pipe-at-pos (grid-cells grid)))))

(define SMALL-GRID1 (make-grid 2 (list (list 0 1 PIPE-TL))))
(define SMALL-GRID2 (make-grid 2 (list (list 1 0 PIPE-TB) (list 1 1 PIPE-TL))))

(check-expect (place-pipe (make-grid 2 empty) PIPE-TL 0 1) SMALL-GRID1)
(check-expect (place-pipe
               (place-pipe (make-grid 2 empty) PIPE-TL 1 1)
               PIPE-TB 1 0)
              SMALL-GRID2)
(check-expect (place-pipe
               (place-pipe (make-grid 2 empty) PIPE-TB 1 0)
               PIPE-TL 1 1)
              SMALL-GRID2)

;; pipe-at: grid Integer Integer -> grid
;; get the pipe at the grid location (x, y)
(define (pipe-at grid x y)
  (local
    [(define (find-pipe-at-pos cells)
       (cond
         [(empty? cells) #false]
         [(and (= x (first (first cells)))
               (= y (second (first cells))))
          (third (first cells))]
         [(or (< x (first (first cells)))
              (and
               (= x (first (first cells)))
               (< y (second (first cells)))))
          #false]
         [else
          (find-pipe-at-pos (rest cells))]))]
    (find-pipe-at-pos (grid-cells grid))))

;; tests
(check-expect (pipe-at SMALL-GRID1 0 1) PIPE-TL)
(check-expect (pipe-at SMALL-GRID1 1 1) #false)
(check-expect (pipe-at SMALL-GRID1 0 0) #false)

;; Goo logic

;; GooFlow is [ne-list-of [any-of #false (list Integer Integer direction)]]
;; the history of location of goo in the maps, and the direction it is flowing.
;; ordered from the most recent goo propagation location to the least recent.
;; That is, (first GooFlow) will be the most recent goo location.
;; #false can only appear in front of the list, means the goo have already spilled
;; and propagate any further.
;; (This design is to facilitate faster goo propagation ending checking for part 3)



; direction?: GooFlow -> (Optional) direction
; returns the direction of the gooflow if applicable
(check-expect (direction? (list (list 2 2 "right")(list 2 3 "left")(list 1 2 "top"))) "right")
(check-expect (direction? (list (list 1 4 "left")(list 2 0 "bottom")(list 1 0 "right"))) "left")
(check-expect (direction? (list (list 2 2 "top")(list 2 3 "right")(list 1 2 "bottom"))) "top")

(define (direction? gf)
  (if (or (empty? gf) (false? (first gf)))
      gf 
      (third (first gf))))


; cross-again?: GooFlow Integer Integer Integer-> Boolean
; returns true if the gooflow is crossing the pipe for the second time
; cross again

(check-expect (cross-again? (list (list 2 2 "right")(list 2 3 "left")(list 1 2 "top")) 2 2 0) #f)
(check-expect (cross-again? (list (list 1 4 "left")(list 2 0 "bottom")(list 1 0 "right")) 3 1 0) #f)
(check-expect (cross-again? (list (list 2 2 "top")(list 2 3 "right")(list 1 2 "bottom")) 1 1 0) #f)

(define (cross-again? gf x y acc)
  (cond
    [(= acc 2) #true]
    [(empty? gf) #false]
    [(cons? gf)
     (if
      (boolean? (first gf))
      (cross-again? (rest gf) x y acc)
      (if
       (and (= (first (first gf)) x) (= (second (first gf)) y))
       (cross-again? (rest gf) x y (add1 acc))
       (cross-again? (rest gf) x y acc)))]))

;; grid-image: grid Number Number GooFlow -> image
(define (grid-image grid tile-size pipe-width goo-flow)
  ;; Make a tile
  ;; Note: tile size must be even number!
  (local
    [(define TILE (rectangle tile-size tile-size "outline" "blue"))
     (define dim (grid-dim grid))
     ;; create-row: num -> image
     ;; take a number n > 0 and returns a row of n tiles
     (define (create-row n)
       (if (= n 1)
           TILE
           (beside TILE (create-row (- n 1)))))
     ;; create-grid: row num-> image
     ;; takes a row image and duplicates it num times to create a grid
     ;; with rows and columns n > 0
     (define (create-grid row n_rows)
       (if (= n_rows 1)
           row
           (above row (create-grid row (- n_rows 1)))))
  
     (define grid-image
       (create-grid (create-row dim) dim))

     (define (place-pipe-on-grid grid-img x y pipe)
       (local
         [(define img-x (floor (* tile-size (+ x 1/2))))
          (define img-y (floor (* tile-size (+ y 1/2))))]
         (place-image
          (pipe-image pipe
                      tile-size
                      pipe-width
                      (my-member? (list x y)
                                  (map (lambda (elem) (list (first elem) (second elem))) 
                                       (if (and (not (empty? goo-flow))
                                                (false? (first goo-flow)))
                                           (rest goo-flow)
                                           goo-flow)))
                      goo-flow
                      x
                      y)
          img-x
          img-y 
          grid-img)))

     (define (draw-pipes cells grid-img-so-far)
       (cond
         [(empty? cells) grid-img-so-far]
         [else
          (draw-pipes (rest cells)
                      (place-pipe-on-grid grid-img-so-far
                                          (first (first cells))
                                          (second (first cells))
                                          (third (first cells))))]))]
    (draw-pipes (grid-cells grid) grid-image)))


;; A test for grid-image.
(grid-image (foldl
             (lambda
                 (pipe-locs pipe base)
               (place-pipe base
                           pipe
                           (first pipe-locs)
                           (second pipe-locs)))
             (make-grid GRID-DIM empty)
             (build-list (length ALL-PIPES)
                         (lambda (x) (list
                                      (remainder x GRID-DIM)
                                      (quotient x GRID-DIM))))
             ALL-PIPES)
            TILE-SIZE
            PIPE-WIDTH
            empty)

;; grid-goo-propagate: Grid GooFlow -> GooFlow
(check-expect (grid-goo-propagate (make-grid 2 empty) (list (list 2 2 "right")(list 2 3 "left")(list 1 2 "top")))(list (list 2 2 "right")(list 2 3 "left")(list 1 2 "top")))
(check-expect (grid-goo-propagate (make-grid 3 empty) (list (list 1 2 "right")(list 1 0 "left")(list 3 2 "top")))(list (list 1 2 "right")(list 1 0 "left")(list 3 2 "top")))
(check-expect (grid-goo-propagate (make-grid 6 empty) (list (list 0 0 "right")(list 4 3 "left")(list 3 2 "top")))(list (list 0 0 "right")(list 4 3 "left")(list 3 2 "top")))

(define (grid-goo-propagate grid goo-flow)
  (if (false? (first goo-flow))
      goo-flow
      (local
        [(define curr-goo-flow (first goo-flow))
         (define curr-dir (third curr-goo-flow))
         (define candidate-x
           (cond
             [(string=? curr-dir "left") (sub1 (first curr-goo-flow))]
             [(string=? curr-dir "right") (add1 (first curr-goo-flow))]
             [else (first curr-goo-flow)]))
         (define candidate-y
           (cond
             [(string=? curr-dir "top") (sub1 (second curr-goo-flow))]
             [(string=? curr-dir "bottom") (add1 (second curr-goo-flow))]
             [else (second curr-goo-flow)]))
         (define candidate-pipe (pipe-at grid candidate-x candidate-y))]
        (if (false? candidate-pipe)
           goo-flow
            (local
              ;; In the perspective of the pipe, a flow going top is comming from the bottom.
              ;; so we need to reverse the direction when checking if the goo can flow into the pipe. 
              [(define new-dir (pipe-can-flow candidate-pipe (reverse-direction curr-dir)))]
              (if (false? new-dir)
                  goo-flow
                  (cons (list candidate-x candidate-y new-dir) goo-flow)))))))


(define-struct gamestate [grid goo-flow incoming-pipes tile-size pipe-width replaced tick])
;; A gamestate is
;; (make-gamestate grid GooFlow (list-of pipe) Integer Integer Integer)
;; grid: the logical grid representation (i.e. the grid struct).
;; goo-flow: the locations of goo.
;; incoming-pipes: stack of next pipe styles to put down.
;; tile-size: size of the tile in the interface.
;; pipe-width: width of the pipe in the interface.

;; draw-grid: gamestate -> image
(define (draw-grid gs)
  (grid-image (gamestate-grid gs)
              (gamestate-tile-size gs)
              (gamestate-pipe-width gs)
              (gamestate-goo-flow gs)))

;; take-first-n-fill-blanks: (listof Any) Integer Any -> (listof Any)
;; take the first n elements of the list.
;; if list is empty, fill the blanks with the item.
(check-expect (take-first-n-fill-blanks '() 3 '()) (list '() '() '()))
(check-expect (take-first-n-fill-blanks '(2 3 4) 3 '()) (list 2 3 4))
(check-expect (take-first-n-fill-blanks '(4 3 2) 3 '(3)) (list 4 3 2))

(define (take-first-n-fill-blanks lst n item)
  (cond
    [(or (empty? lst) (= n 0)) (make-list n item)]
    [else (cons (first lst) (take-first-n-fill-blanks (rest lst) (sub1 n) item))]))
  
;; draw-interface: gamestate -> image
;; draw the interface of the game, including 
;; the grid, and on its right, the first (grid dim - 1) elements of pipe stack.
;; it also uses the get-score function to dispay the current score of the player
(define (draw-interface gs)
  (local
    [(define incoming-pipes (gamestate-incoming-pipes gs))
     (define pipe-width (gamestate-pipe-width gs))
     (define tile-size (gamestate-tile-size gs))
     (define incoming-pipes-img
       (above
        (text (string-append "SCORE\n" (number->string (get-score gs))) 40 "Midnight Blue")
        (foldr
         (lambda (pipe img)
           (above (pipe-image pipe tile-size pipe-width #false (gamestate-goo-flow gs)
                              (first (first (grid-cells (gamestate-grid gs))))
                              (second (first (grid-cells (gamestate-grid gs)))))
                  img))
         empty-image
         (take-first-n-fill-blanks incoming-pipes
                                   (- (grid-dim (gamestate-grid gs)) 1)
                                   ;; a special pipe with no opening is needed to draw an empty tile
                                   ;; with just the pipe background
                                   (make-pipe #false #false #false #false)))))]
    (beside (draw-grid gs) incoming-pipes-img)))

;; gamestate-propagate-goo: gamestate -> gamestate 
(define (gamestate-propagate-goo gs)
  (make-gamestate (gamestate-grid gs)
                  (grid-goo-propagate
                   (gamestate-grid gs)
                   (gamestate-goo-flow gs))
                  (gamestate-incoming-pipes gs)
                  (gamestate-tile-size gs)
                  (gamestate-pipe-width gs)
                  (gamestate-replaced gs) (gamestate-tick gs)))

;; image-coor->grid-coor: Integer Integer -> Integer
;; produce the grid coordiate for the component, given the image coordiate and tile-size
(define (image-coor->grid-coor x-image tile-size)
  (quotient x-image tile-size))
 
;; gamestate-place-pipe-on-click: gamestate int int event -> gamestate
;; handler for mouse event. Takes gs and mouse (i.e. image) coordinates (x, y)
;; and places whatever pipe is currently on the the pipe stack correctly onto
;; grid at mouse point. Updates gamestate accordingly.
;; and prevents the player from replacing pipes that already have goo in them
(define (gamestate-place-pipe-on-click gs x y event)
  (local
    [(define incoming-pipes (gamestate-incoming-pipes gs))
     (define griddy (gamestate-grid gs))
     (define tile-size (gamestate-tile-size gs))
     (define grid-x (image-coor->grid-coor x tile-size))
     (define grid-y (image-coor->grid-coor y tile-size))
     (define dim (grid-dim griddy))]
    (cond
      [(and (string=? "button-down" event)
            (not (empty? incoming-pipes))
            (< grid-x dim)
            (< grid-y dim) 
            (empty?
             (filter (lambda (goo)
                       (and (cons? goo) (and (= grid-x (list-ref goo 0)) (= grid-y (list-ref goo 1))))) (gamestate-goo-flow gs) )))
                      
       (make-gamestate (place-pipe griddy
                                   (first incoming-pipes)
                                   grid-x
                                   grid-y)
                       (gamestate-goo-flow gs)
                       (rest incoming-pipes)
                       (gamestate-tile-size gs)
                       (gamestate-pipe-width gs)
                       ( + (gamestate-replaced gs) (if (pipe? (pipe-at griddy grid-x grid-y)) 1 0))
                       (gamestate-tick gs))]
      ;; incoming-pipes is empty. one click will propagate a tile of goo.
      
      [else gs]))) 

; get-score gamestate -> integer
; get score is used to produce the amount of points the player gets after placing pipes and letting the goo flow through
(check-expect (get-score (gamestate-init GRID-DIM 1 1 "right" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))0)
(check-expect (get-score (gamestate-init GRID-DIM 1 0 "left" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))0)
(check-expect (get-score (gamestate-init GRID-DIM 3 2 "top" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))0)

 (define (get-score gamestate)
         (local
           [(define length-path (- (length (gamestate-goo-flow gamestate)) 1))]
           (* 50 (- length-path (gamestate-replaced gamestate)))))

;; gamestate-init : Integer Integer direction (any-of Integer pipes) -> gamestate
;; initialize a game state given:
;; grid-dim: the dimension of the square grid
;; start-x, start-y: the starting location for the starting pipe
;; start-dir: the starting direction for the starting pipe
;; tile-size: the size of the tile in the interface
;; pipe-width: the width of the pipe in the interface
;; incoming-pipes: the list of incoming pipes
; gamestate-init

(define (gamestate-init grid-dim start-x start-y start-dir tile-size pipe-width incoming-pipes)
  (local
    [(define start-pipe 
       (make-pipe (string=? start-dir "top")
                  (string=? start-dir "bottom")
                  (string=? start-dir "left")
                  (string=? start-dir "right")))
     (define start-grid (place-pipe (make-grid grid-dim empty)
                                    start-pipe
                                    start-x
                                    start-y))]
    (make-gamestate start-grid
                    (list (list start-x start-y start-dir))
                    incoming-pipes
                    tile-size
                    pipe-width 0 140)))

(define TRIPLE-PASS-THROUGH-PIPES
  (append PASS-THROUGH-PIPES PASS-THROUGH-PIPES PASS-THROUGH-PIPES))

(define gs-init1 (gamestate-init GRID-DIM 1 1 "right" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))
(define gs-init2 (gamestate-init GRID-DIM 3 2 "left" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))
(define gs-init3 (gamestate-init GRID-DIM 0 4 "top" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))
; next-goo: gamestate-> gamestate
; if the time is under the 28 ticks, and the 140 ticks has passed, subtracts the tick by 1 every time
; takes in a game state and checks if the time is under 0, propegates the goo and resets the times to 28

(check-expect (get-score (gamestate-init GRID-DIM 3 2 "top" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))0)
(check-expect (get-score (gamestate-init GRID-DIM 0 1 "right" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))0)
(check-expect (get-score (gamestate-init GRID-DIM 1 1 "left" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))0)

(check-expect (next-goo (gamestate-init GRID-DIM 1 1 "right" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))(make-gamestate
 (make-grid
  6
  (list
   (list
    1
    1
    (make-pipe
     #false
     #false
     #false
     #true))))
 (list (list 1 1 "right"))
 (list
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 100
 30
 0
 139))
(check-expect (next-goo (gamestate-init GRID-DIM 3 2 "left" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))(make-gamestate
 (make-grid
  6
  (list
   (list
    3
    2
    (make-pipe
     #false
     #false
     #true
     #false))))
 (list (list 3 2 "left"))
 (list
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 100
 30
 0
 139))
(check-expect (next-goo (gamestate-init GRID-DIM 4 0 "right" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))(make-gamestate
 (make-grid
  6
  (list
   (list
    4
    0
    (make-pipe
     #false
     #false
     #false
     #true))))
 (list (list 4 0 "right"))
 (list
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 100
 30
 0
 139))

(define (next-goo gs)
  (cond
    [( = (gamestate-tick gs) 0) (make-gamestate (gamestate-grid gs)
                  (grid-goo-propagate
                   (gamestate-grid gs)
                   (gamestate-goo-flow gs))
                  (gamestate-incoming-pipes gs)
                  (gamestate-tile-size gs)
                  (gamestate-pipe-width gs)
                  (gamestate-replaced gs) 28)]
    [else 
       (make-gamestate (gamestate-grid gs)                  
                   (gamestate-goo-flow gs)
                  (gamestate-incoming-pipes gs)
                  (gamestate-tile-size gs)
                  (gamestate-pipe-width gs)
                  (gamestate-replaced gs) (- (gamestate-tick gs) 1))]))

(define game-test (make-gamestate
 (make-grid 6
  (list(list 1 1 (make-pipe #false #false #false #true))(list 1 2 (make-pipe #false #true #false #true))(list 1 3 (make-pipe #true #true #true #true))(list 1 4 (make-pipe #true #false #false #true))(list 2 1 (make-pipe #false #true #true #false))(list 2 2 (make-pipe #true #true #true #true))(list 2 3 (make-pipe #true #true #false #false))(list 2 4 (make-pipe #true #false #true #false))(list 3 2 (make-pipe #false #false #true #true))(list 4 2 (make-pipe #true #false #true #false))))
 (list (list 1 1 "right"))
 (list (make-pipe #false #true #false #true) (make-pipe #true #true #false #false) (make-pipe #false #false #true #true) (make-pipe #true #true #true #true) (make-pipe #true #false #true #false)) 100 30 0 140))
  
; pipe-fantasy: gamestate-> image
(define (pipe-fantasy gs)
  (big-bang gs
    [to-draw draw-interface]
    [on-mouse gamestate-place-pipe-on-click]
    [on-tick next-goo]))


;(pipe-fantasy (gamestate-init GRID-DIM 1 1 "right" TILE-SIZE PIPE-WIDTH TRIPLE-PASS-THROUGH-PIPES))