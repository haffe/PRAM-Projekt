;; Created:         021112
;; Last modified::  2004-11-15,  10:53:11
;; Created by:      Tord Svensson (torsv@ida.liu.se)

(module
 othello-ui
 mzscheme

 (require (lib "class.ss" "mzlib")
          (lib "list.ss" "mzlib")
          (lib "mred.ss" "mred"))

 (provide get-next-move
          abort-get-next-move
          set-highlights!
          set-quit-fn!
          set-restart-fn!
          set-title!
          show-board
          hide-board
          clear-board!
          set-piece-at!
          add-listener!
          del-listener!)

 ;; Exported procedures -----------------------------------------------

 ;;  set-title!: string -> void
 ;;  Sets the title on the frame to new-title.
 (define (set-title! new-title)
   (send win set-label new-title))

;; set-highlights! (list (cons integer integer)) -> void
;; Takes a list of cons pairs containing positions on the board
;; and draws them in a slightly lighter color. This can be used
;; to show the possible moves to the user. If it is called with
;; the empty list no positions are highlighted.
 (define (set-highlights! x)
   (set! highlights x)
   (refresh))

 ;; show-board: void -> void
 ;; Showes the frame containing the board.
 (define (show-board)
   (send win show #t))

 ;; hide-board: void -> void
 ;; Hides the frame containing the board.
 (define (hide-board)
   (send win show #f))

 ;; clear-board!: void -> void
 ;; Removes all the pieces from the board.
 (define (clear-board!)
   (vector-fill! board 'NONE)
   (set! highlights '())
   (refresh))

 ;; set-piece-at!: integer x integer x symbol -> void
 ;; Sets the piece at the position x y to piece.
 ;; Valid symbols for piece are: BLACK, WHITE or NONE.
 (define (set-piece-at! x y piece)
   (vector-set! board (+ (* y 8) x) piece)
   (refresh))


 ;; set-quit-fn!: (void -> void) -> void
 ;; Sets the callback function to be called when the
 ;; quit button is pressed.
 (define (set-quit-fn! fn)
   (set! quit-fn fn))

 ;; set-restart-fn!: (void -> void) -> void
 ;; Sets the callback function to be called when the
 ;; restart button is pressed.
 (define (set-restart-fn! fn)
   (set! restart-fn fn))

 ;; get-next-move: void -> (cons integer integer)
 ;; Returns the next move of the user (i.e. the square
 ;; that the user clicked on. The returned pair contains
 ;; the x coordinate in the car position and the y coordinate
 ;; in the cdr position.
 ;; Note, this functions blocks until the user has performed
 ;; a move. 
 (define (get-next-move)
   (add-listener! move-callback)
   (set! is-getting-next-move? #t)
   (semaphore-wait synch)
   (set! is-getting-next-move? #f)
   (del-listener! move-callback)
   last-move)

 ;; abort-get-next-move: void -> void
 ;; Aborts the call to get-next-move so that
 ;; it returns immediately with the symbol
 ;; aborted.
(define (abort-get-next-move)
  ;; we only abort if get-next-move has
  ;; been called.
  (when is-getting-next-move?
        (set! last-move 'aborted)
        (semaphore-post synch)))



 ;; add-listener!: (integer x integer -> void) -> void
 ;; Takes a callback function that will be called when
 ;; the user clicks on a board-position and adds it to
 ;; the list of callbacks.
 (define (add-listener! listener)
   (set! listeners (cons listener listeners)))

 ;; del-listener!: (integer x integer -> void) -> void
 ;; Takes a callback function and removes if from the list
 ;; of callbacks.
 (define (del-listener! listener)
   (set! listeners (remove listener listeners)))


 ;; General utilities -------------------------------------------------

 (define (repeat n f)
   (when (not (= n 0))
         (f n)
         (repeat (- n 1) f)))
 
 (define (do-vector v f)
   (define len (vector-length v))
   (let loop ((n 0))
     (when (< n len)
           (f n (vector-ref v n))
           (loop (+ n 1)))))
  
(define (get-board) ;;Own Addition
  board) 
  
 ;; Graphical utilities -------------------------------------------------
 
 (define glue%  
   (class panel%
          (super-instantiate () 
                             (stretchable-width #t)
                             (stretchable-height #t))))

 (define (get-pen color width style)
   (send the-pen-list find-or-create-pen color width style))
 
 (define (get-brush color style)
   (send the-brush-list find-or-create-brush color style))

 (define (set-color! dc brush pen)
   (send dc set-brush brush)
   (send dc set-pen pen))


;; Board management ----------------------------------------------------

 (define board (make-vector 64 'NONE))

 (define (get-piece x y)
   (vector-ref board (+ (* y 8) x)))
 

;; Callback management --------------------------------------------------

 (define listeners '())
 (define quit-fn (lambda () #f))
 (define restart-fn (lambda () #f))
 
 (define (propagate x y)
   (set! x (inexact->exact (floor (/ x tile-size))))
   (set! y (inexact->exact (floor (/ y tile-size))))
   (when (and (>= x 0) (>= y 0)
              (< x 8) (< y 8))
         (for-each (lambda (f)
                     (f x y)) listeners)))


;; Moves ----------------------------------------------------------------

 (define synch (make-semaphore))
 (define is-getting-next-move? #f)
 (define last-move #f)
 (define (move-callback x y)
   (set! last-move (cons x y))
   (semaphore-post synch))
 

;; Colors, pens and brushes --------------------------------------------
 
 (define highlight-color  (instantiate color% (50 140 60)))
 (define board-color      (instantiate color% (30 120 40)))
 (define background-color (instantiate color% (20 70 30)))
 (define black-piece-pen   (get-pen   "BLACK"               1 'solid))
 (define black-piece-brush (get-brush "BLACK"                 'solid))
 (define white-piece-pen   (get-pen   "WHITE"               1 'solid))
 (define white-piece-brush (get-brush "WHITE"                 'solid))
 (define highlight-pen     (get-pen   highlight-color       1 'solid))
 (define highlight-brush   (get-brush highlight-color         'solid))
 (define grid-pen          (get-pen   "BLACK"               1 'solid))
 (define grid-brush        (get-brush "BLACK"                 'solid))
 (define background-pen    (get-pen   background-color      1 'solid))
 (define background-brush  (get-brush background-color        'solid))
 (define board-pen         (get-pen   board-color           1 'solid))
 (define board-brush       (get-brush board-color             'solid))

;; Graphics -------------------------------------------------------------

 (define get-x car)
 (define get-y cdr)
 (define preferred-tile-size 32)
 (define tile-size preferred-tile-size)
 (define scale-factor 0.8)
 (define offset 0)
 (define piece-size (* scale-factor preferred-tile-size))
 (define highlights '())
 
;; A double-buffered board-view.
 (define board-view% 
   (class canvas%
          (override on-size 
                    on-paint 
                    on-event
                    on-superwindow-show
                    on-focus)
          
          (init-field parent)
          (init-field (paint-callback #f))
          (init-field (invalidated-buffer #f))
          (init-field (lock (make-semaphore 1)))

          (define gbuffer #f)
          (define gbuffer-dc #f)

          (define (make-new-bitmap w h bitmap)
            (let ((res 
                   (if (and bitmap (send bitmap ok?))
                       bitmap
                       (make-new-bitmap w h (make-object bitmap% w h #f)))))
              bitmap))
          
          (define (on-paint)
            (define dc (send this get-dc))
            (semaphore-wait lock)
            (when (not gbuffer)
                  (call-with-values
                      (lambda () (send dc get-size))
                    (lambda (w h)
                      (set! gbuffer 
                            (make-new-bitmap (inexact->exact w)
                                             (inexact->exact h)
                                             (make-object bitmap% 
                                                          (inexact->exact w)
                                                          (inexact->exact h)
                                                          #f)))
                      (set! gbuffer-dc (instantiate bitmap-dc% (gbuffer)))
                      (when invalidated-buffer
                            (invalidated-buffer)))))

            (send gbuffer-dc set-origin 0 0)
            (draw-board gbuffer-dc)
            
            (send dc draw-bitmap gbuffer 0 0)
            (semaphore-post lock))
          
          (define (on-superwindow-show shown?)
            (when shown?
                  (semaphore-wait lock)
                  (set! gbuffer #f)
                  (semaphore-post lock)))
          
          (define (on-size w h)
            (semaphore-wait lock)
            (set! gbuffer #f)
            (semaphore-post lock))
          
          (define (on-event event)
            (when (send event button-up?)
                  (propagate (send event get-x) (send event get-y))))

          (define (on-focus x)
            (on-paint))
          (super-instantiate (parent))))


 (define (draw-highlights dc)
   (set-color! dc highlight-brush highlight-pen)
   (for-each (lambda (coord)
               (send dc draw-rectangle
                     (* tile-size (get-x coord))
                     (* tile-size (get-y coord))
                     tile-size
                     tile-size))
             highlights))
 
 (define (draw-grid dc)
   (set-color! dc grid-brush grid-pen)
   (repeat 8 (lambda (n)
              (send dc draw-line (* n tile-size) 0 
                    (* n tile-size) (* tile-size 8))
              (send dc draw-line 0 (* n tile-size) (* tile-size 8) 
                    (* n tile-size)))))
 
 (define (draw-pieces dc)
   (do-vector board 
              (lambda (index element)
                (when (not (eq? element 'NONE))
                      (if (eq? element 'BLACK)
                          (set-color! dc 
                                      black-piece-brush
                                      black-piece-pen)
                          (set-color! dc 
                                      white-piece-brush
                                      white-piece-pen))
                      (send dc draw-ellipse 
                            (+ offset (* tile-size
                                         (remainder index 8)))
                            (+ offset (* tile-size
                                         (floor (/ index 8))))
                            piece-size piece-size)))))

 (define (draw-board-background dc)
   (set-color! dc board-brush board-pen)
   (send dc draw-rectangle 0 0 (* tile-size 8) (* tile-size 8)))
 
 (define (draw-board dc)
   ;; recalculate sizes and clear the buffer
   (call-with-values 
       (lambda () (send dc get-size))
     (lambda (width height)
       (set! tile-size (/ (- (min width height) 7) 8))
       (set! piece-size (* tile-size scale-factor))
       (set! offset (+ 1 (inexact->exact 
                          (floor (/ (- tile-size piece-size) 2)))))
       (set-color! dc background-brush background-pen)
       (send dc draw-rectangle 0 0 width height)))

   (draw-board-background dc)
   (draw-highlights dc)
   (draw-grid dc)
   (draw-pieces dc))


 ;; Build the user interface -------------------------------------------
 (define win (let ((new-es (make-eventspace)))
               (parameterize ((current-eventspace new-es))
                             (instantiate frame% ("Othello")))))

 (define main-panel (instantiate horizontal-panel% (win)
                                 (alignment '(center top))))

 (define control-panel (instantiate vertical-panel% (main-panel)
                                    (alignment '(center top)) 
                                    (stretchable-width #f)
                                    (stretchable-height #t)))

 (define canvas (instantiate board-view% (main-panel)))

 (define (refresh)
   (send canvas on-paint))
 
 (send canvas min-height (+ 8 (* preferred-tile-size 8)))
 (send canvas min-width (+ 8 (* preferred-tile-size 8)))
 (instantiate glue% (control-panel))
 
 (instantiate button% 
              ("Restart" control-panel (lambda (e b) (restart-fn)))
              (horiz-margin 2)
              (vert-margin 2)
              (stretchable-width #t))
 
 (instantiate button% 
              ("Quit" control-panel (lambda (e b) (quit-fn)))
              (horiz-margin 2)
              (vert-margin 2)
              (stretchable-width #t))
 )




