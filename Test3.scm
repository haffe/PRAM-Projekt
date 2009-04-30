(load "object-system.scm")
(require (file "othello-ui.scm"))

;Support procedures 
(define (valid-move? coordinates)
  #t)
(define (get-x move)
  (car move))
(define (get-y move)
  (cdr move))

(define (make-coordinates x-cord y-cord)
  (cons x-cord y-cord))


(define (current-player-ai-player?) 
  (eq? (ask player-states-object 'current-player) (ask player-states-object 'ai-player)))

(define (can-current-player-make-move?)
  (not (null? (generate-valid-moves))))

(define (generate-valid-moves)
  (cons 'blaha 'blaha))

(define (non-null-cons results result)
  (if (not (null? result))
      (cons result results)
      results))

(define (is-final-square? coordinates)
  (and (eq? (get-x coordinates) (ask board-object 'get-edges 'max-x))
       (eq? (get-y coordinates) (ask board-object 'get-edges 'max-y))))

(define (board-iterator coordinates filter inc results)
  (cond 
    ((eq? (filter coordinates) 'halt) results)
    ((ask board-object 'out-of-board?  (inc coordinates)) (non-null-cons results 
                                                      (filter coordinates)))
    (else (board-iterator 
           (inc coordinates) filter inc 
           (non-null-cons results (filter coordinates))))))







;Objects relevant to game 

(define board-object
  (let ((min-x 0) (max-x 7) (min-y 0) (max-y 7)) 
    (define (self message)
      (cond 
        ((eq? message 'show-board) (lambda (self) (get-board)))
        ((eq? message 'get-piece-at) (lambda (self coordinates)
                                       (vector-ref (get-board) (+ (get-x coordinates) (* (+ 1 max-y) (get-y coordinates))))))
        ((eq? message 'board-iterate) (lambda (self message coordinates filter)
                                        (cond
                                          ((eq? message 'up) 
                                           (board-iterator coordinates filter
                                                           (lambda (x-cord y-cord)
                                                             (make-coordinates  x-cord (- y-cord 1))) 
                                                           '()))
                                          ((eq? message 'up-right)
                                           (board-iterator coordinates filter 
                                                           (lambda (x-cord y-cord)
                                                             (make-coordinates  (+ x-cord 1) (- y-cord 1))) 
                                                           '()))
                                          ((eq? message 'right)
                                           (board-iterator coordinates filter
                                                           (lambda (x-cord y-cord)
                                                             (make-coordinates   (+ 1 x-cord) y-cord)) 
                                                           '()))
                                          ((eq? message 'down-right)
                                           (board-iterator coordinates filter
                                                           (lambda (x-cord y-cord)
                                                             (make-coordinates  (+ 1 x-cord) (+ 1 y-cord)))
                                                           '()))
                                          ((eq? message 'down)
                                           (board-iterator coordinates filter 
                                             (lambda (x-cord y-cord)
                                                             (make-coordinates   x-cord (+ 1 y-cord)))
                                                           '()))
                                          ((eq? message 'down-left)
                                           (board-iterator coordinates filter
                                                           (lambda (x-cord y-cord)
                                                             (make-coordinates   (- x-cord 1) (+ 1 y-cord)))
                                                           '()))
                                          ((eq? message 'left)
                                           (board-iterator coordinates filter 
                                                           (lambda (x-cord y-cord)
                                                             (make-coordinates  (- x-cord 1) y-cord))
                                                           '()))
                                          ((eq? message 'up-left)
                                           (board-iterator coordinates filter
                                                           (lambda (x-cord y-cord)
                                                             (make-coordinates   (- x-cord 1) (- y-cord 1)))
                                                           '()))
                                          ((eq? message 'whole-board)
                                           (board-iterator coordinates filter
                                                           (lambda (coordinates)
                                                             (cond 
                                                               ((ask self 'is-edge? coordinates)
                                                                (if (is-final-square? coordinates)
                                                                    (make-coordinates  (+ 1 (get-x coordinates)) (+ 1 (get-y coordinates)))
                                                                    (begin
                                                                      (make-coordinates  (+ 1 (get-x coordinates)) 
                                                                            (ask board-object 'get-edges 'min-y)))))
                                                               (else (make-coordinates  (get-x coordinates) 
                                                                           (+ 1 (get-y coordinates))))))  
                                                           '())))))
        ((eq? message 'get-edges) (lambda (self coordinate)
                                    (cond 
                                      ((eq? coordinate 'min-x) min-x)
                                      ((eq? coordinate 'max-x) max-x)
                                      ((eq? coordinate 'min-y) min-y)
                                      ((eq? coordinate 'max-y) max-y))))
        ((eq? message 'out-of-board?) (lambda (self coordinates)
                                       (or 
                                        (or (< (get-x coordinates) min-x) (> (get-x coordinates) max-x))
                                        (or (< (get-y coordinates) min-y) (> (get-y coordinates) max-y)))))
        ((eq? message 'is-edge?)      (lambda (self coordinates)
                                        (eq? (get-y coordinates) max-y)))))
                                      
        self))
    
    (define player-states-object
      (let ((current-player 'BLACK) (players (vector 'BLACK 'WHITE)) (ai-player #f))
        (define (self message)
          (cond ((eq? message 'change-player!) (lambda (self) (if (eq? (vector-ref players 0) current-player) 
                                                                  (set! current-player (vector-ref players 1))
                                                                  (set! current-player (vector-ref players 0)))))
                ((eq? message 'current-player) (lambda (self) current-player))
                ((eq? message 'ai-player) (lambda (self) ai-player))))
        self)) 
    
    ;Game loop    
    
    (define (play-game)
      (define (get-move-loop)
        (let ((move #f)) 
          (cond ((can-current-player-make-move?) 
                 (if (current-player-ai-player?)
                     (super-unbeatable-algorithm)
                     (begin
                       (ask board-object 'board-iterate 'whole-board (make-coordinates  0 0)
                            (lambda (coordinates)
                              (if (eq? (ask board-object 'get-piece-at coordinates)
                                       (ask player-states-object 'current-player))
                                  coordinates
                                  null)))
                       (set! move (get-next-move))  
                       (if (valid-move? (get-x move) (get-y move)) 
                           (begin (set-piece-at! (get-x move) (get-y move)  
                                                 (ask player-states-object 'current-player))
                                  (ask player-states-object 'change-player!))
                           (get-move-loop)))))
                (else (ask player-states-object 'change-player!)
                      (get-move-loop)))))
      (show-board)
      (clear-board!)
      (set-piece-at! 4 4 'WHITE)
      (set-piece-at! 3 3 'WHTIE)
      (set-piece-at! 3 4 'BLACK)
      (set-piece-at! 4 3 'BLACK)
      (set-piece-at! 1 1 'WHITE)
      (set-piece-at! 0 1 'BLACK)  
      (show-board)
      (get-move-loop))
    
    