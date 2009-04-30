
;;; A SMALL OBJECT SYSTEM - for easy self reference and argument passing.

;;; See the example in the end of this file to see how you can use the
;;; object system.

;; ask asks an object to perform the method associated with message.
;; A method should take the argument 'self' and an optional number of arguments.
;; See the implementation of make-object.
(define (ask object message . arguments)
  
  ;; returns the method associated with a message
  (define (get-method object message)
    ; object -> method (or whatever object returns)
    (object message))
  
  ;; tests whether the argument is a method
  (define (method? object)
    ; object -> #t/#f
    (procedure? object))
  
  (let ((method (get-method object message))
        ; add 'self, that is, the object itself, to the arguments.
        (method-arguments (cons object arguments)))
    (if (method? method)
        (apply method method-arguments) 
        (error "No such method:" message))))

; asks an object if it answers to a specific message.
(define (has-method? object message)
  ; object x message -> #t/#f
  (if (object message) #t #f))

;; make-object creates an object of the argument type.
;; Primarily this is just a a template for your own object creators,
;; but can be used as the root of an object hierarchy.
(define (make-object type)
  (lambda (message)
    (cond
      ((eq? message 'type)
       (lambda (self) type))
      ;; asks another object for its type and compares
      ((eq? message 'is-this-type?)
       (lambda (self which) 
         (eq? type (ask which 'type))))
      (else #f)))) ; unknown message

;;; ------------------------------------------------------------
;;; EXAMPLES AND COMMENTS
;;; ------------------------------------------------------------

;;; All your method need 'self' as their first argument, because that is
;;; how ask is implemented. So even if you don't use self in your procedures
;;; it still need to be an argument.

;;; What do we need self for? Because sometimes it is useful for an object
;;; to be able to reference itself. For example it might want to send a 
;;; reference of itself to another object which in turn changes uses it in 
;;; some way. 

;;; The idea of object inheritance/hierarchy is as follows:
;;; If a child-object can not answer a message, it sends the message (delegates)
;;; to its parent. 

;;; Below is a rather trivial example but hopefully demonstrates how to 
;;; implement new objects and the idea of inheritance (by using make-object as parent).  

;;; Ask your lab assistant if you want to know more or need better explanations.

;;; This is just an example of how you can implement your own objects.
;;; Uncomment and run if you want to play with it.
;(define (make-child-object name)
;  ;; We define child-object as a child to make-object.
;  (let ((parent (make-object 'example-child)))
;    
;    (define (greeting self)
;      (display "Hello! My name is ")
;      (display name)
;      (display "!\n"))
;    
;    (lambda (message)
;      (cond
;        ((eq? message 'name) greeting)
;        ((eq? message 'sum-of-2)
;         (lambda (self num1 num2)
;           (display num1) (display " and ")
;           (display num2) (display " is ")
;           (display (+ num1 num2)) (display "!\n")))
;        (else
;         ;; Could not answer the message - send it to my parent.
;         (parent message))))))
;
;;; Examples 
;(define *child* (make-child-object 'tweety)) ; -> <void>
;(ask *child* 'name) ; prints "Hello! My name is tweety!"
;(ask *child* 'sum-of-2 1 2) ; prints "1 and 2 is 3!"
;(ask *child* 'type) ; -> example-child
;(ask *child* 'is-this-type? *child*) ; -> #t