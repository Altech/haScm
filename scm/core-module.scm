(write "load core library...")

(define core-module
  (module
   
   ;; macroexpand
   (define (macroexpand form)
     (macroexpand-iter form '()))
   (define (macroexpand-iter form prev)
     (if (equal? form prev)
	 form
	 (macroexpand-iter (macroexpand-1 form) form)))

   (define (bindings-symbols alist)
     (map car alist))

   (define (bindings-values alist)
     (map cadr alist))

   ;; let
   (define-macro (let bindings . body)
     `((lambda ,(bindings-symbols bindings) ,@body) ,@(bindings-values bindings)))   

   ;; let*
   (define-macro (let* bindings . body)
     (let*-expander bindings body))
   (define (let*-expander bindings body)
     (if (null? bindings)
	 `(begin ,@body)
	 `(let (,(car bindings)) ,(let*-expander (cdr bindings) body))))

   ;; letrec
   (define-macro (letrec bindings . body)
     `((lambda ,(bindings-symbols bindings)
	 ,@(map (lambda (pair) `(set! ,(car pair) ,(cadr pair))) bindings)
	 ,@body) ,@(replicate (length bindings) ''dummy)))

   ;; cond
   (define-macro (cond . conds)
     (conds-expander conds))
   (define (conds-expander conds)
     (if (null? conds)
	 #f
	 `(if ,(caar conds)
	      (begin ,@(cdar conds))
	      ,(conds-expander (cdr conds)))))
   (define else #t)

   ;; or
   (define-macro (and . values)
     (and-expander values))
   (define (and-expander values)
     (if (null? (cdr values))
	 (car values)
	 `(if ,(car values)
	      ,(and-expander (cdr values))
	      #f)))

   ;; and
   (define-macro (or . values)
     (or-expander values))
   (define (or-expander values)
     (if (null? (cdr values))
	 (car values)
	 `(if ,(car values)
	      #t
	      ,(or-expander (cdr values)))))

   ;; list, set-car!, set-cdr!, map, for-each
   (define (list h . t) `(,h ,@t))
   (define-macro (set-car! s o)
     `(set! ,s (cons ,o (cdr ,s))))
   (define-macro (set-cdr! s o)
     `(set! ,s (cons (car ,s) ,o)))
   (define (map f ls)
     (if (null? ls)
	 '()
	 (cons (f (car ls)) (map f (cdr ls)))))
   (define (for-each f ls)
     (if (null? ls)
	 '()
	 (begin (f (car ls)) (for-each f (cdr ls)))))
   
   ;; member, memv, memq
   (define-macro (mem-expander name compare-function)
     `(lambda (o ls)
	(if (null? ls)
	    #f
	    (if (,compare-function o (car ls))
		ls
		(,name o (cdr ls))))))
   (define member (mem-expander member equal?))
   (define memv (mem-expander memv eqv?))
   (define memq (mem-expander memq eq?))

   ;; sub-routines
   (define (length ls)
     (if (null? ls)
	 0
	 (+ 1 (length (cdr ls)))))
   (define (replicate n v)
     (if (eq? n 0)
	 '()
	 (cons v (replicate (- n 1) v))))
   (define (caar x) (car (car x)))
   (define (cadr x) (car (cdr x)))
   (define (cdar x) (cdr (car x)))
   (define (cddr x) (cdr (cdr x)))
   
   (export
     macroexpand
     let let* letrec
     cond or and
     member memv memq
     list set-car! set-cdr! map for-each)))

(write "[complete]")
