(write "load list library...")

(define list-module
  (module
   (define (caar x)        (car (car x)))
   (define (cadr x)        (car (cdr x)))
   (define (cdar x)        (cdr (car x)))
   (define (cddr x)        (cdr (cdr x)))
   (define (caaar x)       (car (car (car x))))
   (define (caadr x)       (car (car (cdr x))))
   (define (cadar x)       (car (cdr (car x))))
   (define (caddr x)       (car (cdr (cdr x))))
   (define (cdaar x)       (cdr (car (car x))))
   (define (cdadr x)       (cdr (car (cdr x))))
   (define (cddar x)       (cdr (cdr (car x))))
   (define (cdddr x)       (cdr (cdr (cdr x))))
   (define (caaaar x)      (car (car (car (car x)))))
   (define (caaadr x)      (car (car (car (cdr x)))))
   (define (caadar x)      (car (car (cdr (car x)))))
   (define (caaddr x)      (car (car (cdr (cdr x)))))
   (define (cadaar x)      (car (cdr (car (car x)))))
   (define (cadadr x)      (car (cdr (car (cdr x)))))
   (define (caddar x)      (car (cdr (cdr (car x)))))
   (define (cadddr x)      (car (cdr (cdr (cdr x)))))
   (define (cdaaar x)      (cdr (car (car (car x)))))
   (define (cdaadr x)      (cdr (car (car (cdr x)))))
   (define (cdadar x)      (cdr (car (cdr (car x)))))
   (define (cdaddr x)      (cdr (car (cdr (cdr x)))))
   (define (cddaar x)      (cdr (cdr (car (car x)))))
   (define (cddadr x)      (cdr (cdr (car (cdr x)))))
   (define (cdddar x)      (cdr (cdr (cdr (car x)))))
   (define (cddddr x)      (cdr (cdr (cdr (cdr x)))))
   (define (length ls)
     (if (null? ls)
	 0
	 (+ 1 (length (cdr ls)))))
   (define (last ls)
     (if (null? (cdr ls))
	 (car ls)
	 (last (cdr ls))))
   (define (append xs ys)
     (if (null? xs)
	 ys
	 (cons (car xs) (append (cdr xs) ys))))
   (define (reverse ls)
     (reverse-iter ls '()))
   (define (reverse-iter ls a)
     (if (null? ls)
	 a
	 (reversei (cdr ls) (cons (car ls) a))))
   (define (list-ref ls k)
     (if (= k 0)
	 (car ls)
	 (list-ref (cdr ls) (- k 1))))
   (define-macro (associate-expander name compare-function)
     `(lambda (assq o alist)
	(if (null? alist)
	    #f
	    (if (,compare-function o (caar alist))
		(car alist)
		(,name o (cdr alist))))))
   (define assoc (associate-expander assoc equal?))
   (define accv (associate-expander assv eqv?))
   (define assq (associate-expander assq eq?))
   
   (export
     length last append reverse
     list-ref
     caar cadr cdar cddr
     caaar caadr cadar caddr cdaar cdadr cddar cdddr
     caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)))

(write "[complete]")
(write "load num library...")


(define num-module
  (module
   (define (min . args)
     (min-iter (car args) (cdr args)))
   (define (min-iter v ls)
     (if (null? ls) v (min-iter (if (< (car ls) v) (car ls) v) (cdr ls))))
   (define (max . args)
     (max-iter (car args) (cdr args)))
   (define (max-iter v ls)
     (if (null? ls) v (max-iter (if (> (car ls) v) (car ls) v) (cdr ls))))
   (define (abs n)
     (if (> n 0) n (* n (- 0 1))))
   (define (zero? n) (= n 0))
   (define (positive? n) (> n 0))
   (define (negative? n) (< n 0))
   (define (even? x) (if (= x 0) #t (odd?  (- x 1))))
   (define (odd?  x) (if (= x 0) #f (even? (- x 1))))
   
   (export
      min max
      abs
      zero? positive? negative?
      even? odd?)))

(write "[complete]")
