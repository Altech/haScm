(display "load presentation library...")

(define presentation-module  
  (module
   ;; (define next-page (let ((page 0))
   ;; 		       (define (next-page slides)
   ;; 			 (if (< page (length page))
   ;; 			     (begin
   ;; 			       (display (encode (list-ref slides page) ""))
   ;; 			       (set! page (+ 1 page)))
   ;; 			     (write "Thank you.")))
   ;; 		       next-page))
   
   ;; (define-all (let ((page (- 0 1)))
   ;; 		       (list (cons 'reset-page reset-page))))

   (define page (- 0 1))

   (define slides '())
   (define (set-slides s) (set! slides s) (undefined))

   (define (next-page)
     (if (< (+ page 1) (length slides))
	 (begin
	   (set! page (+ page 1))
	   (display (string-append "\n" (encode (list-ref slides page))))
	   (undefined))
	 (begin (write "Thank you.") (undefined))))
   
   (define (prev-page)
     (if (< 0 page)
	 (begin
	   (set! page (- page 1))
	   (display (encode (list-ref slides page)))
	   (undefined))
	 (begin (write "Thank you.") (undefined))))
   (define (reset-page) (set! page (- 0 1)) (undefined))


   (define (goto-page n) (set! page (+ 1 n)))
   
   (define (encode form)
     (cond
      ((string? form) form)
      ((pair? form)
       (if (eq? 'make-list (car form))
	   (apply string-append
		  (map
		   (lambda (x)
		     (string-append "  " (encode x) "\n\n"))
		   (cdr form)))
	   (colorize form)))
      ))

   (define next next-page)
   (define prev prev-page)
   (define start next-page)
   
   (export encode next-page reset-page next-page prev-page set-slides next prev start)))

(display "[complete]\n")
