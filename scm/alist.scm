(define alist-module
  (module
   
   (define (alist-keys alist)
     (map car alist))

   (define (alist-values alist)
     (map cdr alist))

   (define (alist-exist? key alist)
     (if (pair? (assoc key alist))
	 #t
	 #f))

   (define (filter f x)
     (if (null? x)
	 x
	 (if (f (car x))
	     (cons (car x) (filter f (cdr x)))
	     (filter f (cdr x)))))

   (define (alist-drop key alist)
     (filter (lambda (pair) (not (equal? key (car pair)))) alist))

   (define-macro (alist-set! alist m-key m-value)
     `(set! ,alist (cons (cons ,m-key ,m-value) (alist-drop ,m-key ,alist))))

   (define (alist-merge a1 a2)
     (let ((keys (alist-keys a1)))
       (append a1 (filter (lambda (pair) (equal? #f (member (car pair) keys))) a2))))

   (define alist-remove-duplicated-keys 1)
   
   (export
    alist-keys alist-values alist-exist? alist-drop alist-set! alist-merge)))
