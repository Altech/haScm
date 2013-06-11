(define usr-list-module
  (module
   
   (define (filter f x)
     (if (null? x)
	 x
	 (if (f (car x))
	     (cons (car x) (filter f (cdr x)))
	     (filter f (cdr x)))))

   (define (zip list1 list2)
     (cond
      ((null? list1) '())
      ((null? list2) '()) 
      (else (cons (cons (car list1) (car list2)) (zip (cdr list1) (cdr list2))))))

   (define (replicate n v)
     (if (eq? n 0)
	 '()
	 (cons v (replicate (- n 1) v))))

   (define (remove-dup ls e)
     (if (null? ls)
	 '()
	 (let ((l (filter (lambda (x) (not (equal? x e))) ls)))
	   (cons (car l) (remove-dup (cdr l) (car l))))))
   
   (define (for-all? f ls)
     (if (null? ls)
	 #t
	 (if (f (car ls))
	     (for-all? f (cdr ls))
	     #f)))
   
   (define (member? o ls) (not (eq? #f (member o ls))))

   (define (merge xs ys f)
     (cond
      ((null? xs) ys)
      ((null? ys) xs)
      (else (if (f (car xs) (car ys))
		(cons (car xs) (merge (cdr xs) ys f))
		(cons (car ys) (merge xs (cdr ys) f))))))

   (define (split xs)
     (cond
      ((null? xs) (cons '() '()))
      ((= 1 (length xs)) (cons xs '()))
      (else (let ((ls (split (cddr xs))))
	      (cons (cons (car xs) (car ls)) (cons (cadr xs) (cdr ls)))))))

   (define (msort xs f)
     (cond
      ((null? xs) '())
      ((= 1 (length xs)) xs)
      (else (let ((splited (split xs)))
	      (merge (msort (car splited) f) (msort (cdr splited) f) f)))))

   (define (insert x lst f)
     (if (null? lst)
	 (list x)
	 (let ((y (car lst))
	       (ys (cdr lst)))
	   (if (f x y)
	       (cons x lst)
	       (cons y (insert x ys f))))))
   
   (define (isort lst f)
     (if (null? lst)
	 '()
	 (insert (car lst)
		 (isort (cdr lst) f)
		 f)))

   (define-macro (add-to-list lst value)
     `(set! ,lst (cons ,value ,lst)))


   (export
     filter zip
     replicate
     remove-dup for-all? member?
     isort msort split merge
     add-to-list)))


