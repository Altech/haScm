1
; (defclass [classname] ([ancestor1] [ancestor2] ...) [slots])
; [slots] = (([slotname] [options]))
; [options] = ([option] . [value])
; [option] = accesser | reader | initarg | initform | allocation

; (defmethod [method-name] [combination]? [args] [body])
; [args] list of (symbol or (pair of val-name and class-name))
; 

; (make-instance [quoted-class-name] [keyword-arguments])
; [keyword-arguments] = list of pair of symbol(keyword) and value

; defgeneric / defcomb

;; (define (process-slots slots) )

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
 
(define (id x) x)

(define (process-slots slots)
  ;; sample argments
  ;; ((radius (accessor . circle-radius) (initarg . radius))
  ;;  (center (accessor . circle-center) (initarg . center)))
  (let ((slot-names '()) (init-forms '()) (init-args '()) (accessors '()) (readers '()) (allocations '()))
    (for-each
     (lambda (slot)
       (let ((slot-name (car slot)) (param-alist (cdr slot)))
	 (add-to-list slot-names slot-name)
	 (for-each
	  (lambda (pair)
	    (let ((keyword (car pair)) (param (cdr pair)))
	      (cond
	       ((equal? keyword 'initarg)    (add-to-list init-args (cons slot-name param)))
	       ((equal? keyword 'initform)   (add-to-list init-forms (cons slot-name param)))
	       ((equal? keyword 'accessor)   (add-to-list accessors (cons slot-name param)))
	       ((equal? keyword 'reader)     (add-to-list readers (cons slot-name param)))
	       ((equal? keyword 'allocation) (add-to-list allocations slot-name)))
	      ))
	  param-alist
	  )
	 )
       )
     slots)
    (list
     (cons 'slot-names  slot-names)
     (cons 'init-forms  init-forms)
     (cons 'init-args   init-args)
     (cons 'readers     readers)
     (cons 'accessors   accessors)
     (cons 'allocations allocations) ;; [TODO]
      )))

(define (parents obj)
  (if (pair? obj)
   (assoc-default 'parents obj)
   (cond
    ((string? obj) 'string)
    ((number? obj) 'number)
    ((list? obj) 'list)
    ((boolean? obj) 'boolean)
    (else #f)
    )))

(define (accessor-name-to-getter-name accessor-name)
  (string->symbol (string-append "set-" (symbol->string accessor-name))))

(define-macro (defclass class-name ancestors slots)
  `(begin
    (define ,class-name (let ((class-slots '()) (instance-slots '()))
			   (alist-merge (list (cons 'ansesorts ',ancestors)
					      (cons 'parents ',ancestors))
					(list (cons 'slots (process-slots ',slots))))))
    (define-all (map
		  (lambda (pair)
		    (let ((slot-name (car pair)) (reader-name (cdr pair)))
		      (cons reader-name
			    (lambda (obj) ((assoc-default reader-name obj))))))
		  (assoc-default 'readers (process-slots ',slots))))
    (define-all (map
		  (lambda (pair)
		    (let ((slot-name (car pair)) (accessor-getter-name (cdr pair)))
		      (cons accessor-getter-name
			    (lambda (obj) ((assoc-default accessor-getter-name obj))))))
		  (assoc-default 'accessors (process-slots ',slots))))
    (define-all (map
		 (lambda (pair)
		   (let ((slot-name (car pair)) (accessor-setter-name (accessor-name-to-getter-name (cdr pair))))
		     (cons accessor-setter-name
			   (lambda (obj val) ((assoc-default accessor-setter-name obj) val)))))
		 (assoc-default 'accessors (process-slots ',slots))))
    ))

(define (class-init-forms class-name)
  (apply append (map (lambda (class-name)
		       (let* ((class (eval class-name)) (slots (assoc-default 'slots class)))
			 (assoc-default 'init-forms slots)))
		     (reverse (get-ancestors class-name))))) ;; reverse is used in defmethod(create init-forms).

(define (class-init-args class-name)
  (apply append (map (lambda (class-name)
		       (let* ((class (eval class-name)) (slots (assoc-default 'slots class)))
			 (assoc-default 'init-args slots)))
		     (reverse (get-ancestors class-name)))))

(define (class-accessors class-name)
  (apply append (map (lambda (class-name)
		       (let* ((class (eval class-name)) (slots (assoc-default 'slots class)))
			 (assoc-default 'accessors slots)))
		     (reverse (get-ancestors class-name)))))

(define (class-readers class-name)
  (apply append (map (lambda (class-name)
		       (let* ((class (eval class-name)) (slots (assoc-default 'slots class)))
			   (assoc-default 'readers slots)))
		     (reverse (get-ancestors class-name)))))

(define (make-instance class-name . args)
  (let ((props '()))
    (for-each  ; set values by default
     (lambda (pair)
       (let ((slot-name (car pair)) (init-form (cdr pair)))
    	 (alist-set! props slot-name (eval init-form))
    	 ))
     (class-init-forms class-name))
    (for-each  ; set values by params
     (lambda (pair)
       (let ((key (car pair)) (value (cdr pair)))
    	 (if (alist-exist? key (class-init-args class-name))
    	     (alist-set! props key value)
    	     #f)))
     args)
    
    (alist-set! props 'parents class-name) ; set parents 

    ; set children to parent class
    
    (write props)
    (write (get-ancestors class-name))
    
    (letrec ((obj ; set accessor and reader
	      (apply append (map
		   (lambda (class-name)
		     (append
		      (list
		       (cons 'parents class-name))
		      (map
		       (lambda (pair)
			 (let ((slot-name (car pair)) (reader-name (cdr pair)))
			   (cons reader-name (lambda () (assoc-default slot-name props)))))
		       (class-readers class-name))
		      (map
		       (lambda (pair)
			 (let ((slot-name (car pair)) (accessor-getter-name (cdr pair)))
			   (cons accessor-getter-name (lambda () (assoc-default slot-name props)))))
		       (class-accessors class-name))
		      (map
		       (lambda (pair)
			 (let* ((slot-name (car pair)) (accessor-setter-name (accessor-name-to-getter-name (cdr pair))))
			   (cons accessor-setter-name (lambda (val) (alist-set! props slot-name val) obj))))
		       (class-accessors class-name))))
		   (get-ancestors class-name)))))
      obj)))

(define (method-to-cand method-name)
  (string->symbol (string-append (symbol->string method-name) "-candidates")))

(define-macro (defmethod method-name args . body)
  `(begin
     (if (symbol-bound? ',(method-to-cand method-name)) ; [TODO] check argument pattern. / enclose method-candidates.
	 (if (method? ,(method-to-cand method-name))
	     (let ((method (cdr ,(method-to-cand method-name))))
	       (set! ,(method-to-cand method-name)  (cons 'clos-method (cons (cons ',args ',body) method))))
	     (write "Error: symbol is alreadly used.")
	     )
	 (define ,(method-to-cand method-name) (list 'clos-method (cons ',args ',body))))
     (define ,method-name (lambda (a . args) (dispatch (eval (method-to-cand ',method-name)) (cons a args))))))

(define (method? o)
  (and (not (null? o)) (equal? 'clos-method (car o))))

(define (remove-type-constraint param)
  (if (pair? param) (car param) param))

(define (dispatch method args)
  (let ((method-candidates (cdr method)))
    ;; (write method-candidates)
    ;; (write args)
    (if (= (length args) (length (caar method-candidates))) ; number of argument is constant.
	(let ((dispatchables (filter (dispatchable? args) method-candidates)))
	  (if (null? dispatchables)
	      (write "Error: wrong types of arugments.")
	      (let ((params (caar dispatchables)) (body (cdar dispatchables)))
		`(lambda (,@(map remove-type-constraint params)) ,@body)
		(apply (eval `(lambda (,@(map remove-type-constraint params)) ,@body)) args)
		)))
	(write "Error: invalid number of arguments.")
	)))

(define (for-all? f ls)
  (if (null? ls)
      #t
      (if (f (car ls))
	  (for-all? f (cdr ls))
	  #f)))

(define (dispatchable? args)
  (lambda (method-candidate) ;; e.g. ((a b) (+ a b))
    (let ((meth-params (car method-candidate)) (meth-body (cdr method-candidate)))
      (write (map check-type-of-argument (zip args meth-params)))
      (for-all?
       check-type-of-argument
       (zip args meth-params)))))

(define (check-type-of-argument pair) ;; e.g. (1 . a) / (1 . (s animal))
  (let ((arg (car pair)) (meth-param (cdr pair)))
    (if (pair? meth-param) ;; e.g. (s animal)
	(is-descendant? arg (cadr meth-param))
	#t
	)))


(define (member? o ls) (not (eq? #f (member o ls))))

(define (is-descendant? arg type)
  (member? type (get-ancestors (parents arg))))

(define (get-all class-name)
  (append (list class-name) (if (null? (map get-all (parents (eval class-name))))
				'()
				(apply append (reverse (map get-all (parents (eval class-name))))))))

(define (get-ancestors class-name)
  (isort (remove-dup (get-all class-name) (undefined))
  	 (lambda (cn1 cn2) (member? cn2 (get-all cn1)))))

(defclass string () ())
(defclass number () ())
(defclass boolean () ())

(defclass object () ((color (initform . 1))))

;; (defclass circle ()
;;   ((radius (accessor . circle-radius))
;;    (center (accessor . circle-center))))
;; (defclass circle () ((radius (accessor . circle-radius)) (center (accessor . circle-center))))


;; (defclass circle ()
;;   ((radius (accessor . circle-radius) (initarg . radius))
;;    (center (accessor . circle-center) (initarg . center))))
(defclass circle (object) ((radius (accessor . circle-radius) (initarg . radius)) (center (accessor . circle-center) (initarg . center))))


;; (defclass shape ()
;;   ((color (accessor . shape-color) (initarg . color))
;;    (visible (accessor . shape-visible)
;; 	    (initarg . visible)
;; 	    (initform . #t))))
(defclass shape (object) ((color (accessor . shape-color) (initarg . color) (initform . 'black)) (visible (accessor . shape-visible) (initarg . visible) (initform . #t))))


;; (defclass my-circle (circle)
;;   nil)


;; (defclass mymy-circle (circle)
;;   ((zokusei (accessor . mymy-circle-zokusei))))

(defclass screen-circle (circle shape)
  ((color (initform . 'purple))))

"ok"

;; (make-instance 'circle `(radius . 2) `(center (0 . 0)))

;; (defclass circle () ((radius (accessor . circle-radius)) (center (accessor . circle-center))))

;; (defmethod area ((c circle))
;;   (* pi (expt (circle-radius c) 2)))
;; (defmethod area ((c circle)) (* pi (expt (circle-radius c) 2)))

;; (defmethod move ((c circle) dx dy)
;;   (incf (car (circle-center c)) dx)
;;   (incf (cdr (circle-center c)) dy)
;;   (circle-center c))

;; (defclass circle ()
;; ((radius :accessor circle-radius)
;; (center :accessor circle-center)))

;; (defmethod area ((c unit-circle)) pi)