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
	       ((equal? keyword 'allocation) (add-to-list allocations slot-name))
	       )
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

(define (accessor-name-to-getter-name accessor-name)
  (string->symbol (string-append "set-" (symbol->string accessor-name))))

(define-macro (defclass class-name ancestors slots)
  `(begin
    (define ,class-name (let ((class-slots '()) (instance-slots '()))
			   (alist-merge (list (cons 'ansesorts ',ancestors))
					(list (cons 'slots (process-slots ',slots))))
			   ))
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

"OK"

(define (class-init-forms class-name)
  (let* ((class (eval class-name)) (slots (assoc-default 'slots class)))
    (assoc-default 'init-forms slots)
    ))

(define (class-init-args class-name)
  (let* ((class (eval class-name)) (slots (assoc-default 'slots class)))
    (assoc-default 'init-args slots)
    ))

(define (class-accessors class-name)
  (let* ((class (eval class-name)) (slots (assoc-default 'slots class)))
    (assoc-default 'accessors slots)
    ))

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
    (alist-set! props 'parents class-name)
    ; set children? to parent class
    ; set accessor and reader
    (write props)

    (letrec ((obj
	      (append
	       (list
		(cons 'parents class-name))
	       (map
		(lambda (pair)
		  (let ((slot-name (car pair)) (accessor-getter-name (cdr pair)))
		    (cons accessor-getter-name (lambda () (assoc-default slot-name props)))))
		(class-accessors class-name))
	       (map
		(lambda (pair)
		  (let* ((slot-name (car pair)) (accessor-setter-name (accessor-name-to-getter-name (cdr pair))))
		    (cons accessor-setter-name (lambda (val) (alist-set! props slot-name val) obj))))
		(class-accessors class-name)))))
      obj)))

(write "ok")

;; (defclass circle ()
;;   ((radius (accessor . circle-radius))
;;    (center (accessor . circle-center))))
;; (defclass circle () ((radius (accessor . circle-radius)) (center (accessor . circle-center))))


;; (defclass circle ()
;;   ((radius (accessor . circle-radius) (initarg . radius))
;;    (center (accessor . circle-center) (initarg . center))))
;; (defclass circle () ((radius (accessor . circle-radius) (initarg . radius)) (center (accessor . circle-center) (initarg . center))))


;; (defclass shape ()
;;   ((color (accessor . shape-color) (initarg . color))
;;    (visible (accessor . shape-visible)
;; 	    (initarg . visible)
;; 	    (initform . #t))))
;; (defclass shape () ((color (accessor . shape-color) (initarg . color)) (visible (accessor . shape-visible) (initarg . visible) (initform . #t))))


;; (defclass my-circle (circle)
;;   nil)


;; (defclass mymy-circle (circle)
;;   ((zokusei (accessor . mymy-circle-zokusei))))

;; (defclass screen-circle (circle shape)
;;   nil)

;; (make-instance 'circle `(radius . 2) `(center (0 . 0)))