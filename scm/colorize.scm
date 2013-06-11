(display "load colorize library...")

(define colorize-module  
  (module
   (require 'core)
   (import core-module)
   
   (require 'alist)
   (import alist-module)

   (define colorize-colors '((black         . 0)
		    (red           . 1)
		    (green         . 2)
		    (yellow        . 3)
		    (blue          . 4)
		    (magenta       . 5)
		    (cyan          . 6)
		    (white         . 7)
		    (default       . 9)
		    (light_black   . 10)
		    (light_red     . 11)
		    (light_green   . 12)
		    (light_yellow  . 13)
		    (light_blue    . 14)
		    (light_magenta . 15)
		    (light_cyan    . 16)
		    (light_white   . 17)))

   (define colorize-modes '(
		   (default    . 0) ; Turn off all attributes
		   (bright     . 1) ; Set bright mode
		   (underline  . 4) ; Set underline mode
		   (blink      . 5) ; Set blink mode
		   (swap       . 7) ; Exchange foreground and background colors
		   (hide       . 8) ; Hide text (foreground color would be the same as background)
		   ))

   (define (colorize form)
     (let ((color 9) (mode 0))
       (for-each
	(lambda (param)
	  (if (neq? #f (assoc-default param colorize-colors)) 	(set! color (assoc-default param colorize-colors)) #f)
	  (if (neq? #f (assoc-default param  colorize-modes)) 	(set! mode  (assoc-default param colorize-modes) ) #f))
	(get-params form))
       (string-append
	"\e["
	(number->string mode)
	";"
	(number->string (+ 30 color))
	";49m"
	(get-str form)
	"\e[0m")))

   (define (v! form)
     (display (colorize form))
     (newline))

   (define (get-params form)
     (if (pair? form)
	 (cons (car form) (get-params (cadr form)))
	 '()))

   (define (get-str form)
     (if (pair? form)
	 (if (string? (cadr form))
	     (cadr form)
	     (get-str (cadr form)))
	 #f))
   
   (export colorize v! colorize-colors colorize-modes)))

(display "[complete]\n")
