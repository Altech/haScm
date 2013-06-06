(define-minor-mode hascm-mode
  "minor mode of hascm."
  nil
  " hascm"
  (list)

  (add-to-list 'auto-mode-alist '("hascmrc" . scheme-mode))
  (font-lock-add-keywords 'scheme-mode
		  '(("(\\(module\\|export\\|import\\)[ \n]" 1 'font-lock-keyword-face)))
  (font-lock-add-keywords 'scheme-mode
		  '(("(\\(require\\)[ \n]" 1 'font-lock-builtin-face))))

;; [EXAPMLE] settings
(require 'scheme)
(add-to-list 'scheme-mode-hook 'hascm-mode)

