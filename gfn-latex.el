;; gfn-latex.el
(provide 'gfn-latex)

(defvar gfn-latex-mode-map
  (let ((km (make-keymap)))
    (progn
      (define-key km (kbd "C-c C-b") 'insert-environment)
      (define-key km (kbd "C-c C-t") 'typeset)
      (define-key km (kbd "C-M-c") 'scroll-log)
      (define-key km (kbd "C-c C-f") 'open-pdf)
      (define-key km (kbd "{") 'insert-bracket-pair)
      km)))

(defface gfn-latex-comment-out-face
  '((t (:foreground "#888888" :backgound "dark")))
  "comment-outs")

(defface gfn-latex-expansion-control-face
  '((t (:foreground "#88ff88" :backgound "dark")))
  "control sequences for the expansion control")

(defface gfn-latex-definition-face
  '((t (:foreground "#ffff88" :backgound "dark")))
  "control sequences for the definition")

(defface gfn-latex-if-face
  '((t (:foreground "#ff8888" :backgound "dark")))
  "if-branching primitives")

(defface gfn-latex-important-primitive-face
  '((t (:foreground "#ffbb88" :backgound "dark")))
  "important primitives")

(defface gfn-latex-control-sequence-face
  '((t (:foreground "#8888ff" :backgound "dark")))
  "general control sequences")

(defface gfn-latex-environment-frame-face
  '((t (:foreground "#ff2222" :backgound "dark")))
  "LaTeX environment frames")

(defface gfn-latex-environment-name-face
  '((t (:foreground "#ffaa22" :backgound "dark")))
  "LaTeX environment names")

(define-generic-mode gfn-latex-mode
  '()
  '()
  '(("\\(\\\\[a-zA-Z@]+\\)\\>"
     (1 'gfn-latex-control-sequence-face t))
    ("\\(\\\\\\(?:expandafter\\|noexpand\\)\\)\\>"
     (1 'gfn-latex-expansion-control-face t))
    ("\\(\\\\\\(?:global\\|let\\|def\\|gdef\\)\\)\\>"
     (1 'gfn-latex-definition-face t))
    ("\\(\\\\\\(?:if[a-zA-Z@]*\\|else\\|or\\|fi\\)\\)\\>"
     (1 'gfn-latex-if-face t))
    ("\\(\\\\\\(?:the\\|string\\|csname\\|endcsname\\)\\)\\>"
     (1 'gfn-latex-important-primitive-face t))
    ("\\(\\\\\\(?:begin\\|end\\){\\)\\([a-zA-Z\\*]*\\)\\(}\\)"
     (1 'gfn-latex-environment-frame-face t)
     (2 'gfn-latex-environment-name-face t)
     (3 'gfn-latex-environment-frame-face t))
    ("\\(%.*\n\\)"
     (1 'gfn-latex-comment-out-face t)))
  '(".+\\.\\(tex\\|sty\\)")
  '((lambda ()
      (progn
	(use-local-map gfn-latex-mode-map)
	(setq mode-name "gfn-LaTeX"))))
  "gfn-LaTeX")


(defun typeset ()
  (interactive)
  (progn
    (message "Typesetting '%s' ..." (file-name-nondirectory buffer-file-name))
    (async-shell-command (format "latexmk %s\n" buffer-file-name))))

(defun scroll-log ()
  (interactive)
  (with-selected-window (get-buffer-window "*Async Shell Command*")
    (scroll-down)))

(defun open-pdf ()
  (interactive)
  (let ((pdf-file-path (concat (file-name-sans-extension buffer-file-name) ".pdf")))
    (progn
      (message "Opening '%s' ..." pdf-file-path)
      (async-shell-command (format "sumatrapdf %s\n" pdf-file-path)))))

(defun insert-environment (envname)
  (interactive "sname: ")
  (let ((original-point (point)))
    (let ((indent-width (gfn-latex/get-indent-width original-point)))
      (let ((indent-string (make-string indent-width ? )))
        (insert (format "\\begin{%s}\n%s  \n%s\\end{%s}" envname indent-string indent-string envname))))))

(defun gfn-latex/get-indent-width (orgpt)
  (let ((bgnpt (gfn-latex/find-beginning-point orgpt)))
    (let ((endpt (gfn-latex/get-end-point bgnpt orgpt)))
      (- endpt bgnpt))))

(defun gfn-latex/get-end-point (pt maxpt)
  (cond ((equal pt (point-max)) pt)
        ((not (equal (char-after pt) ? )) pt)
        ((equal pt maxpt) pt)
        (t (gfn-latex/get-end-point (1+ pt) maxpt))))

(defun gfn-latex/find-beginning-point (pt)
  (if (or (equal pt (point-min)) (equal (char-before pt) ?\n))
      pt
    (gfn-latex/find-beginning-point (1- pt))))

(defun insert-bracket-pair (&optional arg)
  (interactive "P")
  (let* ((num (if (equal arg nil) 1 arg)))
    (progn
      (insert-bracket-pair-sub num)
      (forward-char (- 1 (* 2 num)))))
  )

(defun insert-bracket-pair-sub (num)
  (if (<= num 0)
      nil
    (progn
      (insert "{}")
      (insert-bracket-pair-sub (1- num)))))
