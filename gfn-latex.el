;; gfn-latex.el

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
  '(?%)
  '()
  '(("\\(\\\\[a-zA-Z@]+\\)[^a-zA-Z@]"
     (1 'gfn-latex-control-sequence-face t))
    ("\\(\\\\\\(?:expandafter\\|noexpand\\)\\)[^a-zA-Z@]"
     (1 'gfn-latex-expansion-control-face t))
    ("\\(\\\\\\(?:global\\|let\\|def\\|gdef\\)\\)[^a-zA-Z@]"
     (1 'gfn-latex-definition-face t))
    ("\\(\\\\\\(?:if[a-zA-Z@]*\\|else\\|or\\|fi\\)\\)[^a-zA-Z@]"
     (1 'gfn-latex-if-face t))
    ("\\(\\\\\\(?:the\\|string\\|csname\\|endcsname\\)\\)[^a-zA-Z@]"
     (1 'gfn-latex-important-primitive-face t))
    ("\\(\\\\\\(?:begin\\|end\\){\\)\\([a-zA-Z\\*]*\\)\\(}\\)"
     (1 'gfn-latex-environment-frame-face t)
     (2 'gfn-latex-environment-name-face t)
     (3 'gfn-latex-environment-frame-face t)))
  '(".+\\.\\(tex\\|sty\\)")
  '()
  "gfn-LaTeX")


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

(defun get-indent-width-interactive ()
  (interactive)
  (message (format "indent width: %d" (get-indent-width (point)))))
