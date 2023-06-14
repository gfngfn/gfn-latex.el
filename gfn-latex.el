;;; gfn-latex.el --- A lightweight major mode for LaTeX (possibly with Ott)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a lightweight major mode for LaTeX (possibly with Ott).

;;; Code:

(defgroup gfn-latex nil
  "Group for gfn-latex."
  :prefix "gfn-latex-"
  :group 'languages)

(defvar gfn-latex-pdf-viewer-command "open")

(defvar gfn-latex-mode-map
  (let ((km (make-keymap)))
    (progn
      (define-key km (kbd "C-c C-b") 'gfn-latex-insert-environment)
      (define-key km (kbd "C-c C-t") 'gfn-latex-typeset)
      (define-key km (kbd "C-c C-d") 'gfn-latex-insert-math)
      (define-key km (kbd "C-c RET") 'gfn-latex-typeset-using-makefile)
      (define-key km (kbd "C-M-c")   'gfn-latex-scroll-log)
      (define-key km (kbd "C-c C-f") 'gfn-latex-open-pdf)
      (define-key km (kbd "{")       'gfn-latex-insert-brace-pair)
      (define-key km (kbd "RET")     'gfn-latex-new-line)
      km)))

(defface gfn-latex-comment-out-face
  '((t (:foreground "#888888" :backgound "dark")))
  "Comment-outs."
  :group 'gfn-latex)

(defface gfn-latex-escaped-symbol-face
  '((t (:foreground "#ff88ff" :backgroud "dark")))
  "Escaped parentheses."
  :group 'gfn-latex)

(defface gfn-latex-ott-bracket-face
  '((t (:foreground "#22ffff" :backgroud "dark")))
  "Ott brackets."
  :group 'gfn-latex)

(defface gfn-latex-expansion-control-face
  '((t (:foreground "#88ff88" :backgound "dark")))
  "Control sequences for the expansion control."
  :group 'gfn-latex)

(defface gfn-latex-definition-face
  '((t (:foreground "#ffff88" :backgound "dark")))
  "Control sequences for the definition."
  :group 'gfn-latex)

(defface gfn-latex-if-face
  '((t (:foreground "#ff8888" :backgound "dark")))
  "If-branching primitives."
  :group 'gfn-latex)

(defface gfn-latex-important-primitive-face
  '((t (:foreground "#ffbb88" :backgound "dark")))
  "Important primitives."
  :group 'gfn-latex)

(defface gfn-latex-control-sequence-face
  '((t (:foreground "#8888ff" :backgound "dark")))
  "General control sequences."
  :group 'gfn-latex)

(defface gfn-latex-environment-frame-face
  '((t (:foreground "#ff2222" :backgound "dark")))
  "LaTeX environment frames."
  :group 'gfn-latex)

(defface gfn-latex-environment-name-face
  '((t (:foreground "#ffaa22" :backgound "dark")))
  "LaTeX environment names."
  :group 'gfn-latex)

(defface gfn-latex-latex-program-face
  '((t (:foreground "#ffdd22" :backgroud "dark")))
  "LaTeX commands for LaTeX-level programs."
  :group 'gfn-latex)

(define-generic-mode gfn-latex-mode
  '()
  '()
  '(("\\(\\\\[a-zA-Z@]+\\)\\>"
     (1 'gfn-latex-control-sequence-face t))
    ("\\(\\\\\\(?:\\\\\\|(\\|)\\|\\[\\|\\]\\|{\\|}\\)\\)"
     (1 'gfn-latex-escaped-symbol-face t))
    ("\\(\\[\\[\\|\\]\\]\\)"
     (1 'gfn-latex-ott-bracket-face t))
    ("\\(\\\\\\(?:expandafter\\|noexpand\\)\\)\\>"
     (1 'gfn-latex-expansion-control-face t))
    ("\\(\\\\\\(?:global\\|let\\|futurelet\\|def\\|edef\\|gdef\\|xdef\\|newcount\\|newdimen\\|newbox\\|newskip\\|newtoks\\|newif\\|advance\\|multiply\\|setbox\\)\\)\\>"
     (1 'gfn-latex-definition-face t))
    ("\\(\\\\\\(?:if[a-zA-Z@]*\\|else\\|or\\|fi\\)\\)\\>"
     (1 'gfn-latex-if-face t))
    ("\\(\\\\\\(?:the\\|string\\|csname\\|endcsname\\)\\)\\>"
     (1 'gfn-latex-important-primitive-face t))
    ("\\(\\\\\\(?:begin\\|end\\){\\)\\([a-zA-Z\\*]*\\)\\(}\\)"
     (1 'gfn-latex-environment-frame-face t)
     (2 'gfn-latex-environment-name-face t)
     (3 'gfn-latex-environment-frame-face t))
    ("\\(\\\\\\(?:newcommand\\|renewcommand\\|providecommand\\|newcounter\\|newlength\\|setlength\\|setcounter\\|refstepcounter\\|stepcounter\\)\\)\\>"
     (1 'gfn-latex-latex-program-face t))
    ("\\(%.*\n\\)"
     (1 'gfn-latex-comment-out-face t)))
  '(".+\\.\\(tex\\|sty\\|ltx\\|cls\\|clo\\|otex\\)")
  '((lambda ()
      (progn
	(use-local-map gfn-latex-mode-map)
	(setq mode-name "gfn-LaTeX")
        (auto-complete-mode t))))
  "Gfn-LaTeX mode.")


(defun gfn-latex-typeset ()
  "Typeset the file corresponding to the current buffer (by using `latexmk`)."
  (interactive)
  (progn
    (message "Typesetting '%s' ..." (file-name-nondirectory buffer-file-name))
    (async-shell-command (format "latexmk %s\n" buffer-file-name))))


(defun gfn-latex-typeset-using-makefile ()
  "Typeset the file corresponding to the current buffer (by using `make`)."
  (interactive)
  (progn
    (message "Typesetting '%s' ..." (file-name-nondirectory buffer-file-name))
    (async-shell-command "make\n")))


(defun gfn-latex-scroll-log ()
  "Scroll the typesetting log down."
  (interactive)
  (with-selected-window (get-buffer-window "*Async Shell Command*")
    (scroll-down)))


(defun gfn-latex-open-pdf ()
  "Open the resulting PDF file corresponding to the current buffer."
  (interactive)
  (let ((pdf-file-path (concat (file-name-sans-extension buffer-file-name) ".pdf")))
    (progn
      (message "Opening '%s' ..." pdf-file-path)
      (async-shell-command (format "%s %s\n" gfn-latex-pdf-viewer-command pdf-file-path)))))


(defun gfn-latex-insert-environment (envname)
  "Insert an environment \\begin{ENVNAME}...\\end{ENVNAME} and move the cursor in it."
  (interactive "sname: ")
  (let ((original-point (point)))
    (let ((indent-width (gfn-latex/get-indent-width original-point)))
      (let ((indent-string (make-string indent-width ? )))
        (progn
          (insert (format "\\begin{%s}\n%s  \n%s\\end{%s}" envname indent-string indent-string envname))
          (forward-char (- (length (format "\n%s\\end{%s}" indent-string envname)))))))))


(defun gfn-latex/get-indent-width (orgpt)
  "Get the indentation width of the line ORGPT belongs to."
  (let ((bgnpt (gfn-latex/find-beginning-point orgpt)))
    (let ((endpt (gfn-latex/get-end-point bgnpt orgpt)))
      (- endpt bgnpt))))


(defun gfn-latex/get-end-point (pt maxpt)
  "Find where the indentation of the line containing PT ends before MAXPT."
  (cond ((equal pt (point-max)) pt)
        ((not (equal (char-after pt) ? )) pt)
        ((equal pt maxpt) pt)
        (t (gfn-latex/get-end-point (1+ pt) maxpt))))


(defun gfn-latex/find-beginning-point (pt)
  "Find where the line containing PT begins."
  (if (or (equal pt (point-min)) (equal (char-before pt) ?\n))
      pt
    (gfn-latex/find-beginning-point (1- pt))))


(defun gfn-latex-insert-brace-pair (&optional arg)
  "Insert brace pairs {} of number ARG around the selected region.
Default: ARG = 1"
  (interactive "P")
  (cond ((use-region-p)
         (let ((rb (region-beginning)))
           (let ((re (region-end)))
             (progn
               (goto-char rb)
               (insert "{")
               (goto-char (1+ re))
               (insert "}")
               (forward-char -1)))))
        (t (let* ((num (if (equal arg nil) 1 arg)))
             (progn
               (gfn-latex/insert-brace-pair-sub num)
               (forward-char (- 1 (* 2 num))))))))


(defun gfn-latex/insert-brace-pair-sub (num)
  "Insert brace pairs of number NUM."
  (if (<= num 0)
      nil
    (progn
      (insert "{}")
      (gfn-latex/insert-brace-pair-sub (1- num)))))


(defun gfn-latex-insert-math ()
  "Insert a math parenthesis pair \\(\\)."
  (interactive)
  (progn
    (insert "\\(\\)")
    (backward-char 2)))

(defun gfn-latex-new-line ()
  "Start a new line possibly with `%` added."
  (interactive)
  (cond ((gfn-latex/is-cursor-between-braces) (gfn-latex/insert-new-line-between-braces))
        ((gfn-latex/is-cursor-after-brace) (gfn-latex/insert-percent-and-new-line-after-brace))
        ((gfn-latex/is-cursor-after-percent-and-brace) (gfn-latex/insert-new-line-after-brace))
        (t (gfn-latex/insert-new-line-normal))))


(defun gfn-latex/is-cursor-between-braces ()
  "Judge whether the cursor is immediately after `{` and immediately before `}`."
  (and (equal (char-before (point)) ?{) (equal (char-after (point)) ?})))


(defun gfn-latex/is-cursor-after-brace ()
  "Judge whether the cursor is immediately after `{`."
  (equal (char-before (point)) ?{))


(defun gfn-latex/is-cursor-after-percent-and-brace ()
  "Judge whether the cursor is immediately after `{%`."
  (and (equal (char-before (point)) ?%) (equal (char-before (1- (point))) ?{)))


(defun gfn-latex/get-current-indent-string ()
  "Return the indentation string based on the current cursor point."
  (let ((original-point (point)))
    (let ((indent-width (gfn-latex/get-indent-width original-point)))
      (make-string indent-width ? ))))


(defun gfn-latex/insert-new-line-between-braces ()
  "Start a new line with the indentation deepened."
  (let ((indent-string (gfn-latex/get-current-indent-string)))
    (progn
      (insert (format "%%\n%s  \n%s" indent-string indent-string))
      (forward-char (- (length (format "\n%s" indent-string)))))))


(defun gfn-latex/insert-new-line-after-brace ()
  "Start a new line with the indentation deepened."
  (let ((indent-string (gfn-latex/get-current-indent-string)))
    (insert (format "\n%s  " indent-string))))


(defun gfn-latex/insert-percent-and-new-line-after-brace ()
  "Insert a percent sign and start a new line with the indentation deepened."
  (let ((indent-string (gfn-latex/get-current-indent-string)))
    (insert (format "%%\n%s  " indent-string))))


(defun gfn-latex/insert-new-line-normal ()
  "Start a new line with the indentation kept."
  (let ((indent-string (gfn-latex/get-current-indent-string)))
    (insert (format "\n%s" indent-string))))

(provide 'gfn-latex)
;;; gfn-latex.el ends here
