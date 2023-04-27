# `gfn-latex.el`

## Summary

An Emacs package that provides a lightweight major mode for writing LaTeX sources.

Major functionalities:

- Syntax highlighting of the LaTeX syntax
  * Supports highlighting of inline [Ott](https://github.com/ott-lang/ott) expressions `[[...]]` as well
- Easy insertion of `\begin{foo}...\end{foo}`, `\(...\)`, etc.
- Typesetting with [Latexmk](https://www.ctan.org/pkg/latexmk)


## Keybinds

| keybind | corresponding function | what will happen |
|---------|------------------------|------------------|
| `{`       | `(gfn-latex-insert-bracket &optional NUM)` | Inserts a brace pair `{}` and move the cursor in it. When a natural number `NUM` is given by using `C-u`, it will insert `NUM` brace pairs. |
| `C-c C-b` | `(gfn-latex-insert-environment ENVNAME)` | Inserts an environment `\begin{ENVNAME} \end{ENVNAME}` where `ENVNAME` can be given by a minibuffer. |
| `C-c C-d` | `(gfn-latex-insert-math)` | Inserts `\(\)` for inline maths. |
| `C-c C-t` | `(gfn-latex-typeset)` | Typesets the current buffer. Logs will be shown in the `*Async Shell Command*` buffer. |
| `C-M-c`   | `(gfn-latex-scroll-log)` | Scrolls the log down. |
| `C-c C-f` | `(gfn-latex-open-pdf)` | Opens the PDF file corresponding to the current buffer. |


## Settings

Adding the following lines to `init.el` makes `gfn-latex-mode` enabled automatically when opening LaTeX-related files:

```lisp
(add-to-list 'auto-mode-alist '("\\.tex$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.ltx$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.cls$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.sty$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.clo$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.bbl$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.otex$" . gfn-latex-mode))
```

You can set the PDF viewer to `gfn-latex-pdf-viewer-command` (default: `"open"`):

```lisp
(setq gfn-latex-pdf-viewer-command "sumatrapdf")
```
