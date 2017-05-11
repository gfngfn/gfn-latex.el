# `gfn-latex.el`

## �T�v

LaTeX�t�@�C�������M���邽�߂̊ȑf��Emacs Lisp�p�b�P�[�W�D
`gfn-latex-mode` �Ƃ������W���[���[�h����Ȃ��Ă��܂��D
���̂Ƃ���^�C�v�Z�b�g��latexmk�CPDF�{����SumatraPDF���g�����Ƃ�O��Ƃ��Ă��܂��D
�i�����̓I�v�V�����ɂ���ĉςɂ���������ł��܂����C���̂Ƃ�����u�j�D
�@�\�Ƃ���

* �V���^�b�N�X�n�C���C�g
* �� `\begin{hoge} ... \end{hoge}` �̓��͕⏕
* ���� `\( ... \)` �̓��͕⏕
* Emacs����LaTeX�t�@�C�����^�C�v�Z�b�g���ă��O��\��
* Emacs����SumatraPDF���N�����ďo�͂��ꂽPDF���{��

������܂��D

## �ݒ�

`init.el` �Ɉȉ��������邱�ƂŁCLaTeX�t�@�C�����J�����Ƃ��Ɏ����I�� `gfn-latex-mode` �ɓ���܂��F

```lisp
(add-to-list 'auto-mode-alist '("\\.tex$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.ltx$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.cls$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.sty$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.clo$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.bbl$" . gfn-latex-mode))
```

## �L�[�o�C���h�Ƌ@�\

| �L�[�o�C���h | �Ή��Â���ꂽ�����̎d�l           | ���� |
|-----------|----------------------------------|------|
| `{`       | `(gfn-latex-insert-bracket &optional num)` | ������ `}` ������ē��͂��܂��D`C-u` �ȂǂőO�u���� num ��^����ƁCnum �� `{}` �̑g���o�͂��čŏ��̑g�̒��ɃJ�[�\���������Ă��܂��D |
| `C-c C-b` | `(gfn-latex-insert-environment envname)`   | �����͕⏕�D����Ƀ~�j�o�b�t�@�Ŋ�����u���Ă���̂œ��͂��܂��D�Ⴆ�΂����� `align*` �Ɠ��͂���� `\begin{align*} ... \end{align*}` �������I�ɓ��͂���܂��D |
| `C-c C-d` | `(gfn-latex-insert-math)` | �C�����C������ `\( ... \)` ��}�����܂��D |
| `C-c C-t` | `(gfn-latex-typeset)` | ���݂̃o�b�t�@��LaTeX�t�@�C�����^�C�v�Z�b�g���܂��D���O�� `*Async Shell Command*` �o�b�t�@�ɕ\������܂��D |
| `C-M-c`   | `(gfn-latex-scroll-log)` | ���O�̕\������ɑ���܂��D |
| `C-c C-f` | `(gfn-latex-open-pdf)` | ���݂̃o�b�t�@��LaTeX�t�@�C���ɑΉ�����PDF�t�@�C�����J���܂��D |
