# `gfn-latex.el`

## �T�v

LaTeX�t�@�C�������M���邽�߂̊ȑf��Emacs Lisp�p�b�P�[�W�D
`gfn-latex-mode` �Ƃ������W���[���[�h����Ȃ��Ă��܂��D
���̂Ƃ���^�C�v�Z�b�g��latexmk�CPDF�{����SumatraPDF���g�����Ƃ�O��Ƃ��Ă��܂��D
�i�����̓I�v�V�����ɂ���ĉςɂ���������ł��܂����C���̂Ƃ�����u�j�D
�@�\�Ƃ���

* �V���^�b�N�X�n�C���C�g
* �� `\begin{hoge} ... \end{hoge}` �̓��͕⏕
* Emacs����LaTeX�t�@�C�����^�C�v�Z�b�g���ă��O��\��
* Emacs����sumatraPDF���N�����ďo�͂��ꂽPDF���{��

������܂��D

## �p�@

`init.el` �Ɉȉ��������邱�ƂŊJ�����Ƃ��Ɏ����I�� `gfn-latex-mode` �ɓ���܂��F

```lisp
(setq auto-mode-alist
      (append '(("\\.tex$" . gfn-latex-mode)
                ("\\.ltx$" . gfn-latex-mode)
                ("\\.cls$" . gfn-latex-mode)
                ("\\.sty$" . gfn-latex-mode)
                ("\\.clo$" . gfn-latex-mode)
                ("\\.bbl$" . gfn-latex-mode)) auto-mode-alist))
```

�L�[�o�C���h�Ƌ@�\�͈ȉ��̂Ƃ���F

* `C-c C-b` �����͕⏕�D����Ƀ~�j�o�b�t�@�Ŋ�����u���Ă���̂œ��͂��܂��D
            �Ⴆ�΂����� `align*` �Ɠ��͂���� `\begin{align*} \end{align*}` �������I�ɓ��͂���܂��D
* `C-c C-t` ���݂̃o�b�t�@��LaTeX�t�@�C�����^�C�v�Z�b�g���܂��D���O�� `*Async Shell Command*` �o�b�t�@�ɕ\������܂��D
* `C-c C-M-c` ���O�̕\������ɑ���܂��D
* `C-c C-f` PDF�t�@�C�����J���܂��D
