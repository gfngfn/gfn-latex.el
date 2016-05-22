# `gfn-latex.el`

## 概要

LaTeXファイルを執筆するための簡素なEmacs Lispパッケージ．
`gfn-latex-mode` というメジャーモードからなっています．
今のところタイプセットにlatexmk，PDF閲覧にSumatraPDFを使うことを前提としています．
（これらはオプションによって可変にする実装もできますが，今のところ放置）．
機能として

* シンタックスハイライト
* 環境 `\begin{hoge} ... \end{hoge}` の入力補助
* EmacsからLaTeXファイルをタイプセットしてログを表示
* EmacsからsumatraPDFを起動して出力されたPDFを閲覧

があります．

## 用法

`init.el` に以下を加えることで開いたときに自動的に `gfn-latex-mode` に入ります：

```lisp
(setq auto-mode-alist
      (append '(("\\.tex$" . gfn-latex-mode)
                ("\\.ltx$" . gfn-latex-mode)
                ("\\.cls$" . gfn-latex-mode)
                ("\\.sty$" . gfn-latex-mode)
                ("\\.clo$" . gfn-latex-mode)
                ("\\.bbl$" . gfn-latex-mode)) auto-mode-alist))
```

キーバインドと機能は以下のとおり：

* `C-c C-b` 環境入力補助．直後にミニバッファで環境名を訊いてくるので入力します．
            例えばここで `align*` と入力すると `\begin{align*} \end{align*}` が自動的に入力されます．
* `C-c C-t` 現在のバッファのLaTeXファイルをタイプセットします．ログは `*Async Shell Command*` バッファに表示されます．
* `C-c C-M-c` ログの表示を上に送ります．
* `C-c C-f` PDFファイルを開きます．
