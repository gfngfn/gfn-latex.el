<!-- -*- coding: utf-8 -*- -->
# `gfn-latex.el`

## 概要

LaTeXファイルを執筆するための簡素なEmacs Lispパッケージ．
`gfn-latex-mode` というメジャーモードからなっています．
今のところタイプセットにlatexmk，PDF閲覧にSumatraPDFを使うことを前提としています．
（これらはオプションによって可変にする実装もできますが，今のところ放置）．
機能として

* シンタックスハイライト
* 環境 `\begin{hoge} ... \end{hoge}` の入力補助
* 数式 `\( ... \)` の入力補助
* EmacsからLaTeXファイルをタイプセットしてログを表示
* EmacsからSumatraPDFを起動して出力されたPDFを閲覧

があります．

## 設定

`init.el` に以下を加えることで，LaTeXファイルを開いたときに自動的に `gfn-latex-mode` に入ります：

```lisp
(add-to-list 'auto-mode-alist '("\\.tex$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.ltx$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.cls$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.sty$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.clo$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.bbl$" . gfn-latex-mode))
```

`C-c C-f` で起動するPDFヴューワのコマンドは，変数 `gfn-latex-pdf-viewer-command` で指定できます．デフォルトは `open` になっていますが，例えばWindows環境でSumatra PDFで表示したい場合は以下のように指定します：

```lisp
(setq gfn-latex-pdf-viewer-command "sumatrapdf")
```

## キーバインドと機能

| キーバインド | 対応づけられた函数の仕様           | 説明 |
|-----------|----------------------------------|------|
| `{`       | `(gfn-latex-insert-bracket &optional num)` | 自動で `}` も補って入力します．`C-u` などで前置引数 num を与えると，num 個の `{}` の組を出力して最初の組の中にカーソルを持ってきます． |
| `C-c C-b` | `(gfn-latex-insert-environment envname)`   | 環境入力補助．直後にミニバッファで環境名を訊いてくるので入力します．例えばここで `align*` と入力すると `\begin{align*} ... \end{align*}` が自動的に入力されます． |
| `C-c C-d` | `(gfn-latex-insert-math)` | インライン数式 `\( ... \)` を挿入します． |
| `C-c C-t` | `(gfn-latex-typeset)` | 現在のバッファのLaTeXファイルをタイプセットします．ログは `*Async Shell Command*` バッファに表示されます． |
| `C-M-c`   | `(gfn-latex-scroll-log)` | ログの表示を上に送ります． |
| `C-c C-f` | `(gfn-latex-open-pdf)` | 現在のバッファのLaTeXファイルに対応するPDFファイルを開きます． |
