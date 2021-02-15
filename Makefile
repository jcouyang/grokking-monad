all: grokking-monad.epub grokking-monad.pdf grokking-monad.mobi

grokking-monad.epub: !index.org
	pandoc !index.org --epub-cover-image=./images/cover.png --number-sections --standalone  --self-contained -o grokking-monad.epub

grokking-monad.pdf: grokking-monad-zh.tex
	xelatex grokking-monad-zh.tex grokking-monad.pdf

grokking-monad.mobi: grokking-monad.epub
	/Applications/calibre.app/Contents/MacOS/ebook-convert grokking-monad.epub grokking-monad.mobi
