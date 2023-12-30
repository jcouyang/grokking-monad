all: grokking-monad.epub grokking-monad.pdf grokking-monad.mobi

grokking-monad-zh.epub: !index.org
	pandoc !index.org --epub-cover-image=./images/cover.png --number-sections --standalone  --self-contained -o grokking-monad.epub

grokking-monad-zh.pdf: grokking-monad-zh.tex
	xelatex grokking-monad-zh.tex grokking-monad-zh.pdf

grokking-monad-en.pdf: grokking-monad-en.tex
	xelatex grokking-monad-en.tex grokking-monad-en.pdf

grokking-monad-zh.mobi: grokking-monad.epub
	/Applications/calibre.app/Contents/MacOS/ebook-convert grokking-monad.epub grokking-monad.mobi
