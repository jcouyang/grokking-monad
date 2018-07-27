grokking-monad.epub: index.org
	pandoc index.org --epub-cover-image=./images/cover.png --number-sections --standalone  --self-contained -o grokking-monad.epub
