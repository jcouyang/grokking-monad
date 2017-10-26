book.md: index.md
	awk '{ if (NR <= 42) print > \"SUMMARY.md\"; else print > \"book.md\"; }' index.md

SUMMARY.md: book.md
	sed -i ''  -e 's/#org/book.md#org/g' SUMMARY.md

build: SUMMARY.md book.md
	gitbook build . ../docs

book.%: build
	gitbook $(subst .,,$(suffix $@))
