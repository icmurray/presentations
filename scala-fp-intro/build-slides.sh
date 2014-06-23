pandoc -s -t beamer --filter ./include-scala-code.hs --variable fontsize=11pt --template template.tex slides.markdown -o slides.pdf
