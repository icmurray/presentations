.phony: help clean pdf

BUILDDIR = _build
SOURCE   = slides.markdown
CODE     = $(shell find ./code -name '*.scala')
OUTPUT   = $(BUILDDIR)/slides

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  pdf         to make a PDF file"

clean:
	rm -rf $(OUTPUT).*

$(OUTPUT).pdf:	$(SOURCE) $(CODE)
	mkdir -p $(BUILDDIR)
	pandoc -s -t beamer --filter ./include-scala-code.hs --variable fontsize=11pt --template template.tex $(SOURCE) -o $(OUTPUT).pdf

pdf:	$(OUTPUT).pdf
