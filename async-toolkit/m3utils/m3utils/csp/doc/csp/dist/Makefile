TEXFILES=CastTwoLexer.tex CastTwoParser.tex CastTwoTreeParser.tex CspLexer.tex CspParser.tex

.SUFFIXES: .g .html_done .tex .html

all: html tex pdf

pdf:
	pdflatex test.tex

html: Csp.html_done CastTwo.html_done CastTwoTree.html_done

tex: $(TEXFILES)

CastTwoTree.html_done: CastTwo.html_done

test.pdf: $(TEXFILES) test.tex
	pdflatex test.tex

CspLexer.html: Csp.html_done
CspParser.html: Csp.html_done
CastTwoLexer.html: CastTwo.html_done
CastTwoParser.html: CastTwo.html_done
CastTwoTreeParser.html: CastTwoTree.html_done

.g.html_done:
	java antlr.Tool -html $*.g
	java antlr.Tool $*.g
	touch $*.html_done

.html.tex:
	pandoc $*.html -o $*.tex

clean:
	/bin/rm -f *.html $(TEXFILES) *.html_done *.java *.txt *~ *.pdf *.aux *.dvi *.out *.toc *.log 


