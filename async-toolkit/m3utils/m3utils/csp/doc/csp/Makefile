all: csp.pdf

csp.pdf: csp.tex dist/*.tex
	pdflatex csp.tex
	pdflatex csp.tex
	pdflatex csp.tex

clean:
	/bin/rm -f *~ *.aux *.log *.out *.toc *.log *.pdf
