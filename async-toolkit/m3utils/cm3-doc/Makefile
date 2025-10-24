.SUFFIXES: .pdf .ps .dvi .tex .fig .eps .png .eps_bz2 .jpg
DOC=main
MAXGRAPHICS=500000
DVIPS=dvips

FIGTARGETS=
FIGFOLDER=fig
GENFIGURES=$(addprefix $(FIGFOLDER)/, $(FIGTARGETS))

all: $(DOC).ps $(DOC).pdf 

$(DOC).dvi: $(GENFIGURES)

.png.eps:
	pngtopnm $*.png | pamscale -pixels $(MAXGRAPHICS) |  pnmtops > $*.eps

.jpg.eps:
	jpegtopnm $*.jpg | pamscale -pixels $(MAXGRAPHICS) |  pnmtops > $*.eps

.tex.dvi:
	latex $*.tex || true
	latex $*.tex || true
	latex $*.tex

.ps.pdf:
	ps2pdf $*.ps

.dvi.ps: 
	$(DVIPS) -tletter -f $*.dvi -o $*.ps

.fig.eps:
	fig2dev -L eps $*.fig $*.eps
	fig2dev -L png $*.fig $*.png

.fig.png:
	fig2dev -L png $*.fig $*.png

.pdf.eps:
	pdftops -eps $*.pdf $*.eps

clean::
	rm -f $(DOC).ps $(DOC).pdf $(TARGETS) *.eps *.log *.aux *~ *.dvi *.toc *.lof *.lot *.out $(GENFIGURES)
	echo *.fig | xargs -n1 | sed 's/fig$$/png/' | xargs rm -f 


