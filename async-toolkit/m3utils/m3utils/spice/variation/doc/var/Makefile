.SUFFIXES: .pdf .ps .dvi .tex .fig .eps .png .eps_bz2 .jpg
DOC=var

MAXGRAPHICS=500000
DVIPS=dvips
FIG2DEV=/usr/intel/pkgs/xfig/3.2.8b/bin/fig2dev

GENFIGURES=ringosc.eps invring.eps invring1.eps

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
	$(FIG2DEV) -L eps $*.fig $*.eps
	$(FIG2DEV) -L png $*.fig $*.png

.fig.png:
	$(FIG2DEV) -L png $*.fig $*.png

clean::
	rm -f $(DOC).ps $(DOC).pdf $(TARGETS) *.log *.aux *~ *.dvi *.toc $(GENFIGURES) $(DOC2).ps $(DOC2).pdf *.lof *.lot
	echo *.fig | xargs -n1 | sed 's/fig$$/png/' | xargs rm -f 


