.SUFFIXES: .pdf .ps .dvi .tex .fig .eps .png .eps_bz2 .jpg
DOC=vernier
DOC2=vernier_has

MAXGRAPHICS=500000
DVIPS=dvips

GENFIGURES=buffering_dff.eps buffering_pipe.eps buffering_pipe1.eps buffering_pipe2.eps buffering_pipe3.eps clkdiv.eps clock.eps des_outline.eps exevent.eps meas_sync_ff.eps measuring_dff.eps simple_dff.eps taiticks.eps bresenham.eps chronometer.eps hp5370a.eps divtiming.eps

all: $(DOC).ps $(DOC).pdf $(DOC2).ps $(DOC2).pdf 

vernier_has.dvi: meas_sync_ff.eps


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

clean::
	rm -f $(DOC).ps $(DOC).pdf $(TARGETS) *.eps *.log *.aux *~ *.dvi *.toc $(GENFIGURES) $(DOC2).ps $(DOC2).pdf
	echo *.fig | xargs -n1 | sed 's/fig$$/png/' | xargs rm -f 


