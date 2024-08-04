.SUFFIXES: .pdf .ps .dvi .tex .fig .eps .png .eps_bz2 .jpg
DOC=lowtemp
DOC2=vernier_has

MAXGRAPHICS=5000000
DVIPS=dvips

GENFIGURES=dennard_tomb.eps R717cycle1.eps R717cycle2.eps fe.eps\
fe_compare.eps fe_density.eps fe_compare2.eps fe_cost.eps\
ringosc.eps i0m_vs_i0s.eps i0m_vs_i0s_raw.eps ms_swap.eps\
ulvtlvt.eps minimumcostwrong.eps i0s_vt_compare.eps

all: $(DOC).ps $(DOC).pdf
two: $(DOC2).ps $(DOC2).pdf 

vernier_has.dvi: meas_sync_ff.eps


$(DOC).dvi: $(GENFIGURES)

.png.eps:
	pngtopnm $*.png | pnmscale -pixels $(MAXGRAPHICS) |  pnmtops > $*.eps

.jpg.eps:
	jpegtopnm $*.jpg | pnmscale -pixels $(MAXGRAPHICS) |  pnmtops > $*.eps

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


