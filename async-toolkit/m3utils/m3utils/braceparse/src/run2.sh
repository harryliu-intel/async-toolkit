time ../AMD64_LINUX/netlist2goxrpt -f example2.net -t transistor.cells -T gox -w '(lambda(nfin) (if (= nfin 1) 30000 (- (* 30000 nfin) 22000)))' -l 1 -r test5
