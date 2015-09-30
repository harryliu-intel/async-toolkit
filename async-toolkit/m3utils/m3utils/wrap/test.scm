(require-modules "m3")
(Wrap.SelectBetweenAts "@A=x>@ dog @<A=x@" "A" "x")
(Wrap.SelectBetweenAts "@A=x>@ dog @<A=x@ @A=y>@ cat @<A=y@" "A" "x")

