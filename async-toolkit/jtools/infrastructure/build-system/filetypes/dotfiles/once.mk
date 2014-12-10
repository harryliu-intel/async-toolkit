#Rule to convert a postscript file to an encapsulated postcript file.
#Since the .dotoutput.ps file is an intermediate file and
#the .eps file is a result file they will both be in the target
#directory hierarchy thus we don't need to generate a implicit
#rule for each directory.
%.eps: %.dotoutput.ps
	gs -sDEVICE=epswrite -sOutputFile=$@ -dBATCH -dNOPAUSE $<


