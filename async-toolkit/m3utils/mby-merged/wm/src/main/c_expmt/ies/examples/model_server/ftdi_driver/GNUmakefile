ifdef _parallel
include Makefile
else

define make_parse
compute_jnum() {\
	if ! /bin/uname -r | awk -F. '{ if ( $1 < 3 || ( $1 == 3 && $2 < 14 ) ) exit 1 }'; then\
		available=$(awk -F'[: ]+' '/^(MemFree|Active\(file\)|Inactive\(file\)|SReclaimable)/ { val=val+$2 } END { print val }' /proc/meminfo);\
		low=$(awk '/low/ {val+=$2} END { print val } ' /proc/zoneinfo);\
		page_size=$(($(getconf PAGE_SIZE) / 1024));\
		available=$((available - (low * page_size)));\
	else\
		available=$(awk -F'[: ]+' '/^MemAvailable/ { print $2 }' /proc/meminfo);\
	fi; \
\
	nproc=$(($(/usr/bin/nproc)+1));\
	size=180000;\
	required=$((size * nproc));\
\
	if [ "$available" -le "$size" ]; then\
		nproc=1;\
	elif [ "$available" -le "$required" ]; then\
		nproc=$((available/size));\
	fi; \
};\
\
recurse() {\
	pid_cmdline="/proc/$1/cmdline";\
	comm=$(awk -F'\0' '{  sub(/.*\//,"",$1) ; print $1}'  <"$pid_cmdline");\
	pid_status="/proc/$1/status";\
\
	if [ "$comm" != make ]; then\
		return 1;\
	elif awk -F'\0' -vexitcode=1 '{ for (i = 1; i <= NF ; i++) if ( $i ~ "-j[0-9]*" ) { exitcode=0 ; break } } END { exit exitcode }' <"$pid_cmdline"; then\
		return 0;\
	else\
		recurse "$(awk '$1 == "PPid:" { print $2 ; exit }' <"$pid_status")";\
	fi; \
};\
\
if recurse $PPID; then\
  "$make" -f Makefile $makecmdgoals _parallel=1;\
else\
  compute_jnum;\
  "$make" -j$nproc -f Makefile $makecmdgoals _parallel=1;\
fi
endef

.PHONY: default $(MAKECMDGOALS) force
default:
	@make="$(MAKE)"; $(value make_parse)

$(MAKECMDGOALS): force
force:
	@make="$(MAKE)" makecmdgoals="$(MAKECMDGOALS)"; $(value make_parse)
.SUFFIXES:
endif

GNUmakefile: ;
