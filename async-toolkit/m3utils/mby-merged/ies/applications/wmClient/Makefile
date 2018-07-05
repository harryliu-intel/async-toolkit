# vim:noet:sw=4:syntax=make:ts=4:tw=79

all:
	#gcc src/sbiosf.c -ggdb -std=c99 -I ./include -o sbiosf
	gcc -c src/crc32.c -ggdb -std=c99 -I ./include -I ../../include -o crc32.o
	gcc -c src/ies_sbiosf.c -ggdb -std=c99 -I ./include -I ../../include -o ies_sbiosf.o
	gcc -c src/ies_sbiosf_wm.c -ggdb -std=c99 -I ./include -I ../../include -o ies_sbiosf_wm.o
	gcc -c src/sbiosf.c -ggdb -std=c99 -I ./include -I ../../include -o sbiosf.o
	gcc -c src/clientLib.c -ggdb -std=c99 -I ./include -I ../../include -o clientLib.o
	gcc -c src/client.c -ggdb -std=c99 -I ./include -I ../../include -o client.o
	gcc -c src/l2.c -ggdb -std=c99 -I ./include -I ../../include -o l2.o
	gcc -c src/nvm_boot.c -ggdb -std=c99 -I ./include -I ../../include -o nvm_boot.o
	gcc client.o clientLib.o sbiosf.o ies_sbiosf.o ies_sbiosf_wm.o -o client -lpthread
	gcc l2.o clientLib.o sbiosf.o ies_sbiosf.o ies_sbiosf_wm.o crc32.o -o l2 -lpthread
	gcc nvm_boot.o clientLib.o sbiosf.o ies_sbiosf.o ies_sbiosf_wm.o crc32.o -o nvm_boot -lpthread
	gcc -c src/pktsend.c -D_GNU_SOURCE -ggdb -std=c99 -I ./include -I ../../include -o pktsend.o
	gcc pktsend.o clientLib.o sbiosf.o ies_sbiosf.o ies_sbiosf_wm.o crc32.o -o pktsend -lpthread
	gcc -c src/gen_parser_cfg.c -ggdb -std=c99 -o gen_parser_cfg.o
	gcc gen_parser_cfg.o -o gen_parser_cfg
