#/* -*- Mode: Makefile -*- */
# split testfile
# csplit --silent --suffix-format="%02d.tst" --prefix=z test.sce "/example/" "{*}"

SHELL=/bin/sh
.SUFFIXES: .tst .nsp .xmi .sce .x

GMC=/usr/local/src/projet-P/gmc-git/bin/gmc
GMC=/usr/local/src/projet-P/gmc-git/bin/qgenc
NSP=/usr/local/src/nsp2/bin/nsp

OBJS=$(patsubst %.tst,%.nsp,$(wildcard *.tst))

all: $(OBJS)

%.nsp : %.tst
	@echo "running test for $*.sce"
	@$(NSP) -nw -e "logfile='$*';exec('$<');exec('code.sce');quit"
	@$(GMC) $*.xmi  --clean --steps d  -l c > /dev/null

%.x   : %.xmi
	@$(GMC) $*.xmi  --clean --steps d  -l c

clean:
	$(RM)  *.nsp *.xmi *.c
	$(RM) -r *.xmi_generated

SUBDIRS=$(wildcard *.xmi_generated)

compile :
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i && echo "compile nsp.c in $$i " && \
		gcc -std=c99   -c -o nsp.o nsp.c ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "Cannot make $@ in sub directory $$d";\
	       	  exit $$IER;;\
	   	esac;\
	done
