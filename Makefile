ccmd = ghc -threaded --make
title = chorgram
debug = -auto-all -caf-all -rtsopts
ccdebug = $(ccmd) -Wall -threaded --make $(debug)
profiling = -prof -auto-all -caf-all
cfgdir = ~/
cfgfile = .chorgram.config
hkcpath := $(shell find . -type d -name 'hknt*')
petripath := $(shell find . -type d -name petrify)/bin
experimentsdir = ./Dropbox/chorgram_experiments/
logdir = ./experiments/experiments.csv
os := $(shell uname -s)
gitmsg = "checkpoint"

#
# Example of usage of scripts:
# python cfsm2gg.py -l -df png -dir <path-to-results-directory> <path-to-file>
# sgg.py --dot "Tpng" --dot "Gsplines=ortho" --sloppy -dir <path-to-results-directory> <path-to-file>
#


# compile: gmc.hs BuildGlobal.hs GGParser.hs SystemParser.hs sgg.hs sysparser.hs
#	$(ccmd)

compile: gmc.hs BuildGlobal.hs GGParser.hs SystemParser.hs PomsetSemantics.hs sgg.hs sysparser.hs minimise.hs gg2fsa.hs gg2pom.hs pom2gg.hs minimise.hs gg2gml.hs #K KGparser.hs hgsem.hs
	$(MAKE) all

all:
	$(ccmd) gmc.hs &&\
	$(ccmd) BuildGlobal.hs &&\
	$(ccmd) GGParser.hs &&\
#K	$(ccmd) KGparser.hs &&\
	$(ccmd) SystemParser.hs &&\
	$(ccmd) PomsetSemantics.hs &&\
	$(ccmd) sgg.hs &&\
	$(ccmd) sysparser.hs &&\
	$(ccmd) minimise.hs &&\
#	$(ccmd) hgsem.hs &&\
	$(ccmd) gg2pom.hs &&\
	$(ccmd) pom2gg.hs &&\
	$(ccmd) gg2fsa.hs
	$(ccmd) chor2dot.hs &&\
	$(ccmd) gg2gml.hs

debug:
	$(ccdebug) gmc.hs &&\
	$(ccdebug) BuildGlobal.hs &&\
	$(ccdebug) GGParser.hs &&\
#K	$(ccdebug) KGparser.hs &&\
	$(ccdebug) SystemParser.hs &&\
	$(ccdebug) PomsetSemantics.hs &&\
	$(ccdebug) sgg.hs &&\
	$(ccdebug) minimise.hs &&\
#	$(ccdebug) hgsem.hs &&\
	$(ccdebug) sysparser.hs\
	$(ccdebug) gg2pom.hs &&\
	$(ccdebug) pom2gg.hs &&\
	$(ccdebug) gg2fsa.hs &&\
	$(ccmd) chor2dot.hs &&\
	$(ccdebug) gg2gml.hs

# To get the stack trace, add +RTS -xc at the end of the gmc or BuildGlobal command
prof:
	$(ccmd) $(profiling) gmc.hs && ghc --make  $(profiling) BuildGlobal.hs

clean:
	@rm -f *~ *.o *.hi SystemParser.* GGParser.* KGparser.* gmc sgg BuildGlobal sysparser $(cfgfile) *.info *.log
	$(info >>> cleaning done.)

parser:
	happy -a -i  GGGrammar.y -o GGParser.hs && $(ccmd) GGParser.hs
	happy -a -i  RGGGrammar.y -o RGGParser.hs && $(ccmd) RGGParser.hs
	happy -a -i  SystemGrammar.y -o SystemParser.hs && $(ccmd) SystemParser.hs
#K	happy -a -i  KGGrammar.y -o KGparser.hs && $(ccmd) KGparser.hs

config:
	@echo "experiments\t"$(experimentsdir) > /tmp/$(cfgfile)
	@echo "logfilename\t"$(logdir) >> /tmp/$(cfgfile)
	@echo "hkc\t"$(hkcpath) >> /tmp/$(cfgfile)
	@echo "petrify\t"$(petripath) >> /tmp/$(cfgfile)
	@echo "gmc\t./gmc" >> /tmp/$(cfgfile)
	@echo "bg\t./BuildGlobal" >> /tmp/$(cfgfile)
	@echo "base\t./choreography/chorgram/" >> /tmp/$(cfgfile)
	@echo "dot\taux/dot.cfg" >> /tmp/$(cfgfile)
	@mv /tmp/$(cfgfile) $(cfgdir)$(cfgfile)
	$(info >>> config file created $(cfgdir)$(cfgfile))

hp:
	@if test -e $(hkcpath)/hkc; then echo ">>> The binary of hkc is already there. Nothing to be done."; else make -C $(hkcpath); echo ">>> hkc compiled"; fi
	@if test -e $(hkcpath)/hkc$(os); then echo ">>> The link to hkc is already there. Nothing to be done."; else (cd $(hkcpath); ln -s hkc hkc$(os)) ; echo ">>> link to petrify added"; fi
	@if test -e $(petripath)/petrify$(os); then echo ">>> The link to petrify is already there. Nothing to be done."; else (cd $(petripath); ln -s petrify petrify$(os)); fi

setup:
	make config
	make hp
	make parser
	make all

e:
	e -T $(title) gmc.hs &

git:
	git pull
	git commit -am $(gitmsg) && git push

alpha:
	rm -f pre-alpha/*
	cp Makefile *.hs *.y *.py pre-alpha
