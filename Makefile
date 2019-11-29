ccmd = ghc -threaded --make
title = chorgram
debug = -auto-all -caf-all -rtsopts
ccdebug = $(ccmd) -Wall -threaded --make $(debug)
profiling = -prof -auto-all -caf-all
cfgdir = $(shell find . -type d -name 'aux')
cfgfile = chorgram.config
hkcpath := $(shell find . -type d -name 'hknt*')
petripath := $(shell find . -type d -name petrify)/bin
experimentsdir = $(shell find $(cfgdir) -name experiments)
logfile = $(shell find . -name experiments.csv)
os := $(shell uname -s)
gitmsg = "checkpoint"

#
# Examples of usage of scripts:
# python cfsm2gg.py -l -df png -dir <path-to-results-directory> <path-to-file>
# sgg.py --dot "Tpng" --dot "Gsplines=ortho" --sloppy -dir <path-to-results-directory> <path-to-file>
# cfsm2gg.py -sn -D det -dir /tmp ~/Dropbox/chorgram_experiments/gmc/bank > /tmp/b.txt
# minimise -v -D det -d experiments/open_chor/ experiments/open_chor/ex4_1.fsa
# gg2fsa ~/Dropbox/chorgram_experiments/examples_sgg/two_buyers_protocol.sgg > experiments/results/two_buyers_protocol/sys.fsa
# gg2pom -l 2 --gml -d /tmp/ experiments/test/atm.sgg
# gg2gml -o t.x experiments/test/atm.sgg 
# pom2gg -d /tmp experiments/jlamp2020/csl7/cc3/closure/0.graphml 
# chor2dot -d /tmp ~/Dropbox/chorgram_experiments/examples_sgg/two_buyers_protocol.sgg 
# chor2dot -d /tmp/ -fmt gmldiff /tmp/t.graphml


# gmc dependencies
# compile: gmc.hs BuildGlobal.hs GGParser.hs SystemParser.hs PomsetSemantics.hs sgg.hs sysparser.hs minimise.hs gg2fsa.hs gg2pom.hs pom2gg.hs minimise.hs gg2gml.hs
#	$(MAKE) all

all:
	$(MAKE) gmc.hs &&\
	$(MAKE) BuildGlobal.hs &&\
	$(MAKE) PomsetSemantics.hs &&\
	$(MAKE) sgg.hs &&\
	$(MAKE) sysparser.hs &&\
	$(MAKE) minimise.hs &&\
	$(MAKE) gg2pom.hs &&\
	$(MAKE) pom2gg.hs &&\
	$(MAKE) gg2fsa.hs
	$(MAKE) chor2dot.hs &&\
	$(MAKE) gg2gml.hs

gmc.hs: SystemParser.hs FSA.hs CFSM.hs TS.hs Representability.hs Misc.hs DotStuff.hs Dependency.hs PartialOrderReduction.hs BranchingProperty.hs HKCBridge.hs PetrifyBridge.hs
	$(ccmd) $@

PomsetSemantics.hs: CFSM.hs SyntacticGlobalGraphs.hs Misc.hs DotStuff.hs 
	$(ccmd) $@

BuildGlobal.hs: PetriNet.hs Misc.hs GlobalGraph.hs
	$(ccmd) $@

PetriNet.hs: Misc.hs
	$(ccmd) $@

sgg.hs: Misc.hs DotStuff.hs GGParser.hs RGGParser.hs CFSM.hs SyntacticGlobalGraphs.hs ErlanGG.hs HGSemantics.hs BCGBridge.hs DotStuff.hs
	$(ccmd) $@

RGGParser.hs: Misc.hs DotStuff.hs GGParser.hs RGGParser.hs CFSM.hs SyntacticGlobalGraphs.hs ErlanGG.hs HGSemantics.hs BCGBridge.hs DotStuff.hs
	$(ccmd) $@

sysparser.hs: SystemParser.hs
	$(ccmd) $@

minimise.hs: Misc.hs FSA.hs CFSM.hs DotStuff.hs SystemParser.hs
	$(ccmd) $@

gg2pom.hs: Misc.hs GGParser.hs PomsetSemantics.hs
	$(ccmd) $@

pom2gg.hs: Misc.hs PomsetSemantics.hs SyntacticGlobalGraphs.hs DotStuff.hs
	$(ccmd) $@

gg2fsa.hs: Misc.hs GGParser.hs CFSM.hs FSA.hs
	$(ccmd) $@

chor2dot.hs: Misc.hs PomsetSemantics.hs SyntacticGlobalGraphs.hs DotStuff.hs GGParser.hs
	$(ccmd) $@

gg2gml.hs: Misc.hs SyntacticGlobalGraphs.hs GGParser.hs
	$(ccmd) $@

debug:
	$(ccdebug) gmc.hs &&\
	$(ccdebug) BuildGlobal.hs &&\
	$(ccdebug) GGParser.hs &&\
#	$(ccdebug) KGparser.hs &&\
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
	happy -a -i  SystemGrammar.y -o SystemParser.hs && $(ccmd) SystemParser.hs
	happy -a -i  RGGGrammar.y -o RGGParser.hs && $(ccmd) RGGParser.hs
#	happy -a -i  KGGrammar.y -o KGparser.hs && $(ccmd) KGparser.hs

config:
	@echo "experiments\t"$(experimentsdir) > $(cfgdir)/$(cfgfile)
	$(info .)
	@echo "logfile\t"$(logfile) >> $(cfgdir)/$(cfgfile)
	$(info ..)
	@echo "hkc\t"$(hkcpath) >> $(cfgdir)/$(cfgfile)
	$(info ...)
	@echo "petrify\t"$(petripath) >> $(cfgdir)/$(cfgfile)
	$(info ....)
	@echo "gmc\t./gmc" >> $(cfgdir)/$(cfgfile)
	$(info .....)
	@echo "bg\t./BuildGlobal" >> $(cfgdir)/$(cfgfile)
	$(info ......)
	@echo "dot\taux/dot.cfg" >> $(cfgdir)/$(cfgfile)
	$(info >>> config file created $(cfgdir)/$(cfgfile))

showconfig:
	clear
	@echo cfgdir=$(cfgdir)
	@echo hkcpath=$(hkcpath)
	@echo petripath=$(petripath)
	@echo experimentsdir=$(experimentsdir)
	@echo logfile=$(logfile)

hp:
	@if test -e $(hkcpath)/hkc; then echo ">>> The binary of hkc is already there. Nothing to be done."; else make -C $(hkcpath); echo ">>> hkc compiled"; fi
	@if test -e $(hkcpath)/hkc$(os); then echo ">>> The link to hkc is already there. Nothing to be done."; else (cd $(hkcpath); ln -s hkc hkc$(os)) ; echo ">>> link to petrify added"; fi
	@if test -e $(petripath)/petrify$(os); then echo ">>> The link to petrify is already there. Nothing to be done."; else (cd $(petripath); ln -s petrify petrify$(os)); fi

setup:
	@if test -e aux/experiments; then echo ">>> The directory experiments is already there. Nothing to be done."; else make -C aux/experiments; echo ">>> directory experiments created"; fi
	make hp
	make config
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
