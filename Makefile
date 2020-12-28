ccmd = ghc -threaded --make
title = chorgram
debug = -auto-all -caf-all -rtsopts
ccdebug = $(ccmd) -Wall -threaded --make $(debug)
profiling = -prof -auto-all -caf-all
cfgdir = $(shell find . -type d -name 'aux' -printf "%P\n")
cfgfile = chorgram.config
hkcpath := hknt-1.0
petripath := $(shell find . -type d -name petrify -printf "%P\n")/bin
experimentsdir = $(shell find $(cfgdir) -name experiments -printf "%P\n")
logdir = ./experiments
logfile = $(logdir)/experiments.log
os := $(shell uname -s)
gitmsg = "checkpoint"

#
# Examples of usage of scripts:
# python cfsm2gc.py -l -df png -dir <path-to-results-directory> <path-to-file>
# gc.py --dot "Tpng" --dot "Gsplines=ortho" --sloppy -dir <path-to-results-directory> <path-to-file>
# cfsm2gc.py -sn -D det -dir /tmp ~/Dropbox/chorgram_experiments/gmc/bank > /tmp/b.txt
# minimise -v -D det -d experiments/open_chor/ experiments/open_chor/ex4_1.fsa
# gc2fsa ~/Dropbox/chorgram_experiments/examples_gc/two_buyers_protocol.gc > experiments/results/two_buyers_protocol/sys.fsa
# gc2pom -l 2 --gml -d /tmp/ experiments/test/atm.gc
# gc2gml -o t.x experiments/test/atm.gc 
# pom2gc -d /tmp experiments/jlamp2020/csl7/cc3/closure/0.graphml 
# gc2dot -d /tmp ~/Dropbox/chorgram_experiments/examples_gc/two_buyers_protocol.gc 
# gc2dot -d /tmp/ -fmt gmldiff /tmp/t.graphml


# gmc dependencies
# compile: gmc.hs BuildGlobal.hs GCParser.hs SystemParser.hs PomsetSemantics.hs gc.hs sysparser.hs minimise.hs gc2fsa.hs gc2pom.hs pom2gc.hs minimise.hs gc2gml.hs
#	$(MAKE) all

all:
	$(MAKE) gmc_hs &&\
	$(MAKE) BuildGlobal_hs &&\
	$(MAKE) PomsetSemantics_hs &&\
	$(MAKE) gc_hs &&\
	$(MAKE) sysparser_hs &&\
	$(MAKE) minimise_hs &&\
	$(MAKE) gc2pom_hs &&\
	$(MAKE) pom2gc_hs &&\
	$(MAKE) gc2fsa_hs &&\
	$(MAKE) gc2dot_hs &&\
	$(MAKE) gc2gml_hs

gmc_hs: gmc.hs SystemParser.hs FSA.hs CFSM.hs TS.hs Representability.hs Misc.hs DotStuff.hs BranchingProperty.hs PetrifyBridge.hs
	$(ccmd) $<

PomsetSemantics_hs: PomsetSemantics.hs CFSM.hs SyntacticGlobalGraphs.hs Misc.hs DotStuff.hs
	$(ccmd) $<

BuildGlobal_hs: BuildGlobal.hs PetriNet.hs Misc.hs GlobalGraph.hs
	$(ccmd) $<

PetriNet_hs: PetriNet.hs Misc.hs
	$(ccmd) $<

gc_hs: gc.hs Misc.hs DotStuff.hs GCParser.hs RGCParser.hs CFSM.hs SyntacticGlobalGraphs.hs ErlanGC.hs HGSemantics.hs BCGBridge.hs DotStuff.hs
	$(ccmd) $<

RGCParser_hs: RGCParser.hs Misc.hs DotStuff.hs GCParser.hs CFSM.hs SyntacticGlobalGraphs.hs ErlanGC.hs HGSemantics.hs BCGBridge.hs DotStuff.hs
	$(ccmd) $<

sysparser_hs: sysparser.hs SystemParser.hs
	$(ccmd) $<

minimise_hs: minimise.hs Misc.hs FSA.hs CFSM.hs DotStuff.hs SystemParser.hs
	$(ccmd) $<

gc2pom_hs: gc2pom.hs Misc.hs GCParser.hs PomsetSemantics.hs
	$(ccmd) $<

pom2gc_hs: pom2gc.hs Misc.hs PomsetSemantics.hs SyntacticGlobalGraphs.hs DotStuff.hs
	$(ccmd) $<

gc2fsa_hs: gc2fsa.hs Misc.hs GCParser.hs CFSM.hs FSA.hs SyntacticGlobalGraphs.hs
	$(ccmd) $<

proj_hs: proj.hs Misc.hs GCParser.hs CFSM.hs FSA.hs SyntacticGlobalGraphs.hs
	$(ccmd) $<


gc2dot_hs: gc2dot.hs Misc.hs PomsetSemantics.hs SyntacticGlobalGraphs.hs DotStuff.hs GCParser.hs
	$(ccmd) $<

gc2gml_hs: gc2gml.hs Misc.hs SyntacticGlobalGraphs.hs GCParser.hs
	$(ccmd) $<

debug:
	$(ccdebug) gmc_hs &&\
	$(ccdebug) BuildGlobal_hs &&\
	$(ccdebug) GCParser_hs &&\
#	$(ccdebug) KGparser_hs &&\
	$(ccdebug) SystemParser_hs &&\
	$(ccdebug) PomsetSemantics_hs &&\
	$(ccdebug) gc_hs &&\
	$(ccdebug) minimise_hs &&\
#	$(ccdebug) hgsem_hs &&\
	$(ccdebug) sysparser_hs\
	$(ccdebug) gc2pom_hs &&\
	$(ccdebug) pom2gc_hs &&\
	$(ccdebug) gc2fsa_hs &&\
	$(ccmd) gc2dot_hs &&\
	$(ccdebug) gc2gml_hs

# To get the stack trace, add +RTS -xc at the end of the gmc or BuildGlobal command
prof:
	$(ccmd) $(profiling) gmc.hs && ghc --make  $(profiling) BuildGlobal.hs

clean:
	@rm -f *~ *.o *.hi SystemParser.* GCParser.* KGparser.* gmc gc BuildGlobal sysparser $(cfgfile) *.info *.log
	$(info >>> cleaning done.)

parser:
	happy -a -i  GCGrammar.y -o GCParser.hs && $(ccmd) GCParser.hs
	happy -a -i  SystemGrammar.y -o SystemParser.hs && $(ccmd) SystemParser.hs
	happy -a -i  RGCGrammar.y -o RGCParser.hs && $(ccmd) RGCParser.hs
#	happy -a -i  KGCrammar.y -o KGparser.hs && $(ccmd) KGparser.hs

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
	@echo "logfilename\t"$(logfile) >> $(cfgdir)/$(cfgfile)
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
	# make hp
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
