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
os := $(shell uname -s)
gitmsg = "checkpoint"

all:
	$(MAKE) GGParser.hs &&\
	$(MAKE) sgg.hs &&\
	$(MAKE) PomsetSemantics.hs &&\
	$(MAKE) gg2pom.hs &&\
	$(MAKE) BuildGlobal.hs &&\
	$(MAKE) pom2gg.hs &&\
	$(MAKE) gg2fsa.hs &&\
	$(MAKE) chor2dot.hs &&\
	$(MAKE) gmc.hs &&\
	$(MAKE) gg2gml.hs

GGParser.hs: SyntacticGlobalGraphs.hs ErlanGG.hs Misc.hs CFSM.hs sgg.hs
	$(ccmd) $@

PomsetSemantics.hs: CFSM.hs SyntacticGlobalGraphs.hs Misc.hs DotStuff.hs 
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

clean:
	@rm -f *~ *.o *.hi SystemParser.* GGParser.* RGGParser.* KGparser.* HGSemantics.hs BCGBridge.hs gmc sgg BuildGlobal sysparser $(cfgfile) *.info *.log
	$(info >>> cleaning done.)

parser:
	happy -a -i  GGGrammar.y -o GGParser.hs && $(ccmd) GGParser.hs
	happy -a -i RGGGrammar.y -o RGGParser.hs 
	happy -a -i  SystemGrammar.y -o SystemParser.hs

config:
	@echo "experiments\t"$(experimentsdir) > $(cfgdir)/$(cfgfile)
	$(info .)
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

setup:
	@if test -e aux/experiments; then echo ">>> The directory experiments is already there. Nothing to be done."; else make -C aux/experiments; echo ">>> directory experiments created"; fi
	make config
	make parser
	make all

