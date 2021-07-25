# Welcome

ChorGram is a tool chain to support choreographic development of message-oriented applications. The tool kit started originally to support the experimental work related to the theory introduced in this [PoPL 2015 paper](https://doi.org/10.4204/EPTCS.203.7). New features have been added since the initial prototype; in particular, the chain now supports the semantics of choregraphies defined in this [ICE 2016 paper](https://doi.org/10.4204/EPTCS.223.5) and its [journal version](https://authors.elsevier.com/c/1W8zs8MrKMC5ww). A tutorial is also in [chapter 6](https://www.riverpublishers.com/book_details.php?book_id=439) of [Behavioural Types: from Theory to Tools](https://www.riverpublishers.com/book_details.php?book_id=439), a book published by River Publishers.
Recently [Roberto](http://www.csc.kth.se/~robertog/) and [Emilio](https://cs.gssi.it/emilio.tuosto) started to integrate into ChorGram the pomset-based analysis of choregraphies based on  [Roberto](http://www.csc.kth.se/~robertog/)'s [COORDINATION 2019 paper](https://doi.org/10.1007/978-3-030-22397-7_14) and our [JLAMP 2018 paper](https://doi.org/10.1016/j.jlamp.2017.11.002). The integration consists of the files in the subdirectory './cc/', but soon we will provide a more coherent integration of all the stuff above. This integration will also include our recent tool for choreographic model-based testing development by [Alex Coto](https://www.gssi.it/people/students/students-computer-science/item/4803-coto-alex),  [Roberto](http://www.csc.kth.se/~robertog/) and [Emilio](https://cs.gssi.it/emilio.tuosto) whose results where presented at in this [ICE 2020 paper](https://doi.org/10.1007/978-3-030-61362-4_2) and this [ISoLA 2020 paper](https://doi.org/10.1007/978-3-030-61362-4_2).

You might be also interested in our tutorial at [COORDINATION 2020](https://doi.org/10.1007/978-3-030-50029-0_2).

[Emilio](https://cs.gssi.it/emilio.tuosto) also started to develop the automatic generation of Erlang executables and monitor-based reversibility introduced in this [DAIS 2018 paper](https://doi.org/10.1007/978-3-319-93767-0_6), but he feels to ashemed to make it public; he is trying to improve that part, but contact him if interested.

# Forewords
ChorGram is a polished and extended version of [gmc-synthesis](https://bitbucket.org/julien-lange/gmc-synthesis) and of [gmc-synthesis-v0.2](https://bitbucket.org/emlio_tuosto/gmc-synthesis-v0.2); those old versions are deprecated. Since the intial prototype we have

- experimented new features
- improved the usability (e.g., better feedback to the user when generalised multiparty compatibility is violated),
- added new features to
  - manipuate/analyse the TS (eg highlighting of configurations with some properties; click [here](http://www.doc.ic.ac.uk/~jlange/demo.tar.gz) for examples of graphical outputs)
  - added pomset based algorithms

Although many features are almost stable, a few are still work in progress (and new variants are often attempted on stable features too).

The main commands provided by the implementation are

- **gmc**: takes in input a CFSM system, checks it for generalised multiparty compatibility, and builds the corresponding global graph
- **chorgram**.py: a python script that executes gmc, transforms the .dot files it generates in graphical formats, and displays some performance information
- **sgg**: takes in input a description of a global graph (with an extended syntax), projects the graph in a set of (non-minimal) CFSMs
- **project**: takes in input a description of a global graph (with an extended syntax) and projects the graph on all or some of its participants
- **gg2fsa**: returns the .fsa format of the minimised CFSMs of a global graph (the '-v' option also generates a '.hs' file that is the haskell representation of the system)
- **gg2pom**: takes in input a description of a global graph (with an extended syntax) and generates the pomsets (in graphml format) representing the semantics of the graph (cf. (R. Guanciale and E. Tuosto, ICE 2016) and its journal version **(E. Tuosto and R. Guanciale, 2018)**
- **pom2gg**: takes in input a description of pomsets (in graphml format) and tries to compute a global graph for which the pomset in input is the semantics
- **gg2gml**: transforms a description of a global graph (with an extended syntax) to the graphml format
- **chor2dot**: takes in input a description of a choreography either in the syntax of ChorGram (.sgg files), or as a graphml file of a pomset, or a graphml file obtained by diffing choreographies as a result of 'diff.py', and generates a corresponding to dot file

# Syntax of the input files

Strings are made of the following characters

   0123456789<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz/$#&~()\"

and must start with a letter when specifying the name of a machine (non-terminal Ptp) or of a system. Reserved characters not usable in strings are:

   @ . , ; : ( ) [ ] { } | + * ! ? - % §



## GMC
The syntax of the input files of gmc can be either in the 'fsa' (after finite state automata) or 'cms' format, the latter being the simple process-algebraic syntax described below. Which format is used depends on the extesion to the file name ('.fsa' or '.cms' respectively).

The 'fsa' format is a text where each line describes (some element of) a CFSM: for instance,
```
#!ABNF

.outputs A
.state graph
q0 1 ! hello q1
q0 1 ! world q1
.marking q0
.end

.outputs
.state graph
q0 0 ? hello q1
q0 0 ? world q1
.marking q0
.end
```

specifies a system of two machines.
- machine 0 is named A (line '.outputs A'), has q0 as initial state (line .marking q0) from which it may send either a message 'hello' or a message 'world' to machine 1 and move to state q1 (lines 'q0 1 ! hello q1' and 'q0 1 ! world q1', respectively). Messages can be any sort for string; the current implementation does not interpret them, but in the future we plan to fix a syntax and a semantics in order to deal with e.g., variables, binderds, values, etc.
- machine 1 is ready to receive either message sent by machine 0.

Machine names are optional (e.g., machine 1 is anonymous in the example above); if present in the input file, they are used to denote the channels in the files generated by the tool (for instance, the first transition of A is displayed as "AB ! hello" rather than "01!hello". Lines '.outputs ...', '.state graph', '.marking ...', and '.end' are mandatory. Lines starting with '--' are ignored.

The 'cms' format is probably more appropriate for systems consisting of many machines. The idea is that each CFSM of a system is described by a simple process in the following syntax:

Text surrounded by square brackets or between '..' and a newline is treated as comment.

```
#!ABNF

    S     ::= 'system' str 'of' Ptp, ..., Ptp ':' Ptp '=' M || ... || Ptp '=' M
    M     ::= end
           |  p ';' M
           |  p 'do' Ptp
           |  M '+' M
           |  '*' M
    p     ::= Ptp '!' str | Ptp '?' str | 'tau' str | 'tau'
```

A system S is given a name, specifies the (comma-separated list of the) names Ptps of its machines, and has a declaration Mdec for each machine. Each name in Ptps must have a unique defining equation Ptp = M where the syntax of M is straightforward. Transition are similar those of the 'fsa' format, but they use explicit names rather than positions to address machines. Moreover, it is possible to specify internal transitions 'tau' which also carry an uninterpred annotating string. Note that branches and forks have to be surrounded by round brackets (this constraint will be relaxed in the future).

The only syntactic check made (right now) during the parsing are (i) that sender and receiver of interactions have to be different, (ii) that defining equations are unique, (iii) that communication actions name existing machines, and (iv) a machine cannot be defined as its own dual. Error messages give some information, but should be improved. Text enclosed in '[' and ']' is treated as comment.

## Syntax of Global Graphs
The syntax of global graph is defined by the grammar:
```
#!ABNF

    G ::= (o)                                  * empty graph
       |  Ptp -> Ptp : str                     * interaction
       |  Ptp => Ptps : str                    * multiple interactions (syntactic sugar: A -> B1, ..., Bn : m = A -> B1: M |  ... A -> Bn : m) 
       |  G '|' G                              * fork
       |  G ';' G                              * sequential
       |  chop '{' Brc '}'                     * (possibly reversible) choice without explicit selector
       |  chop Ptp '{' Brc '}'                 * (possibly reversible) choice with explicit selector 
       |  '*' G '@' P                          * irreversible loop
       |  'repeat' '{' G 'unless' guard '}'    * (possibly reversible) loop without explicit selector
       |  'repeat' P '{' G 'unless' guard '}'  * (possibly reversible) loop with explicit selector
       |  '{' G '}'
       |  '(' G ')'                            * deprecated

	chop ::= 'sel' | 'branch' | 'choice'

    Brc ::= G | G unless guard | Brc + Brc

    guard ::= P % str | P % str, guard

    Ptps ::= P , Ptps  |  P
```

which shuffles the syntaxes presented in various papers and extends them with some syntactic sugar.

The empty graph '(o)' has a special role: it is the empty graph outside loops, while in loops it marks a point where the selector may exit the iteration. Guards are used only for the reversible semantics and the string in them is supposed to be some valid erlang code. Likewise for the sel construct, which generalises the choice for the reversible semantics. Notice that the sel and the branch constructs have the same semantics and require to specify the selector of the branch (to make it simple the realisation of projections on Erlang; the selector is mandatory for REGs and optional otherwise).

The clause 'unless guard' is optional in branching and iteration.

When  reversibility is not assumed

   sel P { Gn unless g1 + ... + Gn unless gn } = G1 + ... + Gn      for all guards g1, ..., gn 
   repeat P {G unless g}                       = * G @ P            for all guards g

The binary operators |, +, and ; are given in ascending order of precedence.

Text enclosed by '[' and ']' and is treated as comment and, after '..', so is the rest of a line.

Basic syntactic checks are made during the parsing (e.g, (i) that sender and receiver of interactions have to be different and (2) that the participant controlling a loop is active in the loop). More are planned together with some more informative error messages.


A global graph representing the ping-pong protocol in this syntax could be
```
#!python

Ping -> Pong : finished
+
*{
   Ping -> Pong : ping ; Pong -> Ping : pong
} @ Ping ; Ping -> Pong : finished

```
(recall that ';' has priority over '+').

# How to install ChorGram
At the moment ChorGram is available for linux-like architectures only.
So far, ChorGram has been tested on Ubuntu 13.04, 14.04, 16.04 and Mac OS X (v10.9).


## Required Tools/ Libraries
The following tools and libraries are required.

* graphviz (and python-numpy if you would like to run the benchmarks scripts)

You can install via the command "sudo apt-get install graphviz" on
ubuntu/debian

- [Haskell](http://www.haskell.org/platform/) platform

- MissingH and hxt (Haskell libraries)
  You can install them via the command:
```
#!

cabal install MissingH hxt
```

- [HKC](http://perso.ens-lyon.fr/damien.pous/hknt/) (slight variation on git)

- [Petrify](http://www.lsi.upc.edu/~jordicf/petrify/)

Note that if you have a recent version of Linux, you have to install libc6:i386:

```
#!bash
sudo dpkg --add-architecture i386
sudo apt update
sudo apt install libc6:i386
```

thanks to [Zanna](https://askubuntu.com/questions/1028358/how-can-i-install-petrify-and-set-it-up) for the tips.

Besides the command line options, there is a configuration file 'aux/dot.cfg' that can be used to tune up dot files. The format of the file is very basic: each line is a blank-separated pair of key / value and an optional comment (starting with ':' after each value). You will probably need to change just the 2nd column by setting your preferred value; for instance, to change to simbol separating local states in configurations to '++' you've to change the value of 'statesep': making the line look like

```
#!bash
'statesep		++		:symbol to separate states in output files'
```

Unless you know what you're do doing, do not add lines to
'aux/dot.cfg'. If you need to edit the file other than for altering
the values as described, remember the following rules:

- do not insert text before  the very first line. 
- do not insert empty lines
- do not insert lines with the very same initial word

## Configuration

Move the compressed files of [HKC](http://perso.ens-lyon.fr/damien.pous/hknt/) and [Petrify](http://www.lsi.upc.edu/~jordicf/petrify/) in the directory ChorGram; any subdirectory would do, but it is advisable to put them in 'aux'.

Decompress the files of HKC and Petrify (for Linux these are 'hknt-1.0.tar.bz2' and 'petrify-5.2-linux.tar.gz').


## Compilation
Once both the directories of [HKC](http://perso.ens-lyon.fr/damien.pous/hknt/) and [Petrify (http://www.lsi.upc.edu/~jordicf/petrify/) are ready and in the directory ChorGram, you simply
```
#!bash

1. from a shell cd in the directory ChorGram
2. make setup

```

If you want, you can compile [HKC](http://perso.ens-lyon.fr/damien.pous/hknt/) yourself by going into the directory of [HKC](http://perso.ens-lyon.fr/damien.pous/hknt/) and typing make (you will need OCaml, ./hknt-1.0/README).


# Running
## Executing cfsm2gg.py
The Python script 'cfsm2gg.py' offers a command-line interface to gmc. For an overview of its usage, get the help message on the tool via the command: 

```
#!python

python cfsm2gg.py --help 
```
which prints on the screen the following message:

```
#!bash

usage: cfsm2gg.py [-h] [-v] [-shh] [-df DF] [--dot DOT] [-l] [-sn] [-dw DW]
                  [-ts] [-tp TP] [-cp CP] [-p PATH] [-b BOUND] [-nc] [-pn PN]
                  [-hkc HKC] [-gmc GMC] [-bg BG] [-dir DIR] [-m MUL] [-D D]
                  filename

chorgram: From communicating machines to graphical choreographies

positional arguments:
  filename              Specify the path to file containing the CFSMs

optional arguments:
  -h, --help            show this help message and exit
  -v, --verbose         Run in verbose mode
  -shh                  Switches off verbose mode
  -df DF                Output format from dot files (svg, png, pdf, etc.)
                        {default = svg}
  --dot DOT             Options for dot starting without '-' (e.g., --dot
                        Nnodesep=.5). Use 'none' if no manipulation of dot
                        files is required
  -l                    Generates a legend from dot files
  -sn                   Suppresses simple names for states
  -dw DW                Set fixedsize of dot nodes to the given value {default
                        = 0}
  -ts                   Just computes the CFSMs and the transition system(s)
  -nf, --bag            Disable FIFO policy: buffers become bags
  -tp TP                Pattern for colouring transitions; the syntax is "s r
                        d msg" where s and r are the indexes of sender and
                        receiver, d is the action, and msg is the message
                        {default = "- - - -"}
  -cp CP                Pattern for colouring configurations; the syntax is a
                        string with blank-separated local state ids or '*' (as
                        many as the number of machines) and then some blank
                        separated string of the forom "s r" where s and r are
                        the indexes of sender and receiver {default = ""}
  -p PATH, --path PATH  Colours paths from the initial node to the ones
                        matching the configuration pattern PATH {default: ""}
  -b BOUND, --bounded BOUND
                        Set the bound to BOUND; if BOUND < 1, the synchronous
                        TS is computed {default: 0}
  -nc, --noclean        Do not remove auxiliary files
  -pn PN                Specify the path to petrify {default:
                        ./aux/petrify/bin/petrifyLinux}
  -hkc HKC              Specify the path to hkc {default:
                        ./aux/hknt-1.0/hkcLinux}
  -gmc GMC              Specify the path to gmc {default: ./gmc}
  -bg BG                Specify the path to BuildGlobal {default:
                        ./BuildGlobal}
  -dir DIR              Specify the directory for the output files {default:
                        outputs}
  -m MUL                Specify the multiplicity factor [DEPRECATED]
  -D D                  Applies determinisation if D = det, minimisation if D
                        = min, or nothing otherwise
```

## Getting the results

Upon execution with the '-v' option, 'cfsm2gg.py' displays some information about the files and the execution time and produces some files as results. Such files are collected in a directory that can either be a default one (currently the directory 'experiments/results/' followed by the basename of the input file) or the one you specify with the option 'dir' plus the base name of the input file. For example, if you run 

```
#!bash
python cfsm2gg.py -v -nc -dw 0 -df pdf -dir experiments/results/ experiments/pingpong.fsa 
```
you get a message like
```
chorgram: Execution Started on Sat, 15 Oct 2016 09:18:18 +0000
gmc:	Parsing CFSMs file...experiments/pingpong.fsa
gmc:	dir "experiments/results/pingpong/"
gmc:	Synchronous TS: (nodes 3, transitions 3)
gmc:	1-bounded TS:	(nodes 6, transitions 6)
gmc:	Branching representability:     []
gmc:	Branching Property (part (ii)): []
chorgram: Language-equivalence (Representability part (i))? True
gg:     Global graph synthesis
chorgram: All done.
	Total execution time: 0.0250358581543
		GMC check:                         1.90734863281e-05
		HKC minimisation:                  0.00535798072815
		Petrify:                           0.00365400314331
		Global graph generation:           0.00565099716187
```

on the screen and the following files are generated in the directory 'experiments/results/pingpong'


```
#!bash
1. pingpong_global.pdf
2. pingpong_machine_0
3. pingpong_machine_1
4. pingpong_machines.dot
5. pingpong_machines.pdf
6. pingpong_petrinet
7. pingpong_petrinet_finalpn.dot
8. pingpong_petrinet_global.dot
9. pingpong_petrinet_onesourcepn.dot
10. pingpong_petrinet_preglobal.dot
11. pingpong_projected
12. pingpong_projection_0
13. pingpong_projection_0.dot
14. pingpong_projection_0.pdf
15. pingpong_projection_1
16. pingpong_projection_1.dot
17. pingpong_projection_1.pdf
18. pingpong_toPetrify
19. pingpong_ts0.dot
20. pingpong_ts0.pdf
21. tempefc

```
The important files are 1, 4, 5, 10, 19, and 20 which respectively are the global graph in pdf format (1), the CFSM machines in dot (4) and pdf (5) format, the global graph in dot format (10), and the transition system in dot (19) and pdf format (20).

Here is an (ugly) example of the 1-bounded transition system of the pingpong protocol generated with the command


```
#!bash

python cfsm2gg.py -df png -b 1 --dot "Nshape=parallelogram" --dot "Gnodesep=.5" -dir out \
-p "w *" -tp "Ping Pong * *" -cp "* * Ping Pong" experiments/pingpong.fsa 
```

![pingpong_ts1.png](https://bitbucket.org/repo/BrqBML/images/436085583-pingpong_ts1.png)

this example is meant just to show how to specify conditions on nodes and transitions using the options -p, -ts, and -cp
and how to use dot options --dot (the parallelogram shape is what makes the diagram look bad). A better way to set dot preferences
is to edit the file .dot.cfg described above.
