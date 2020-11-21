
<!--
## Executing sgg.py
The Python script 'sgg.py' offers a command-line interface to sgg. For an overview of its usage,
get the help message via the command: 

```
#!python

python sgg.py --help 
```

### Getting the results

Upon execution, 'sgg.py' displays some information about the files and produces some files as results. Such files are collected in a directory that can either be a default one (currently the directory 'experiments/results/' followed by the basename of the input file) or the one you specify with the option 'dir' plus the base name of the input file. For example, if you run 

```
#!bash
python sgg.py experiments/examples_sgg/pingpong.sgg
```
you get the message 
```
sgg: start
	experiments/results/pingpong/in.sgg: is the initial gg
	experiments/results/pingpong/graph_sgg.dot: is the input gg
	experiments/results/pingpong/norm_sgg.dot:  is the normalised initial gg
	experiments/results/pingpong/fact_sgg.dot:  is the factorised initial gg
	experiments/results/pingpong/sem_sgg.dot: is the semantics of the initial gg
	experiments/results/pingpong/cfsmPing.dot is the machine for participant Ping
	experiments/results/pingpong/cfsmPong.dot is the machine for participant Pong
sgg: end
```
and, for each line between 'sgg: start' and 'sgg: end' the corresponding file is genereated (in the default directory) together with the 'svg' format of each '.dot' file. For instance, the svg format of the graphical representation of the global graph of the ping-pong example is

![pingpong_gg.svg](https://bitbucket.org/repo/rXMoye/images/1560864411-graph_sgg.png)
-->
