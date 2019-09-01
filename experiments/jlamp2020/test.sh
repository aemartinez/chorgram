#!/bin/sh
rm -rf cc*
rm -rf global_view
rm -rf global_view_corrected
rm -rf proj
../../../gg2pom -d . --gml global_view.sgg 
../../../cc/dirpoms.py --draw --graphml global_view cc2 cc3 proj
../../../cc/gml2fsa.py -o proj/system.fsa ./proj

