#!/bin/sh
../../gg2pom -d . global_view.sgg
../../../ccpom/dirpoms.py --draw --graphml global_view cc2 cc3 proj

