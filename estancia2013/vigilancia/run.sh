# SitLog (Situation and Logic)
#
# Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
# Ivan Meza (http://turing.iimas.unam.mx/~ivanvladimir)
# Caleb Rascón (http://turing.iimas.unam.mx/~caleb)
# Lisset Salinas (http://turing.iimas.unam.mx/~liz)
# Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
# Luis Pineda (http://turing.iimas.unam.mx/~luis)

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#!/bin/bash

# Executes dialogue manager
SICSTUS=/usr/local/bin/sicstus
SITLOG=/home/equipo2/SitLog/
PROJECT="."
MODELS="models"
MAIN="main"
InArg="now"
MESSAGE="logfile.txt"
SAVE="log"
KB="../../../knowledge_base/KB.pl"
KnowledgeBase="knowledge_base/KB.pl"

$SICSTUS -l $SITLOG/src/init --goal start_init. -a -v structlog -m test -d $MESSAGE --save_dir $SAVE $PROJECT $MAIN $InArg $MODELS  $KB $SITLOG


