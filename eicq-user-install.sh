#! /bin/bash
## eicq-user-install.sh   -*-Shell-script-*-
## $Id$

## Copyright (C) 2000, 2001 Steve Youngs


## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

#----------------------------------------------------------------
# Commentary:
#    Installs the necessary files in your home directory to run eicq
#----------------------------------------------------------------

# Variables and constants
MKDIR=/bin/mkdir
INSTALL=/usr/bin/install
RCFILE=/usr/local/lib/xemacs/site-packages/etc/eicq/world
RCDIR=${HOME}/.eicq

# Code
$MKDIR $RCDIR
$INSTALL -m 600 $RCFILE $RCDIR

echo "Don't forget to edit ${RCDIR}/world to your requirements"

# eicq-user-install.sh ends here.
