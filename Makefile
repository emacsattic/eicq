# Makefile for eicq code

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

VERSION = 1.03
AUTHOR_VERSION = 0.2.8
MAINTAINER = Steve Youngs <youngs@xemacs.org>
PACKAGE = eicq
PKG_TYPE = regular
REQUIRES = xemacs-base mail-lib
CATEGORY = comm

ELCS = eicq.elc eicq-toolbar.elc

DATA_FILES = $(wildcard etc/*.xpm) etc/world
DATA_DEST = $(PACKAGE)

INFO_FILES = $(PACKAGE).info
TEXI_FILES = $(PACKAGE).texi
MANUALS = $(PACKAGE)

CC = g++
CFLAGS = -O2 -Wall -static
STRIP = /usr/bin/strip

BIN = udp2tcp
USERSH = eicq-user-install.sh

LIBSRC_FILES = $(BIN) $(USERSH)

EXTRA_SOURCES = README NEWS INSTALL TODO $(BIN).cc

EXTRA_OBJS = $(BIN)

include ../../XEmacs.rules

GENERATED += custom-load.elc

all:: $(BIN) $(ELCS) auto-autoloads.elc custom-load.elc $(INFO_FILES)

udp2tcp: $(BIN).cc
	${CC} -o $(BIN) $(BIN).cc ${CFLAGS}
	${STRIP} $(BIN)

srckit: srckit-std

binkit: binkit-common


