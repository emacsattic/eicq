# Makefile for eicq

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

VERSION = 1.00
AUTHOR_VERSION = 0.2.5
MAINTAINER = Steve Youngs <youngs_s@ozlinx.com.au>
PACKAGE = eicq
PKG_TYPE = regular
REQUIRES = mail-lib xemacs-base 
CATEGORY = comm

BIN = udp2tcp
ELCS = $(PACKAGE).elc $(PACKAGE)-toolbar.elc

EXTRA_SOURCES = INSTALL NEWS README TODO

DATA_FILES = $(wildcard etc/*)

INFO_FILES = $(PACKAGE).info
TEXI_FILES = $(PACKAGE).texi
MANUALS = $(PACKAGE)

LIBSRC_FILES = $(BIN) $(PACKAGE)-user-install.sh

CC = g++
CFLAGS = -O2 -Wall
STRIP = strip

include ../../XEmacs.rules

GENERATED += custom-load.elc

all:: $(BIN) $(ELCS) auto-autoloads.elc custom-load.elc $(INFO_FILES)

udp2tcp: udp2tcp.cc
	${CC} -o $(BIN) udp2tcp.cc ${CFLAGS}
	$(STRIP) $(BIN)

srckit: srckit-std

binkit: binkit-common

