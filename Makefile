## Makefile for Eicq   -*-Makefile-*-

## Copyright (C) 2001 Steve Youngs


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

PACKAGE = eicq

# csh... yell no, we won't go!
SHELL = /bin/sh

# Everything hangs off this.  
PREFIX = /usr/local

# Where the lisp files go.
LISP_DIR = $(PREFIX)/lib/xemacs/site-packages/lisp/$(PACKAGE)

# Where the toolbar images go.
DATA_DIR = $(PREFIX)/lib/xemacs/site-packages/etc/$(PACKAGE)

# Where the binary and user install script go.
BIN_DIR = $(PREFIX)/bin

# Where the info files go.
INFO_DIR = $(PREFIX)/lib/xemacs/site-packages/info

# Programs and their flags.
EMACS = xemacs
EMACS_FLAGS = -batch
CC = g++
CFLAGS = -O2 -Wall
INSTALL = ginstall -o 0 -g 0
INSTALL_INFO = install-info
INSTALL_INFO_FLAGS = --info-dir=/usr/info \
	--info-file=$(INFO_DIR)/$(PACKAGE).info

############################################################################
##                No User Configurable Items Below Here                   ##
############################################################################

BIN = udp2tcp
USERSH = eicq-user-install.sh
OBJECTS = eicq.elc eicq-toolbar.elc eicq-report.elc 
SOURCES = eicq.el eicq-toolbar.el eicq-report.el 
EXTRA_SRC = ChangeLog INSTALL NEWS README TODO
EXTRA_OBJ = $(wildcard ./auto-autoloads.el*) $(wildcard ./custom-load.el*)
DATA_FILES = $(wildcard ./etc/$(PACKAGE)/*.xpm) ./etc/$(PACKAGE)/world
TEXI_FILES = $(PACKAGE).texi
INFO_FILES = $(wildcard ./$(PACKAGE).info*)

EICQ_INFO_COMPILE = ./infohack.el 
PRELOADS = -eval \("push default-directory load-path"\)
AUTO_PRELOADS = -eval \("setq autoload-package-name \"$(PACKAGE)\""\)

.SUFFIXES:
.SUFFIXES: .info .texi .elc .el

all:: bin autoloads $(OBJECTS) customloads texinfo

bin: $(BIN).cc
	$(CC) $(CFLAGS) -o $(BIN) $(BIN).cc

autoloads: auto-autoloads.el

customloads: custom-load.el

.el.elc:
	$(EMACS) $(EMACS_FLAGS) $(PRELOADS) -f batch-byte-compile $<

texinfo: $(TEXI_FILES)
	$(EMACS) $(EMACS_FLAGS) -l $(EICQ_INFO_COMPILE) -f batch-makeinfo $<

auto-autoloads.el : $(SOURCES)
	$(EMACS) $(EMACS_FLAGS) $(AUTO_PRELOADS) -f batch-update-directory ./
	$(EMACS) $(EMACS_FLAGS) -f batch-byte-compile ./auto-autoloads.el

custom-load.el : $(SOURCES)
	$(EMACS) $(EMACS_FLAGS) -f Custom-make-dependencies ./
	$(EMACS) $(EMACS_FLAGS) -f batch-byte-compile ./custom-load.el

install: all
	$(INSTALL) -d $(BIN_DIR) $(DATA_DIR) $(INFO_DIR) $(LISP_DIR)
	$(INSTALL) -s -m 755 $(BIN) $(BIN_DIR)
	$(INSTALL) -m 755 $(USERSH) $(BIN_DIR)
	$(INSTALL) -m 644 $(DATA_FILES) $(DATA_DIR)
	$(INSTALL) -m 644 $(INFO_FILES) $(INFO_DIR)
	$(INSTALL) -m 644 $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) \
		$(LISP_DIR)
	$(INSTALL_INFO) $(INSTALL_INFO_FLAGS)


upgrade: uninstall install

uninstall:: 
	rm -rf $(DATA_DIR) $(LISP_DIR)
	rm -f $(INFO_DIR)/$(INFO_FILES) $(BIN_DIR)/$(BIN) $(BIN_DIR)/$(USERSH)
	$(INSTALL_INFO) --remove $(INSTALL_INFO_FLAGS)

clean::
	rm -f $(OBJECTS) $(BIN) $(INFO_FILES) \
		auto-autoloads.el* custom-load.el*

distclean: clean
	rm -f core *~

# Targets for Steve only.
superupgrade: distclean upgrade
	chown -R steve.xemacs /usr/local/lib/xemacs/site-packages/*
	chown -R steve.xemacs ./*
