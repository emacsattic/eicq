## Makefile for Eicq   -*-Makefile-*-
## $Id$
##
## Copyright (C) 2001 Steve Youngs
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

PACKAGE = eicq
VER = 0.2.17

# csh... yell no, we won't go!

SHELL = /bin/sh
# Cygwin # Comment out line above and uncomment the following line
# SHELL = /usr/bin/sh

# Everything hangs off this.  

PREFIX = /usr/local/lib/xemacs/site-packages
# Cygwin # Comment out the line above and uncomment the
# following line (/cygdrive/c is Cygwin notation for C:\ !!!)
# PREFIX = /cygdrive/c/Program\ Files/XEmacs/site-packages

# Where the lisp files go.
LISP_DIR = $(PREFIX)/lisp/$(PACKAGE)

# Where the toolbar images go.
DATA_DIR = $(PREFIX)/etc/$(PACKAGE)

# Where the binary and user install script go.
BIN_DIR = $(PREFIX)/lib-src

# Where the info files go.
INFO_DIR = $(PREFIX)/info

# If you want to make a tarball that you can just unpack on all your
# PC's you can 'make pkg'.  The 'pkg' target uses these directories to
# build the tarball.
STAGING = ../build-pkg
BIN_STAGING = $(STAGING)/lib-src
DATA_STAGING = $(STAGING)/etc/$(PACKAGE)
INFO_STAGING = $(STAGING)/info
LISP_STAGING = $(STAGING)/lisp/$(PACKAGE)

# Programs and their flags.
EMACS = xemacs
EMACS_FLAGS = -batch
CC = gcc
CFLAGS = -O2 -Wall
INSTALL = install -o 0 -g 0
# Solaris #  Comment out above line and uncomment the line below
# INSTALL = install -u 0 -g 0

PKG_INSTALL = install
TAR = /bin/tar
TAR_FLAGS = czf

############################################################################
##                No User Configurable Items Below Here                   ##
############################################################################

BIN = icq2tcp
USERSH = eicq-user-install.sh
SOURCES = eicq.el eicq-toolbar.el eicq-report.el eicq-convert.el \
	eicq-wharf.el
OBJECTS = $(SOURCES:.el=.elc)
EXTRA_SRC = ChangeLog INSTALL NEWS README TODO
EXTRA_OBJ = $(wildcard ./auto-autoloads.el*) $(wildcard ./custom-load.el*)
DATA_FILES = $(wildcard ./etc/$(PACKAGE)/*.xpm) ./etc/$(PACKAGE)/world
TEXI_FILES = $(PACKAGE).texi
INFO_FILES = $(TEXI_FILES:.texi=.info)

EICQ_INFO_COMPILE = ./infohack.el 
PRELOADS = -eval \("push default-directory load-path"\)
AUTO_PRELOADS = -eval \("setq autoload-package-name \"$(PACKAGE)\""\)

.SUFFIXES:
.SUFFIXES: .info .texi .elc .el

all:: $(BIN) autoloads $(OBJECTS) customloads texinfo

$(BIN): $(BIN).c
	$(CC) $(CFLAGS) -o $(BIN) $(BIN).c
# Solaris # Comment out the above and uncomment the following.
# $(BIN): $(BIN).c
# 	$(CC) $(CFLAGS) -o $(BIN) $(BIN).c -lsocket -lnsl

bin: $(BIN)

autoloads: auto-autoloads.el

customloads: custom-load.el

.el.elc:
	$(EMACS) $(EMACS_FLAGS) $(PRELOADS) -f batch-byte-compile $<

.texi.info:
	$(EMACS) $(EMACS_FLAGS) -l $(EICQ_INFO_COMPILE) -f batch-makeinfo $<

texinfo: $(INFO_FILES)

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

# Solaris # Comment out the above and uncomment the following.
# install: all
# 	for file in $(BIN_DIR) $(DATA_DIR) $(INFO_DIR) $(LISP_DIR); \
# 	  do $(INSTALL) -d $$file; \
# 	done
# 	for file in $(USERSH) $(BIN); \
# 	  do $(INSTALL) -f $(BIN_DIR) -m 755 $$file; \
# 	done
# 	for file in $(DATA_FILES); \
# 	  do $(INSTALL) -f $(DATA_DIR) -m 644 $$file; \
# 	done
# 	for file in $(INFO_FILES); \
# 	  do $(INSTALL) -f $(INFO_DIR) -m 644 $$file; \
# 	done
# 	for file in $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ); \
# 	  do $(INSTALL) -f $(LISP_DIR) -m 644 $$file; \
# 	done


pkg: all
	$(PKG_INSTALL) -d $(STAGING) $(BIN_STAGING) $(DATA_STAGING) \
		$(INFO_STAGING) $(LISP_STAGING)
	$(PKG_INSTALL) -s -m 755 $(BIN) $(BIN_STAGING)
	$(PKG_INSTALL) -m 755 $(USERSH) $(BIN_STAGING)
	$(PKG_INSTALL) -m 644 $(DATA_FILES) $(DATA_STAGING)
	$(PKG_INSTALL) -m 644 $(INFO_FILES) $(INFO_STAGING)
	$(PKG_INSTALL) -m 644 $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) \
		$(LISP_STAGING)
	(cd $(STAGING); \
		$(TAR) $(TAR_FLAGS) $(PACKAGE)-$(VER)-pkg.tar.gz ./*)

upgrade: uninstall install

uninstall:: 
	rm -rf $(DATA_DIR) $(LISP_DIR)
	rm -f $(INFO_DIR)/$(INFO_FILES) $(BIN_DIR)/$(BIN) $(BIN_DIR)/$(USERSH)

clean::
	rm -f $(OBJECTS) $(BIN) $(INFO_FILES) \
		auto-autoloads.el* custom-load.el*

distclean: clean
	rm -f core *~

# Targets for Steve only.
superupgrade: distclean upgrade
	chown -R steve.xemacs /usr/local/lib/xemacs/site-packages/*
	chown -R steve.xemacs ./*
