## Makefile for Eicq   -*-Makefile-*-
## $Id$
##
## Copyright (C) 2001,03 Steve Youngs
##
## This file is part of Eicq.

## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
##
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
##
## 3. Neither the name of the author nor the names of any contributors
##    may be used to endorse or promote products derived from this
##    software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
## IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
## CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
## SUBSTITUTE GOODS OR SERVICES# LOSS OF USE, DATA, OR PROFITS# OR
## BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
## OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
## IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

PACKAGE = eicq
VER = 0.5.9

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

# Where the info files go.
INFO_DIR = $(PREFIX)/info

# If you want to make a tarball that you can just unpack on all your
# PC's you can 'make pkg'.  The 'pkg' target uses these directories to
# build the tarball.
STAGING = ../build-pkg
INFO_STAGING = $(STAGING)/info
LISP_STAGING = $(STAGING)/lisp/$(PACKAGE)

# Programs and their flags.
XEMACS = xemacs
XEMACS_FLAGS = -batch -no-autoloads
MAKEINFO = makeinfo
INSTALL = install -o 0 -g 0
# Solaris #  Comment out above line and uncomment the line below
# INSTALL = install -u 0 -g 0

PKG_INSTALL = install
TAR = /bin/tar
TAR_FLAGS = czf

############################################################################
##                No User Configurable Items Below Here                   ##
############################################################################

SOURCES = $(wildcard ./eicq*.el)
OBJECTS = $(SOURCES:.el=.elc)
EXTRA_SRC = ChangeLog INSTALL NEWS README TODO
EXTRA_OBJ = $(wildcard ./auto-autoloads.el*) $(wildcard ./custom-load.el*)
TEXI_FILES = $(PACKAGE).texi
INFO_FILES = $(TEXI_FILES:.texi=.info)

PRELOADS = -eval \("push default-directory load-path"\)

AUTOLOAD_PACKAGE_NAME = (setq autoload-package-name \"$(PACKAGE)\")
AUTOLOAD_FILE = (setq generated-autoload-file \"./auto-autoloads.el\")


.SUFFIXES:
.SUFFIXES: .info .texi .elc .el

all:: autoloads compile customloads texinfo

autoloads: auto-autoloads.el

customloads: custom-load.el

compile: $(SOURCES)
	$(XEMACS) $(XEMACS_FLAGS) $(PRELOADS) -l bytecomp \
		-f batch-byte-compile $^

.texi.info:
	$(MAKEINFO) $<

texinfo: $(INFO_FILES)

auto-autoloads.el: $(SOURCES)
	$(XEMACS) $(XEMACS_FLAGS) \
		-eval "$(AUTOLOAD_PACKAGE_NAME)" \
		-eval "$(AUTOLOAD_FILE)" \
		-l autoload -f batch-update-autoloads $^
	$(XEMACS) $(XEMACS_FLAGS) -l bytecomp \
		-f batch-byte-compile ./auto-autoloads.el

custom-load.el: $(SOURCES)
	$(XEMACS) $(XEMACS_FLAGS) -l cus-dep \
		-f Custom-make-dependencies ./
	$(XEMACS) $(XEMACS_FLAGS) -l bytecomp \
		-f batch-byte-compile ./custom-load.el

install: all
	$(INSTALL) -d $(INFO_DIR) $(LISP_DIR)
	$(INSTALL) -m 644 $(INFO_FILES) $(INFO_DIR)
	$(INSTALL) -m 644 $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) \
		$(LISP_DIR)

# Solaris # Comment out the above and uncomment the following.
# install: all
# 	for file in $(INFO_DIR) $(LISP_DIR); \
# 	  do $(INSTALL) -d $$file; \
# 	done
# 	for file in $(INFO_FILES); \
# 	  do $(INSTALL) -f $(INFO_DIR) -m 644 $$file; \
# 	done
# 	for file in $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ); \
# 	  do $(INSTALL) -f $(LISP_DIR) -m 644 $$file; \
# 	done


pkg: all
	$(PKG_INSTALL) -d $(STAGING) $(INFO_STAGING) $(LISP_STAGING)
	$(PKG_INSTALL) -m 644 $(INFO_FILES) $(INFO_STAGING)
	$(PKG_INSTALL) -m 644 $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) \
		$(LISP_STAGING)
	(cd $(STAGING); \
		$(TAR) $(TAR_FLAGS) $(PACKAGE)-$(VER)-pkg.tar.gz ./*)

upgrade: uninstall install

uninstall:: 
	rm -rf $(LISP_DIR)
	rm -f $(INFO_DIR)/$(INFO_FILES)

clean::
	rm -f $(OBJECTS) $(INFO_FILES) \
		auto-autoloads.el* custom-load.el*

distclean: clean
	rm -f core* *~ TAGS

# Developer targets
tags: TAGS

TAGS: $(SOURCES)
	etags $(SOURCES)

# Targets for Steve only.
superupgrade: distclean upgrade
	chown -R steve.xemacs /usr/local/lib/xemacs/site-packages/*
	chown -R steve.xemacs ./*
