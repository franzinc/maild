# $Id: Makefile,v 1.21 2005/09/16 14:29:11 dancy Exp $

arch:=$(shell if [ `arch` = x86_64 ]; then echo amd64.64; else echo x86; fi)

lisp:=$(shell if test -x /fi/cl/7.0/bin/linux$(arch)/mlisp; then \
		echo /fi/cl/7.0/bin/linux$(arch)/mlisp; \
	     elif test -x /usr/local/acl70/mlisp; then \
		echo /usr/local/acl70/mlisp; \
	     elif test -x /storage1/acl70/mlisp; then \
		echo /storage1/acl70/mlisp; \
	     else \
		echo /backup/acl/acl70/mlisp; \
	     fi)
libdir=/usr/local/lib
bindir=/usr/local/sbin

version := $(shell grep 'allegro-maild-version' version.cl | sed -e 's,.*"v\([0-9.]*\)".*,\1,')

all: maild/maild check-mail-virus/check-mail-virus
	(cd greyadmin; make)

maild/maild: *.cl
	rm -fr maild
	$(lisp) -batch -L load.cl -e "(build)" -kill

check-mail-virus/check-mail-virus: check-mail-virus.cl
	rm -fr check-mail-virus
	$(lisp) -batch -L check-mail-virus.cl -e '(build)' -kill

install: install-maild install-check-mail-virus

install-common:
	mkdir -p $(libdir) $(bindir)

install-maild: maild/maild install-common
	rm -fr $(DESTDIR)$(libdir)/maild.old
	-mv $(DESTDIR)$(libdir)/maild $(DESTDIR)$(libdir)/maild.old
	cp -pr maild $(DESTDIR)$(libdir)
ifndef BUILD_FOR_RPM
	chown root $(DESTDIR)$(libdir)/maild/*
	chmod +s $(DESTDIR)$(libdir)/maild/maild
endif
	ln -sf ../lib/maild/maild $(DESTDIR)$(bindir)/maild
	ln -sf ../lib/maild/maild $(DESTDIR)$(bindir)/mailq

install-check-mail-virus: check-mail-virus/check-mail-virus install-common
	rm -fr $(DESTDIR)$(libdir)/check-mail-virus
	cp -pr check-mail-virus $(DESTDIR)$(libdir)
ifndef BUILD_FOR_RPM
	chown root $(DESTDIR)$(libdir)/check-mail-virus/*
endif
	ln -sf ../lib/check-mail-virus/check-mail-virus \
	       $(DESTDIR)$(bindir)/check-mail-virus

install-init: FORCE
	cp -p maild.init $(DESTDIR)/etc/init.d/maild

clean: FORCE
	rm -f *.fasl maild.tar.gz maild-*.tar.gz 
	rm -fr maild check-mail-virus rpmbuild maild.spec
	(cd greyadmin; make clean)

update: FORCE
	cvs -q update -dP

HERE := $(shell pwd)

BUILDROOT = $(HERE)/rpmbuild

rpm: FORCE
	rm -fr $(BUILDROOT)
	mkdir  $(BUILDROOT)
	mkdir  $(BUILDROOT)/BUILD
	mkdir  $(BUILDROOT)/RPMS
	mkdir  $(BUILDROOT)/SOURCES
	mkdir  $(BUILDROOT)/SPECS
	mkdir  $(BUILDROOT)/SRPMS
	rm -f maild.spec
	sed -e 's,__SOURCE__,$(HERE),g' < maild.spec.in > maild.spec 
	rpmbuild -vv --buildroot=$(BUILDROOT)/BUILD -bb maild.spec

tarball: all
	tar zcf maild.tar.gz maild

dist: tarball
	tar zcf maild-$(version)-installer.tar.gz \
		maild.tar.gz \
		maild.init \
		maild-installer 

FORCE:
