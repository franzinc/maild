# $Id: Makefile,v 1.26 2005/12/19 19:09:45 layer Exp $

arch:=$(shell if [ `arch` = x86_64 ]; then echo amd64.64; else echo 86; fi)

preferred_lisp_version=8.0
preferred_lisp=/fi/cl/$(preferred_lisp_version)/bin/linux$(arch)/mlisp
alt_lisp0=/usr/local/acl70/mlisp
alt_lisp1=/storage1/acl80/mlisp

lisp:=$(shell if test -x $(preferred_lisp); then \
		echo $(preferred_lisp); \
	     elif test -x $(alt_lisp0); then \
		echo $(alt_lisp0); \
	     elif test -x $(alt_lisp1); then \
		echo $(alt_lisp1); \
	     else \
		echo mlisp; \
	     fi)
libdir=/usr/local/lib
bindir=/usr/local/sbin

version := $(shell grep 'allegro-maild-version' version.cl | sed -e 's,.*"v\([0-9.]*\)".*,\1,')

all: clean maild/maild check-mail-virus/check-mail-virus
	(cd greyadmin; ACL=$(lisp) make)

maild/maild: *.cl
	rm -fr maild
	$(lisp) -batch -L load.cl -e "(build)" -kill

check-mail-virus/check-mail-virus: check-mail-virus.cl
	rm -fr check-mail-virus
	$(lisp) -batch -L check-mail-virus.cl -e '(build)' -kill

install: install-stop install-maild install-greyadmin \
	 install-check-mail-virus install-start

install-stop: FORCE
	/etc/rc.d/maild stop

install-start: FORCE
	/etc/rc.d/maild start

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

install-greyadmin: FORCE
	(cd greyadmin; make install)

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
