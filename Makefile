# $Id: Makefile,v 1.13 2004/11/10 15:52:21 layer Exp $

lisp=$(shell if test -x /storage1/acl/mlisp; then \
		echo /storage1/acl/mlisp; \
	     else \
		echo /backup/acl/acl70/mlisp; \
	     fi)
libdir=/usr/local/lib
bindir=/usr/local/sbin

all: maild/maild check-mail-virus/check-mail-virus

maild/maild: *.cl
	rm -fr maild
	$(lisp) -L load.cl -e "(build)" -kill

check-mail-virus/check-mail-virus: check-mail-virus.cl
	rm -fr check-mail-virus
	$(lisp) -L check-mail-virus.cl -e '(build)' -kill

install: install-maild install-check-mail-virus

install-common:
	mkdir -p $(libdir) $(bindir)

install-maild: maild/maild install-common
	rm -fr $(DESTDIR)$(libdir)/maild.old
	-mv $(DESTDIR)$(libdir)/maild $(DESTDIR)$(libdir)/maild.old
	cp -pr maild $(DESTDIR)$(libdir)
ifndef BUILD_FOR_RPM
	chown root $(DESTDIR)$(libdir)/maild/*
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
	rm -f *.fasl
	rm -fr maild check-mail-virus rpmbuild maild.spec
	(cd greyadmin; make clean)

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

FORCE:
