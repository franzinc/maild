# $Id: Makefile,v 1.11 2003/07/23 19:21:48 dancy Exp $

lisp=$(shell if test -x /storage1/acl/mlisp; then \
		echo /storage1/acl/mlisp; \
	     else \
		echo /backup/acl/acl62/mlisp; \
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
	rm -fr $(libdir)/maild.old
	-mv $(libdir)/maild $(libdir)/maild.old
	cp -pr maild $(libdir)
	chown root $(libdir)/maild/*
	ln -sf $(libdir)/maild/maild $(bindir)/maild

install-check-mail-virus: check-mail-virus/check-mail-virus install-common
	rm -fr $(libdir)/check-mail-virus
	cp -pr check-mail-virus $(libdir)
	chown root $(libdir)/check-mail-virus/*
	ln -sf $(libdir)/check-mail-virus/check-mail-virus $(bindir)/check-mail-virus

install-init:
	cp -p maild.init /etc/init.d/maild

clean:
	rm -f *.fasl
	rm -fr maild
