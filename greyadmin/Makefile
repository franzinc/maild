# $Id: Makefile,v 1.11 2006/03/01 19:35:26 dancy Exp $

lisp=$(shell if test -n "$(ACL)"; then \
		echo $(ACL); \
	     elif test -x /fi/cl/8.0/bin/mlisp; then \
		echo /fi/cl/8.0/bin/mlisp; \
	     elif test -x /storage1/acl80/mlisp; then \
		echo /storage1/acl80/mlisp; \
	     else \
		echo mlisp; \
	     fi)

ROOT ?= /
prefix ?= $(ROOT)/usr
libdir ?= $(prefix)/lib
bindir ?= $(prefix)/bin
sbindir ?= $(prefix)/sbin

all: greyadmin/greyadmin

greyadmin/greyadmin: greyadmin.cl
	rm -fr greyadmin
	$(lisp) -batch -L greyadmin.cl -e "(build)" -kill

install: install-greyadmin

install-common:
	mkdir -p $(libdir) $(sbindir)

install-greyadmin: greyadmin/greyadmin install-common
	rm -fr $(libdir)/greyadmin
	cp -pr greyadmin $(libdir)
	cp -p *.clp $(libdir)/greyadmin/
	rm -f $(sbindir)/greyadmin
	ln -s $(libdir)/greyadmin/greyadmin $(sbindir)/greyadmin

clean:
	rm -f *.fasl autoloads.out
	rm -fr greyadmin
