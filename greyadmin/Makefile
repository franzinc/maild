# $Id: Makefile,v 1.5 2004/12/20 17:45:08 layer Exp $

lisp=$(shell if test -x /fi/cl/7.0/bin/linux86/mlisp; then \
		echo /fi/cl/7.0/bin/linux86/mlisp; \
	     elif test -x /storage1/acl/mlisp; then \
		echo /storage1/acl/mlisp; \
	     else \
		echo /backup/acl/acl70/mlisp; \
	     fi)
libdir=/usr/local/lib
bindir=/usr/local/sbin

all: greyadmin/greyadmin

greyadmin/greyadmin: greyadmin.cl
	rm -fr greyadmin
	$(lisp) -batch -L greyadmin.cl -e "(build)" -kill

install: install-greyadmin

install-common:
	mkdir -p $(DESTDIR)$(libdir) $(DESTDIR)$(bindir)

install-greyadmin: greyadmin/greyadmin install-common
	rm -fr $(DESTDIR)$(libdir)/greyadmin
	cp -pr greyadmin $(DESTDIR)$(libdir)
	cp -p *.clp $(DESTDIR)$(libdir)/greyadmin/
	ln -sf ../lib/greyadmin/greyadmin $(DESTDIR)$(bindir)/greyadmin

clean:
	rm -f *.fasl
	rm -fr greyadmin
