# $Id: Makefile,v 1.10 2006/01/01 15:02:37 dancy Exp $

lisp=$(shell if test -n "$(ACL)"; then \
		echo $(ACL); \
	     elif test -x /fi/cl/8.0/bin/mlisp; then \
		echo /fi/cl/8.0/bin/mlisp; \
	     elif test -x /storage1/acl80/mlisp; then \
		echo /storage1/acl80/mlisp; \
	     else \
		echo mlisp; \
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
