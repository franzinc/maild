# $Id: Makefile,v 1.8 2005/04/06 03:22:32 dancy Exp $

lisp=$(shell if test -n "$(ACL)"; then \
		echo $(ACL); \
	     elif test -x /fi/cl/7.0/bin/linux86/mlisp; then \
		echo /fi/cl/7.0/bin/linux86/mlisp; \
	     elif test -x /usr/local/acl70/mlisp; then \
		echo /usr/local/acl70/mlisp; \
	     elif test -x /storage1/acl70/mlisp; then \
		echo /storage1/acl70/mlisp; \
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
