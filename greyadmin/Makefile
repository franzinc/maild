# $Id: Makefile,v 1.3 2004/11/10 15:51:53 layer Exp $

lisp=$(shell    if test -x /fi/cl/6.2/acl/osname.sh; then \
			echo /fi/cl/6.2/bin/`/fi/cl/6.2/acl/osname.sh`/mlisp; \
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
	$(lisp) -L greyadmin.cl -e "(build)" -kill

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
