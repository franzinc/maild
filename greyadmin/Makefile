# $Id: Makefile,v 1.2 2003/07/24 03:02:14 dancy Exp $

lisp=$(shell    if test -x /fi/cl/6.2/acl/osname.sh; then \
			echo /fi/cl/6.2/bin/`/fi/cl/6.2/acl/osname.sh`/mlisp; \
		elif test -x /storage1/acl/mlisp; then \
			echo /storage1/acl/mlisp; \
		else \
			echo /backup/acl/acl62/mlisp; \
		fi)
libdir=/usr/local/lib
bindir=/usr/local/sbin

all: greyadmin/greyadmin

greyadmin/greyadmin: greyadmin.cl
	rm -fr greyadmin
	$(lisp) -L greyadmin.cl -e "(build)" -kill

install: install-greyadmin

install-common:
	mkdir -p $(libdir) $(bindir)

install-greyadmin: greyadmin/greyadmin install-common
	rm -fr $(libdir)/greyadmin
	cp -pr greyadmin $(libdir)
	cp -p *.clp $(libdir)/greyadmin/
	ln -sf $(libdir)/greyadmin/greyadmin $(bindir)/greyadmin

clean:
	rm -f *.fasl
	rm -fr greyadmin
