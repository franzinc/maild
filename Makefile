# $Id: Makefile,v 1.7 2003/06/25 23:03:12 layer Exp $

lisp=$(shell if test -d /storage1/acl/mlisp; then \
		echo /storage1/acl/mlisp; \
	     else \
		echo /backup/acl/acl62/mlisp; \
	     fi)
libdir=/usr/local/lib
bindir=/usr/local/sbin

foo:
	echo $(lisp)

maild/maild: *.cl
	rm -fr maild
	$(lisp) -L load.cl -e "(build)" -kill

install: maild/maild
	mkdir -p $(libdir) $(bindir)
	rm -fr $(libdir)/maild.old
	-mv $(libdir)/maild $(libdir)/maild.old
	cp -pr maild $(libdir)
	chown root $(libdir)/maild/*
	ln -sf $(libdir)/maild/maild $(bindir)/maild

install-init:
	cp -p maild.init /etc/init.d/maild

clean:
	rm -f *.fasl
	rm -fr maild
