lisp=/storage1/acl/mlisp
libdir=/usr/local/lib
bindir=/usr/local/sbin

maild/maild: *.cl
	rm -fr maild
	$(lisp) -L load.cl -e "(build)" -kill

install: maild/maild
	mkdir -p $(libdir) $(bindir)
	cp -pr maild $(libdir)
	ln -sf $(libdir)/maild/maild $(bindir)/maild

clean:
	rm -f *.fasl
	rm -fr maild
