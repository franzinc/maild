lisp=/storage1/acl/mlisp

maild/maild: *.cl
	rm -fr maild
	$(lisp) -L load.cl -e "(build)" -kill

clean:
	rm -f *.fasl
	rm -fr maild