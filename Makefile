at_franz = $(shell if test -d /fi/cl/8.2/acl; then echo t; else echo nil; fi)

Makefile_local = \
	$(shell if test -f Makefile.local; then echo Makefile.local; fi)

ifneq ($(Makefile_local),)
include $(Makefile_local)
endif

ARCH=$(shell uname -i)

ifeq ($(ARCH),x86_64)
lisp?=/fi/cl/8.2/bin/mlisp-64
else
preferred_lisp?=/fi/cl/8.2/bin/mlisp
alt_lisp0=/usr/local/acl82/mlisp
alt_lisp1=/storage1/acl82/mlisp
lisp?=$(shell if test -x $(preferred_lisp); then \
		echo $(preferred_lisp); \
	     elif test -x $(alt_lisp0); then \
		echo $(alt_lisp0); \
	     elif test -x $(alt_lisp1); then \
		echo $(alt_lisp1); \
	     else \
		echo mlisp; \
	     fi)
endif

ROOT ?= /
prefix ?= $(ROOT)/usr
libdir ?= $(prefix)/lib
bindir ?= $(prefix)/bin
sbindir ?= $(prefix)/sbin

version := $(shell grep 'allegro-maild-version' version.cl | sed -e 's,.*"v\([0-9.]*\)".*,\1,')

ifeq ($(at_franz),t)
release ?= $(shell . fi-apps-common/rpm-utils.sh && \
	rpm_next_release_number \
	   $$fs1/$(ARCH)/maild-$(version)-*.$(ARCH).rpm)
else
release ?= 1
endif

installer-package := maild-$(version)-installer.tar.gz

REDHAT73 := $(shell rpm -q redhat-release-7.3 >/dev/null && echo yes)
SUSE92 := $(shell rpm -q suse-release-9.2 >/dev/null && echo yes)

SRCFILES=Makefile \
	maild.init maild.init.suse9 maild.sysconfig \
	maild.pam maild.pam.suse maild.pam.rh73 \
	aliases.cl auth.cl blacklist.cl bounce.cl checkers.cl \
	config.cl deliver.cl deliver-smtp.cl dns.cl emailaddr.cl \
	greylist.cl headers.cl input.cl ipaddr.cl lex.cl load.cl \
	lock.cl log.cl maild.cl mailer.cl queue.cl queue-process.cl \
	recips.cl rep-server.cl rewrite.cl sasl.cl security.cl smtp.cl \
	smtp-server-checkers.cl smtp-server.cl utils.cl version.cl www.cl 

DOCFILES=ALIASES MAILERS.txt NOTES STATS greylist.sql greylist.sql.notes

GREYADMINSRCFILES=Makefile greyadmin.cl login.clp menu.clp super.clp

ifeq ($(at_franz),t)
ALL_EXTRA = repo_check
endif

all: $(ALL_EXTRA) clean maild/maild
	(cd greyadmin; ACL=$(lisp) make)

ifeq ($(at_franz),t)
repo_check: FORCE
	@if test ! -d fi-apps-common; then \
	    git clone git:/repo/git/fi-apps-common; \
	fi
endif

maild/maild: *.cl
	rm -fr maild
	$(lisp) -W -batch -L load.cl -e "(build)" -kill

check-mail-virus/check-mail-virus: check-mail-virus.cl
	rm -fr check-mail-virus
	$(lisp) -batch -L check-mail-virus.cl -e '(build)' -kill

install: install-system install-maild install-greyadmin

install-stop: FORCE
	-/etc/init.d/maild stop

install-start: FORCE
	/etc/init.d/maild start

install-common:
	mkdir -p $(libdir) $(sbindir)

install-maild: maild/maild install-common
	rm -fr $(libdir)/maild.old
	-mv $(libdir)/maild $(libdir)/maild.old
	cp -r maild $(libdir)
	chmod +s $(libdir)/maild/maild
	rm -f $(sbindir)/maild
	ln -s $(libdir)/maild/maild $(sbindir)/maild

install-check-mail-virus: check-mail-virus/check-mail-virus install-common
	rm -fr $(libdir)/check-mail-virus
	cp -r check-mail-virus $(libdir)
	chown root $(libdir)/check-mail-virus/*
	ln -sf ../lib/check-mail-virus/check-mail-virus \
	       $(sbindir)/check-mail-virus

ifeq ($(SUSE92),yes)
pamsrc=maild.pam.suse
else
ifeq ($(REDHAT73),yes)
pamsrc=maild.pam.rh73
else
pamsrc=maild.pam
endif
endif

install-system: FORCE
ifeq ($(SUSE92),yes)
	cp maild.init.suse9 $(ROOT)/etc/init.d/maild
else
	cp maild.init $(ROOT)/etc/rc.d/init.d/maild
endif
	if [ ! -e $(ROOT)/etc/sysconfig/maild ]; then \
		cp maild.sysconfig $(ROOT)/etc/sysconfig/maild; \
	fi
	if [ ! -e $(ROOT)/etc/pam.d/smtp ]; then \
		cp $(pamsrc) $(ROOT)/etc/pam.d/smtp; \
	fi

install-greyadmin: FORCE
	(cd greyadmin; make install)

clean: FORCE
	rm -f *.fasl maild.tar.gz maild-*.tar.gz autoloads.out
	rm -fr maild check-mail-virus
	(cd greyadmin; make clean)

realclean: clean
	rm -fr RPMS BUILD SRPMS 
	rm -f *~

update: FORCE
	cvs -q update -dP

tarball: all
	tar zcf maild.tar.gz maild

dist: tarball
	tar zcf $(installer-package) \
		maild.tar.gz \
		maild.init \
		maild-installer 

src-tarball: FORCE
	rm -fr maild-$(version) maild-$(version).tar.gz
	mkdir maild-$(version)
	cp $(SRCFILES) $(DOCFILES) maild-$(version)
	if test -f Makefile.local; then \
	    cp Makefile.local maild-$(version); \
	fi
	mkdir maild-$(version)/greyadmin
	(cd greyadmin && cp $(GREYADMINSRCFILES) ../maild-$(version)/greyadmin)
	tar zcf maild-$(version).tar.gz maild-$(version)
	rm -fr maild-$(version)

%.spec: %.spec.in version.cl
	sed -e "s/__VERSION__/$(version)/" < $< > $@

rpm-setup: FORCE
	mkdir -p BUILD RPMS SRPMS

%-rpm: maild-%.spec src-tarball rpm-setup
	rpmbuild --define "_sourcedir $(CURDIR)" \
		--define "_topdir $(CURDIR)" \
		--define "release $(release)" \
		-ba $<

SIGN ?= --sign

# This is the "normal" target (non-redhat 7.3, non-suse)
redhat-rpm: maild.spec src-tarball rpm-setup
	rpmbuild $(SIGN) --define "_sourcedir $(CURDIR)" \
		--define "_topdir $(CURDIR)" \
		--define "_builddir $(CURDIR)/BUILD" \
		--define "_rpmdir $(CURDIR)/RPMS" \
		--define "_srcrpmdir $(CURDIR)/SRPMS" \
		--define "release $(release)" \
		-ba maild.spec

REMOVE_PREVIOUS_VERSIONS ?= yes
REPOHOST                 ?= fs1
REPODIR                  ?= /storage1/franz/$(ARCH)

install-repo:
ifeq ($(REMOVE_PREVIOUS_VERSIONS),yes)
	ssh root@$(REPOHOST) "rm -f $(REPODIR)/maild-*"
endif
	scp -p RPMS/$(ARCH)/maild-$(version)-*.rpm root@$(REPOHOST):$(REPODIR)
	ssh root@$(REPOHOST) "createrepo -q --update $(REPODIR)"

ifeq ($(SUSE92),yes)
rpm: suse-rpm

else
ifeq ($(REDHAT73),yes)
rpm: rh73-rpm

else
rpm: redhat-rpm

endif
endif

FORCE:
