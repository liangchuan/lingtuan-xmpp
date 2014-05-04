# $Id$

CC = gcc
CFLAGS = -g -O2 -Wall 
CPPFLAGS = -I/usr/local/ssl/include -g -O2 -Wall 
LDFLAGS = 
LIBS = 

EXPAT_CFLAGS = 
ERLANG_CFLAGS= -I/app/erlang/lib/erlang/lib/erl_interface-3.7.14/include -I/app/erlang/lib/erlang/usr/include

EXPAT_LIBS =  -lexpat
ERLANG_LIBS = -L/app/erlang/lib/erlang/lib/erl_interface-3.7.14/lib -lerl_interface -lei

ASN_FLAGS = -bber_bin +der +compact_bit_string +optimize +noobj

INSTALLUSER=
# if no user was enabled, don't set privileges or ownership
ifeq ($(INSTALLUSER),)
  O_USER=
  G_USER=
  CHOWN_COMMAND=echo
  CHOWN_OUTPUT=/dev/null
  INIT_USER=root
else
  O_USER=-o $(INSTALLUSER)
  G_USER=-g $(INSTALLUSER)
  CHOWN_COMMAND=chown
  CHOWN_OUTPUT=&1
  INIT_USER=$(INSTALLUSER)
endif

EFLAGS += -DSSL40 -pa .
ERLANG_CFLAGS += -DSSL40

# make debug=true to compile Erlang module with debug informations.
ifdef debug
  EFLAGS+=+debug_info +export_all
endif

DEBUGTOOLS = p1_prof.erl
ifdef debugtools
  SOURCES+=$(DEBUGTOOLS)
endif

ifeq (false, true)
  EFLAGS+=+native
endif

ifeq (false, true)
  EFLAGS+=-DROSTER_GATEWAY_WORKAROUND
endif

ifeq (false, true)
  EFLAGS+=-DFULL_XML_SUPPORT
endif

ifeq (false, true)
  EFLAGS+=-DNIF
  ERLSHLIBS=xml.so
endif

ifeq (true, false)
  EFLAGS+=-DNO_TRANSIENT_SUPERVISORS
endif

ifeq (false, true)
  EFLAGS+=-DHAVE_MD2
  ERLANG_CFLAGS += -DHAVE_MD2
endif

INSTALL_EPAM=
ifeq (, pam)
  INSTALL_EPAM=install -m 750 $(O_USER) epam $(PBINDIR)
endif

prefix = /home/appusr/ejabberd/runtime
exec_prefix = ${prefix}

SUBDIRS = mod_irc mod_pubsub mod_muc mod_proxy65 eldap  web stringprep stun tls  ejabberd_zlib
ERLSHLIBS += expat_erl.so
ERLBEHAVS = cyrsasl.erl gen_mod.erl p1_fsm.erl
SOURCES_ALL = $(wildcard *.erl)
SOURCES_MISC = $(ERLBEHAVS) $(DEBUGTOOLS)
SOURCES += $(filter-out $(SOURCES_MISC),$(SOURCES_ALL))
ERLBEHAVBEAMS = $(ERLBEHAVS:.erl=.beam)
BEAMS = $(SOURCES:.erl=.beam)

DESTDIR =

# /etc/ejabberd/
ETCDIR = $(DESTDIR)${prefix}/etc/ejabberd

# /sbin/
SBINDIR = $(DESTDIR)${exec_prefix}/sbin

# /lib/ejabberd/
EJABBERDDIR = $(DESTDIR)${exec_prefix}/lib/ejabberd

# /share/doc/ejabberd
PACKAGE_TARNAME = ejabberd
datarootdir = ${prefix}/share
DOCDIR = $(DESTDIR)${datarootdir}/doc/${PACKAGE_TARNAME}

# /usr/lib/ejabberd/ebin/
BEAMDIR = $(EJABBERDDIR)/ebin

# /usr/lib/ejabberd/include/
INCLUDEDIR = $(EJABBERDDIR)/include

# /usr/lib/ejabberd/priv/
PRIVDIR = $(EJABBERDDIR)/priv

# /usr/lib/ejabberd/priv/bin
PBINDIR = $(PRIVDIR)/bin

# /usr/lib/ejabberd/priv/lib
SODIR = $(PRIVDIR)/lib

# /usr/lib/ejabberd/priv/msgs
MSGSDIR = $(PRIVDIR)/msgs

# /var/lib/ejabberd/
SPOOLDIR = $(DESTDIR)${prefix}/var/lib/ejabberd

# /var/lock/ejabberdctl
CTLLOCKDIR = $(DESTDIR)${prefix}/var/lock/ejabberdctl

# /var/lib/ejabberd/.erlang.cookie
COOKIEFILE = $(SPOOLDIR)/.erlang.cookie

# /var/log/ejabberd/
LOGDIR = $(DESTDIR)${prefix}/var/log/ejabberd

# Assume Linux-style dynamic library flags
DYNAMIC_LIB_CFLAGS = -fpic -shared
ifeq ($(shell uname),Darwin)
    DYNAMIC_LIB_CFLAGS = -fPIC -bundle -flat_namespace -undefined suppress
endif
ifeq ($(shell uname),SunOs)
    DYNAMIC_LIB_CFLAGS = -KPIC -G -z text
endif

all: $(ERLSHLIBS) compile-beam all-recursive

compile-beam: XmppAddr.hrl $(ERLBEHAVBEAMS) $(BEAMS)

$(BEAMS): $(ERLBEHAVBEAMS)

all-recursive: $(ERLBEHAVBEAMS)

%.beam:       %.erl
	/app/erlang/bin/erlc -W $(EFLAGS) $<


all-recursive install-recursive uninstall-recursive \
clean-recursive distclean-recursive \
mostlyclean-recursive maintainer-clean-recursive:
	@subdirs="$(SUBDIRS)"; for subdir in $$subdirs; do \
	target=`echo $@|sed 's,-recursive,,'`; \
	echo making $$target in $$subdir; \
	(cd $$subdir && $(MAKE) $$target) || exit 1; \
	done


%.hrl: %.asn1
	/app/erlang/bin/erlc $(ASN_FLAGS) $<
	/app/erlang/bin/erlc -W $(EFLAGS) $*.erl

$(ERLSHLIBS):	%.so:	%.c
	$(CC) $(CFLAGS) $(LDFLAGS) $(LIBS) \
		$(subst ../,,$(subst .so,.c,$@)) \
		$(EXPAT_LIBS) \
		$(EXPAT_CFLAGS) \
		$(ERLANG_LIBS) \
		$(ERLANG_CFLAGS) \
		-o $@ \
		$(DYNAMIC_LIB_CFLAGS)

translations:
	../contrib/extract_translations/prepare-translation.sh -updateall

install: all
	#
	# Configuration files
	install -d -m 750 $(G_USER) $(ETCDIR)
	[ -f $(ETCDIR)/ejabberd.cfg ] \
		&& install -b -m 640 $(G_USER) ejabberd.cfg.example $(ETCDIR)/ejabberd.cfg-new \
		|| install -b -m 640 $(G_USER) ejabberd.cfg.example $(ETCDIR)/ejabberd.cfg
	sed -e "s*@rootdir@*/home/appusr/ejabberd/runtime*" \
		-e "s*@installuser@**" \
		-e "s*@LIBDIR@*${exec_prefix}/lib*" \
		-e "s*@SYSCONFDIR@*${prefix}/etc*" \
		-e "s*@LOCALSTATEDIR@*${prefix}/var*" \
		-e "s*@DOCDIR@*${datarootdir}/doc/${PACKAGE_TARNAME}*" \
		-e "s*@erl@*/app/erlang/bin/erl*" ejabberdctl.template \
		> ejabberdctl.example
	[ -f $(ETCDIR)/ejabberdctl.cfg ] \
		&& install -b -m 640 $(G_USER) ejabberdctl.cfg.example $(ETCDIR)/ejabberdctl.cfg-new \
		|| install -b -m 640 $(G_USER) ejabberdctl.cfg.example $(ETCDIR)/ejabberdctl.cfg
	install -b -m 644 $(G_USER) inetrc $(ETCDIR)/inetrc
	#
	# Administration script
	[ -d $(SBINDIR) ] || install -d -m 755 $(SBINDIR)
	install -m 550 $(G_USER) ejabberdctl.example $(SBINDIR)/ejabberdctl
	#
	# Init script
	sed -e "s*@ctlscriptpath@*$(SBINDIR)*" \
		-e "s*@installuser@*$(INIT_USER)*" ejabberd.init.template \
		> ejabberd.init
	chmod 755 ejabberd.init
	#
	# Binary Erlang files
	install -d $(BEAMDIR)
	install -m 644 *.app $(BEAMDIR)
	install -m 644 *.beam $(BEAMDIR)
	rm -f $(BEAMDIR)/configure.beam
	#
	# ejabberd header files
	install -d $(INCLUDEDIR)
	install -m 644 *.hrl $(INCLUDEDIR)
	install -d $(INCLUDEDIR)/eldap/
	install -m 644 eldap/*.hrl $(INCLUDEDIR)/eldap/
	install -d $(INCLUDEDIR)/mod_muc/
	install -m 644 mod_muc/*.hrl $(INCLUDEDIR)/mod_muc/
	install -d $(INCLUDEDIR)/mod_proxy65/
	install -m 644 mod_proxy65/*.hrl $(INCLUDEDIR)/mod_proxy65/
	install -d $(INCLUDEDIR)/mod_pubsub/
	install -m 644 mod_pubsub/*.hrl $(INCLUDEDIR)/mod_pubsub/
	install -d $(INCLUDEDIR)/web/
	install -m 644 web/*.hrl $(INCLUDEDIR)/web/
	#
	# Binary C programs
	install -d $(PBINDIR)
	install -m 750 $(O_USER) ../tools/captcha.sh $(PBINDIR)
	$(INSTALL_EPAM)
	#
	# Binary system libraries
	install -d $(SODIR)
	install -m 644 *.so $(SODIR)
	#
	# Translated strings
	install -d $(MSGSDIR)
	install -m 644 msgs/*.msg $(MSGSDIR)
	#
	# Spool directory
	install -d -m 750 $(O_USER) $(SPOOLDIR)
	$(CHOWN_COMMAND) -R  $(SPOOLDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(SPOOLDIR)
	[ ! -f $(COOKIEFILE) ] || { $(CHOWN_COMMAND)  $(COOKIEFILE) >$(CHOWN_OUTPUT) ; chmod 400 $(COOKIEFILE) ; }
	#
	# ejabberdctl lock directory
	install -d -m 750 $(O_USER) $(CTLLOCKDIR)
	$(CHOWN_COMMAND) -R  $(CTLLOCKDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(CTLLOCKDIR)
	#
	# Log directory
	install -d -m 750 $(O_USER) $(LOGDIR)
	$(CHOWN_COMMAND) -R  $(LOGDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(LOGDIR)
	#
	# Documentation
	install -d $(DOCDIR)
	install -m 644 ../doc/dev.html $(DOCDIR)
	install -m 644 ../doc/guide.html $(DOCDIR)
	install -m 644 ../doc/*.png $(DOCDIR)
	install -m 644 ../doc/*.txt $(DOCDIR)
	[ -f ../doc/guide.pdf ] \
		&& install -m 644 ../doc/guide.pdf $(DOCDIR) \
		|| echo "No ../doc/guide.pdf was built"
	install -m 644 ../COPYING $(DOCDIR)

uninstall: uninstall-binary

uninstall-binary:
	rm -f  $(SBINDIR)/ejabberdctl
	rm -fr $(DOCDIR)
	rm -f  $(BEAMDIR)/*.beam
	rm -f  $(BEAMDIR)/*.app
	rm -fr $(BEAMDIR)
	rm -f  $(INCLUDEDIR)/*.hrl
	rm -fr $(INCLUDEDIR)
	rm -fr $(PBINDIR)
	rm -f  $(SODIR)/*.so
	rm -fr $(SODIR)
	rm -f  $(MSGSDIR)/*.msgs
	rm -fr $(MSGSDIR)
	rm -fr $(PRIVDIR)
	rm -fr $(EJABBERDDIR)

uninstall-all: uninstall-binary
	rm -rf $(ETCDIR)
	rm -rf $(EJABBERDDIR)
	rm -rf $(SPOOLDIR)
	rm -rf $(CTLLOCKDIR)
	rm -rf $(LOGDIR)

clean: clean-recursive clean-local

clean-local:
	rm -f *.beam $(ERLSHLIBS) epam ejabberdctl.example
	rm -f XmppAddr.asn1db XmppAddr.erl XmppAddr.hrl

distclean: distclean-recursive clean-local
	rm -f config.status
	rm -f config.log
	rm -f Makefile
	[ ! -f ../ChangeLog ] || rm -f ../ChangeLog

TAGS:
	etags *.erl

Makefile: Makefile.in

dialyzer: $(BEAMS)
	@dialyzer -c .
