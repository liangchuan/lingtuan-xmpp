# $Id$

CC = gcc
CFLAGS = -g -O2 -Wall 
CPPFLAGS = -I/usr/local/ssl/include -g -O2 -Wall 
LDFLAGS = 
LIBS = 

ERLANG_CFLAGS = -I/app/erlang/lib/erlang/lib/erl_interface-3.7.14/include -I/app/erlang/lib/erlang/usr/include
ERLANG_LIBS = -L/app/erlang/lib/erlang/lib/erl_interface-3.7.14/lib -lerl_interface -lei

EFLAGS += -I ..
EFLAGS += -pz ..

# make debug=true to compile Erlang module with debug informations.
ifdef debug
	EFLAGS+=+debug_info +export_all
endif

OUTDIR = ..
SOURCES = $(wildcard *.erl)
BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))


all:    $(BEAMS)

$(OUTDIR)/%.beam:       %.erl
	/app/erlang/bin/erlc -W $(EFLAGS) -Dgeneric -o $(OUTDIR) $<

clean:
	rm -f $(BEAMS)

distclean: clean
	rm -f Makefile

TAGS:
	etags *.erl

