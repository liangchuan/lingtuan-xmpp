# $Id$

CC = gcc
CFLAGS = -g -O2 -Wall 
CPPFLAGS = -I/usr/local/ssl/include -g -O2 -Wall 
LDFLAGS = 
LIBS = 

ERLANG_CFLAGS = -I/app/erlang-otp/lib/erlang/lib/erl_interface-3.7.13/include -I/app/erlang-otp/lib/erlang/usr/include
ERLANG_LIBS = -L/app/erlang-otp/lib/erlang/lib/erl_interface-3.7.13/lib -lerl_interface -lei

EFLAGS += -DSSL40
EFLAGS += -I ..
EFLAGS += -pz ..

# make debug=true to compile Erlang module with debug informations.
ifdef debug
	EFLAGS+=+debug_info +export_all
endif

SOURCES = $(wildcard *.erl)
OUTDIR = ..
BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))

all: $(BEAMS)

$(OUTDIR)/%.beam: %.erl
	/app/erlang-otp/bin/erlc -W $(EFLAGS) -o $(OUTDIR) $<

clean:
	rm -f $(BEAMS)

distclean: clean
	rm -f Makefile

TAGS:
	etags *.erl

