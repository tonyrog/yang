prefix ?= /usr
exec_prefix ?= $(prefix)
bindir ?= $(exec_prefix)/bin

PRIVDIR := ../priv

LD ?= ld
CC ?= gcc
RM ?= rm

CFLAGS ?= -O2

override CFLAGS += -std=gnu99 -Wall -fpic 
override LDFLAGS += -shared -fpic 

all : $(PRIVDIR)/yang_drv.so

$(PRIVDIR)/yang_drv.so : yang_scan_nif.o
	$(CC) $(LDFLAGS) -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/yang_drv.so yang_scan_nif.o
