PRIVDIR := ../priv

all : $(PRIVDIR)/yang_scan_nif.so

override CFLAGS + -fpic -shared

$(PRIVDIR)/yang_scan_nif.so : yang_scan_nif.o
	$(CC) -shared -fpic $(LDFLAGS) -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/yang_scan_nif.so yang_scan_nif.o
