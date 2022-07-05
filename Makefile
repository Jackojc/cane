# cane
.POSIX:

include config.mk

all: options cane

config:
	mkdir -p build/

options:
	@printf "cxx \033[32m$(CXX)\033[0m | "
	@printf "dbg \033[32m$(DBG)\033[0m\n"

cane: config options
	$(CXX) $(CANE_CXXFLAGS) src/cane.cpp -o build/cane $(CANE_LDFLAGS)

clean:
	rm -rf build/cane cane-$(VERSION).tar.gz

dist: clean cane
	mkdir -p cane-$(VERSION)
	cp -R LICENSE Makefile README.md config.mk src/ doc/ modules/ cane-$(VERSION)
	tar -cf - cane-$(VERSION) | gzip > cane-$(VERSION).tar.gz
	rm -rf cane-$(VERSION)

install: cane
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp -f build/cane $(DESTDIR)$(PREFIX)/bin
	chmod 755 $(DESTDIR)$(PREFIX)/bin/cane
	mkdir -p $(DESTDIR)$(MANPREFIX)/man1
	sed "s/VERSION/$(VERSION)/g" < doc/cane.1 > $(DESTDIR)$(MANPREFIX)/man1/cane.1
	chmod 644 $(DESTDIR)$(MANPREFIX)/man1/cane.1

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/cane
	rm -f $(DESTDIR)$(MANPREFIX)/man1/cane.1

.PHONY: all config options clean dist install uninstall

