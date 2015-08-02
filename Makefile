PREFIX=/usr/local

FILE=sourcetoc.ml
TARGET=$(PREFIX)/bin/sourcetoc

install:
	cp -f $(FILE) $(TARGET)

lninstall:
	ln -sf $(abspath $(FILE)) $(TARGET)

uninstall:
	rm -f $(TARGET)

.PHONY: install lninstall uninstall
