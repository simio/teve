TARGET = teve
DISTNAME = teve-prototype1

SOURCE_FILES=*.scm
EXTRA_FILES=Makefile README
DISTFILES=$(SOURCE_FILES) $(EXTRA_FILES)

all: $(SOURCE_FILES)
	csc $(TARGET).scm

clean:
	rm -f $(TARGET)

source-tarball:
	tar -czf $(DISTNAME).tar.gz $(DISTFILES)

distclean:
	rm -f $(TARGET) $(DISTNAME).tar.gz
