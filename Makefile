TARGET = teve
DISTNAME = teve-prototype1

PREFIX ?= /usr/opt

SOURCE_FILES=*.scm sites/*.scm
TEST_FILES=./tests/*.scm
EXTRA_FILES=Makefile README
DISTFILES=$(SOURCE_FILES) $(EXTRA_FILES)

EGGS=utf8 intarweb srfi-37 json http-client vector-lib \
	packrat openssl base64 defstruct uri-common matchable uri-generic \
	message-digest ssax

OTHER_DEPLOY_FILES=type-checks type-errors regex blob-hexadecimal to-hex \
	string-hexadecimal variable-item blob-set-int md5 string-utils \
	memoized-string lookup-table unicode-utils sendfile

SO=.so

DEPLOY_PATH=$(PWD)/deploy
DEPLOY_CSC_OPTIONS="-C -Os -strip"

$(TARGET): $(SOURCE_FILES)
	csc $(TARGET).scm

all: $(SOURCE_FILES) $(TARGET)

check: $(SOURCE_FILES) $(TEST_FILES)
	for file in tests/*.scm; do \
		csi -s $$file; \
	done

install: $(TARGET)
	install -m 755 $(TARGET) $(PREFIX)/bin

install-eggs:
	chicken-install $(EGGS)

# wip, won't work
deploy: $(SOURCE_FILES) deploy-eggs deploy-others
	mkdir -p $(DEPLOY_PATH)/$(TARGET)
	env CSC_OPTIONS=$(DEPLOY_CSC_OPTIONS) \
		csc -deploy -o $(DEPLOY_PATH)/$(TARGET) $(TARGET).scm

deploy-eggs:
	mkdir -p $(DEPLOY_PATH)/$(TARGET)
	for egg in $(EGGS); do \
		env CSC_OPTIONS=$(DEPLOY_CSC_OPTIONS) chicken-install \
			-prefix $(DEPLOY_PATH)/$(TARGET) -deploy $$egg; \
	done

deploy-others:
	mkdir -p $(DEPLOY_PATH)/$(TARGET)
	CR_PATH=$$(chicken-install -repository) && \
	for file in $(OTHER_DEPLOY_FILES); do \
		cp $$CR_PATH/$$file$(SO) $(DEPLOY_PATH)/$(TARGET) && \
		strip $(DEPLOY_PATH)/$(TARGET)/$$file$(SO); \
	done

clean:
	rm -f $(TARGET)
	rm -rf $(DEPLOY_PATH)

source-tarball:
	tar -czf $(DISTNAME).tar.gz $(DISTFILES)

distclean: clean
	rm -f $(DISTNAME).tar.gz

print-eggs:
	@echo $(EGGS)
