
BIN = zen-db
DIST = $(BIN).tar.xz
SCRIPT = $(BIN).ros

all: $(BIN)

run:
	bin/${SCRIPT}

dist: $(DIST)

$(BIN): bin/$(BIN)
	ln -f $<

bin/$(BIN): *.asd src/*.lisp bin/$(BIN).ros
	ros build bin/$(SCRIPT)

$(DIST): $(BIN)
	tar cfvJ $@ $< extras

clean:
	rm -f $(BIN) bin/$(BIN) $(DIST)

clean-cache:
	-rm -rf ~/.cache/common-lisp

fresh: clean clean-cache all

.PHONY: all run clean clean-cache fresh
