# Find Erlang modules
ERLS  := $(wildcard src/*.erl)
BEAMS := ${ERLS:src/%.erl=ebin/%.beam}

# Find input files
IN_TXTS  := $(wildcard input/*.txt)
IN_TERMS := ${IN_TXTS:.txt=.term}

.PHONY: all
all: ${BEAMS} ${IN_TERMS}

.PHONY: clean
clean:
	rm -rf ${BEAMS} ${IN_TERMS}

ebin/%.beam: src/%.erl | ebin
	erlc -o ebin $<

ebin:
	mkdir -p $@

%.term: %.txt
	sed 's/$$/./g' $< > $@
