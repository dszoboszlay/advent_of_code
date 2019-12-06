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

input/a06.term: input/a06.txt
	sed 's/^\(.*\)[)]\(.*\)$$/{x\2, x\1}./' $< > $@

input/a05.term: input/a05.txt
	sed 's/$$/]./g;s/^/[/g' $< > $@

input/a03.term: input/a03.txt
	sed 's/$$/}]./g;s/^/[{/g;s/,/},{/g;s/L/l,/g;s/U/u,/g;s/R/r,/g;s/D/d,/g' $< > $@

input/a02.term: input/a02.txt
	sed 's/$$/]./g;s/^/[/g' $< > $@

%.term: %.txt
	sed 's/$$/./g' $< > $@
