MAKE_DOC := make-doc.lisp
SRC := enchant.lisp
SBCL := sbcl
DOC := README

$(DOC).md: $(SRC) $(MAKE_DOC)
	$(SBCL) --script $(MAKE_DOC) >$@

$(DOC).html: $(DOC).md
	markdown $< >$@

clean:
	rm -f -- $(DOC).html

.PHONY: clean
