MAKE_DOC := make-doc.lisp
SBCL := sbcl
DOC := api-doc

$(DOC).md: enchant.lisp $(MAKE_DOC)
	$(SBCL) --script $(MAKE_DOC) >$@

$(DOC).html: $(DOC).md
	markdown $< >$@

clean:
	rm -f -- $(DOC).html

.PHONY: clean
