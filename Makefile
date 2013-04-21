MAKE_DOC := make-doc.lisp
SBCL := sbcl
DOC := api-doc

doc: $(DOC).md

$(DOC).md: enchant.lisp $(MAKE_DOC)
	$(SBCL) --noinform --no-userinit --load $(MAKE_DOC) --quit >$@

$(DOC).html: $(DOC).md
	markdown $< >$@

clean:
	rm -f -- $(DOC).*

.PHONY: clean
