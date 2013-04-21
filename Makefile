MAKE_DOC := make-doc.lisp
SBCL := sbcl
DOC := api-doc

doc: $(DOC).md

$(DOC).md: enchant.lisp $(MAKE_DOC)
	@printf 'The API documentation for CL-Enchant\n' >$@
	@printf '====================================\n\n\n' >>$@
	$(SBCL) --noinform --no-userinit --load $(MAKE_DOC) --quit ENCHANT >>$@

$(DOC).html: $(DOC).md
	markdown $< >$@

clean:
	rm -f -- $(DOC).*

.PHONY: clean
