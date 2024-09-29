sbcl = $(shell which sbcl)

README.md: quicklisp/setup.lisp enchant.lisp readme.lisp print-doc.lisp
	$(sbcl) --script readme.lisp >$@

quicklisp/install.lisp:
	mkdir -p quicklisp
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp/install.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load asdf.conf \
		--load quicklisp/install.lisp \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

clean:
	rm -fr build

distclean: clean
	rm -fr quicklisp

.PHONY: clean distclean
