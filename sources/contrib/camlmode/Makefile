# Files to install

FILES=caml.el inf-caml.el camldebug.el

# Overriden when called from ../Makefile
LISPDIR=/usr/local/emacs/site-lisp

all:

install:
	cp $(FILES) $(LISPDIR)
	for f in $(FILES); do \
          emacs -batch -f batch-byte-compile $(LISPDIR)/$$f; \
        done

clean:
	rm -f *~ #*#
