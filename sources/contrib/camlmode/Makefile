# Files to install -- Comment out the first line and uncomment the second line
# if you're using Emacs 18.
FILES=caml.el inf-caml.el             # If you're using Emacs 19
##FILES=caml.el inf-caml.el comint.el # If you're using Emacs 18

# Overriden when called from ../Makefile
LISPDIR=/usr/local/emacs/site-lisp

all:

install:
	cp $(FILES) $(LISPDIR)
	for f in $(FILES); do emacs -batch -f batch-byte-compile $(LISPDIR)/$$f; done

clean:
	rm -f *~ #*#
