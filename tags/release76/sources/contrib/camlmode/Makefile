# Files to install

FILES=caml.el inf-caml.el camldebug.el

# Overriden when called from ../Makefile
EMACSLISPDIR=/usr/local/emacs/site-lisp

# Which Emacs to use for compilation
EMACS=emacs

all:

install:
	cp $(FILES) $(EMACSLISPDIR)
	dir=`pwd`; cd $(EMACSLISPDIR); $(EMACS) -batch -l $$dir/compile

clean:
	rm -f *~ #*#
