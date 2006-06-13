# Makefile for the Unix interface library

# These options are overriden when called from ../Makefile
CC=cc
OPTS=
LIBDIR=/usr/local/lib/caml-light

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

# Compilation options
CFLAGS=-I../../src/runtime -O $(OPTS)
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
COMPFLAGS=-W
CAMLMKTOP=camlmktop

OBJS=accept.o access.o addrofstr.o alarm.o bind.o chdir.o chmod.o \
  chown.o chroot.o close.o closedir.o connect.o cst2constr.o cstringv.o \
  dup.o dup2.o envir.o errmsg.o execv.o execve.o execvp.o exit.o \
  fchmod.o fchown.o fcntl.o fork.o ftruncate.o getcwd.o getegid.o getenv.o \
  geteuid.o getgid.o getgr.o getgroups.o gethost.o gethostname.o \
  getlogin.o getpeername.o getpid.o getppid.o getproto.o getpw.o \
  getserv.o getsockname.o gettimeofday.o getuid.o \
  gmtime.o ioctl.o kill.o link.o listen.o lockf.o lseek.o mkdir.o \
  mkfifo.o nice.o open.o opendir.o pause.o pipe.o read.o \
  readdir.o readlink.o rename.o rewinddir.o rmdir.o select.o sendrecv.o \
  setgid.o setuid.o shutdown.o signal.o sleep.o socket.o socketaddr.o \
  socketpair.o stat.o strofaddr.o symlink.o termios.o time.o times.o \
  truncate.o umask.o unix.o unlink.o utimes.o wait.o waitopt.o waitpid.o \
  write.o

INTF= unix.zi

IMPL= unix.zo

all: libunix.a $(INTF) $(IMPL) camlunix

libunix.a: $(OBJS)
	rm -f libunix.a
	ar rc libunix.a $(OBJS)
	if $(RANLIBTEST); then $(RANLIB) libunix.a; else true; fi

camlunix: libunix.a $(IMPL)
	$(CAMLMKTOP) -o camlunix -custom unix.zo libunix.a

clean:
	rm -f libunix.a *.o *.zi *.zo lint-blurb camlunix

install:
	cp libunix.a $(LIBDIR)/libunix.a
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libunix.a; else true; fi
	cp $(INTF) $(IMPL) $(LIBDIR)
	cp camlunix $(LIBDIR)

.SUFFIXES: .ml .mli .zo .zi

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

lint:
	lint -I../../src/runtime *.c > lint-blurb

depend:
	mv Makefile Makefile.BAK; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.BAK; \
         gcc -MM $(CFLAGS) *.c; \
         ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
accept.o: accept.c unix.h socketaddr.h
access.o: access.c unix.h
addrofstr.o: addrofstr.c unix.h socketaddr.h
alarm.o: alarm.c unix.h
bind.o: bind.c unix.h socketaddr.h
chdir.o: chdir.c unix.h
chmod.o: chmod.c unix.h
chown.o: chown.c unix.h
chroot.o: chroot.c unix.h
close.o: close.c unix.h
closedir.o: closedir.c unix.h
connect.o: connect.c unix.h socketaddr.h
cst2constr.o: cst2constr.c cst2constr.h
cstringv.o: cstringv.c unix.h
dup.o: dup.c unix.h
dup2.o: dup2.c unix.h
envir.o: envir.c
errmsg.o: errmsg.c
execv.o: execv.c unix.h
execve.o: execve.c unix.h
execvp.o: execvp.c unix.h
exit.o: exit.c unix.h
fchmod.o: fchmod.c unix.h
fchown.o: fchown.c unix.h
fcntl.o: fcntl.c unix.h
fork.o: fork.c unix.h
ftruncate.o: ftruncate.c unix.h
getcwd.o: getcwd.c unix.h
getegid.o: getegid.c unix.h
getenv.o: getenv.c unix.h
geteuid.o: geteuid.c unix.h
getgid.o: getgid.c unix.h
getgr.o: getgr.c unix.h
getgroups.o: getgroups.c unix.h
gethost.o: gethost.c unix.h socketaddr.h
gethostname.o: gethostname.c unix.h
getlogin.o: getlogin.c unix.h
getpeername.o: getpeername.c unix.h socketaddr.h
getpid.o: getpid.c unix.h
getppid.o: getppid.c unix.h
getproto.o: getproto.c unix.h
getpw.o: getpw.c unix.h
getserv.o: getserv.c unix.h
getsockname.o: getsockname.c unix.h socketaddr.h
gettimeofday.o: gettimeofday.c unix.h
getuid.o: getuid.c unix.h
gmtime.o: gmtime.c unix.h
ioctl.o: ioctl.c unix.h
kill.o: kill.c unix.h
link.o: link.c unix.h
listen.o: listen.c unix.h
lockf.o: lockf.c unix.h
lseek.o: lseek.c unix.h
mkdir.o: mkdir.c unix.h
mkfifo.o: mkfifo.c unix.h
nice.o: nice.c unix.h
open.o: open.c unix.h
opendir.o: opendir.c unix.h
pause.o: pause.c unix.h
pipe.o: pipe.c unix.h
read.o: read.c unix.h
readdir.o: readdir.c unix.h
readlink.o: readlink.c unix.h
rename.o: rename.c unix.h
rewinddir.o: rewinddir.c unix.h
rmdir.o: rmdir.c unix.h
select.o: select.c unix.h
sendrecv.o: sendrecv.c unix.h socketaddr.h
setgid.o: setgid.c unix.h
setuid.o: setuid.c unix.h
shutdown.o: shutdown.c unix.h
signal.o: signal.c unix.h
sleep.o: sleep.c unix.h
socket.o: socket.c unix.h
socketaddr.o: socketaddr.c unix.h socketaddr.h
socketpair.o: socketpair.c unix.h
stat.o: stat.c unix.h cst2constr.h
strofaddr.o: strofaddr.c unix.h socketaddr.h
symlink.o: symlink.c unix.h
termios.o: termios.c unix.h
time.o: time.c unix.h
times.o: times.c unix.h
truncate.o: truncate.c unix.h
umask.o: umask.c unix.h
unix.o: unix.c unix.h cst2constr.h
unlink.o: unlink.c unix.h
utimes.o: utimes.c unix.h
wait.o: wait.c unix.h
waitopt.o: waitopt.c unix.h
waitpid.o: waitpid.c unix.h
write.o: write.c unix.h
iunix.zi: unix.zi 
unix.zo: unix.zi iunix.zi 
