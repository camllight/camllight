  This archive contains the binaries for Caml Light 0.72,
  Windows (3.1, 95 and NT) version.

CONTENTS

  copyrght.txt  copyright notice
  install.txt	installation instructions
  readme.txt    this file
  camlwin.exe	the interactive toplevel system
  camlwin.ini	its configuration file
  bin/          command-line compilers and tools
  lib/          library files
  examples/     various examples
  doc/          reference manual

COPYRIGHT

  All files in this archive except CAML\BIN\CWSDPMI.EXE and
  CAML\BIN\EMU387.DXE are copyright INRIA,
  and covered by the copyright notice in file COPYRGHT.TXT.

  The files CAML\BIN\CWSDPMI.EXE and CAML\BIN\EMU387.DXE
  are copyright 1995 Charles W Sandmann
  (sandmann@clio.rice.edu), 102 Hurst Ct, Destrehan, LA 70047.
  Sources and binary updates are available at
  ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/


OVERVIEW

  Caml Light is a small, portable implementation of the ML language,
  that runs on Unix machines, on the PC under Windows and DOS (in
  32-bit protected mode), and on the Macintosh.

  Caml Light implements the Caml language, a functional language from
  the ML family. Caml is quite close to Standard ML, though not strictly
  conformant. There are some slight differences in syntax and semantics,
  and major differences in the module system (these changes were
  required to support separate compilation).

  Caml Light is implemented as a bytecode compiler, and fully
  bootstrapped. The whole system is quite small: about 100K for the runtime
  system, and another 100K of bytecode for the compiler. 1.2 megabyte of
  memory is enough to recompile the whole system. This stands in sharp
  contrast with other implementations of ML, such as SML-NJ, that
  requires about ten times more memory. Performance is quite good for a
  bytecoded implementation: five to ten times slower than SML-NJ.

  Caml Light comes in two flavors: a classical, interactive, toplevel-based
  system; and a standalone, batch-oriented compiler that produces standalone
  programs, in the spirit of the batch C compilers. The former is good for
  learning the language and testing programs. The latter integrates more
  smoothly within programming environments. The generated programs are
  quite small, and can be used as standalone programs.


REQUIREMENTS

  The Windows version requires a PC equipped with Microsoft Windows
  3.1, 95 or NT. Under Windows 3.1, the Win32s system (distributed along
  with Caml Light) is required. The batch compilers can also be run
  under plain MS-DOS on 80386 and higher machines. At least 8M of RAM
  is recommended.

  Linking with C functions (the "-custom" option to camlc and camlmktop)
  requires the DJGPP compiler version 2, freely available at
  ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/


INSTALLATION

  Installation is normally fully automatic from the self-extracting
  archive. In case of problems after installation, see the file
  INSTALL.TXT for more information.


DOCUMENTATION

  The Caml Light reference manual is included in this distribution,
  in Windows Help format. Other formats as well as a tutorial
  ("Functional programming in Caml Light", by Michel Mauny)
  are available from archive site from which you obtained Caml Light.
  They can also be obtained by anonymous FTP from
  ftp.inria.fr as described below, or from the Web site.

  The following Web site offers lots of information on Caml:

        http://pauillac.inria.fr/caml/

  including a comprehensive "Frequently Asked Questions" list, short
  tutorials, and material for programming courses.


AVAILABILITY

  The whole Caml Light distribution resides on ftp.inria.fr, and can
  be accessed by anonymous FTP:

        host:       ftp.inria.fr
        directory:  lang/caml-light


KEEPING IN TOUCH WITH THE CAML COMMUNITY:

  There exists a mailing list of users of the Caml and Caml Light
  systems developed at INRIA. The purpose of this list is to share
  experience, exchange ideas (and even code), and report on applications
  of the Caml language. This list is moderated; messages can be
  written in English or in French. 

  Messages to the list should be sent to:

                caml-list@inria.fr

  If you wish to subscribe to this list, please send a message
  (including your email address) to:

                caml-list-request@inria.fr


BUG REPORTS AND USER FEEDBACK:

  Send your bug reports by E-mail to

          caml-light@pauillac.inria.fr

  or by paper mail to

          Caml Light, projet Cristal
          INRIA Rocquencourt
          B.P. 105
          78153 Le Chesnay cedex
          France

  To be effective, bug reports should include a complete program (as
  small as possible) that exhibits the unexpected behavior, and the
  configuration you are using (machine type, etc).

  We do not wish to be contacted by phone.
