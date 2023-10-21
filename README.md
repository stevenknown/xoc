xoc
=====

Welcome to use XOC

What does XOC mean?
	eXtremely Optimizing Compiler.
	We build XOC compiler that intent to be a finely honed tool to squeezing
	the last performance out of ordinary code.

Contribution and License Agreement.
	The XOC Repo is licensed under the BSD license.
	If you contribute code to this project, you are implicitly allowing your
	code to be distributed under the BSD license.

Your ideas have no limits.
	Surprisingly powerful.
	Why let programming tools shackle your mind?
	XOC was designed for extensibility.
	Tailoring and retargeting the compiler is easy, and fun.

Truly open.
	XOC is not just open-source, it is open from end to end.
	You can tune its syntax, add new instructions, integrate arbitrary system
	capabilities,  enforce custom policies, add specific optimizations ...

XOC is a compiler infrastructure that provides multi-level IR operations,
flexibility, and the capability of representing almost all popular languages.
There are two level IR representations used throughout all phases of the
compilation. You can use XOC to perform optimization, program analysis or build diagnostic tools.

The main documentation is in doc/Manual.txt.

Build libxoc.a on Linux:
Default build host is Linux
```cmd
	cd xoc
	make -f Makefile.xoc
```

Build libxoc.a on Windows:
```cmd
	cd xoc
	make -f Makefile.xoc WIN=1
```

How to Contribute :
	All contributors should own the rights to any code you contribute, and
	that you give us permission to use that code in XOC.
	You maintain the copyright on that code.
	Submit a pull request for your changes. A project developer will review
	your work and then merge your request into the project.
