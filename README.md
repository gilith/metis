Metis Theorem Prover (Development Version)
==========================================

[Metis][] is an automatic theorem prover for first order logic with equality. It accepts problems in [TPTP format][TPTP].

Cloning this repo will install a [development version][MetisDevelopment], which includes active debugging code and regression scripts. The latest official release of Metis without any extra development cruft [lives here][MetisRelease].

This software is released under the [MIT License][MITLicense].

Install
-------

Installing Metis requires the [MLton][], [Poly/ML][PolyML] or [Moscow ML][MoscowML] compiler, as well as standard system tools including GNU Make and Perl.

Clone this repo and initialize the development version:

    git clone https://github.com/gilith/metis.git
    cd metis
    make init

By default the initialization step requires the [MLton compiler][Mlton], but you can change it to [Poly/ML][PolyML] or [Moscow ML][MoscowML] by editing the top of `Makefile.dev`.

Build
-----

### Using the MLton compiler

Use the [MLton compiler][MLton] to build from source and run the test suite by executing

    make mlton

The Metis executable can then be found at

    bin/mlton/metis

### Using the Poly/ML compiler

Use the [Poly/ML compiler][PolyML] to build from source and run the test suite by executing

    make polyml

The Metis executable can then be found at

    bin/polyml/metis

### Using the Moscow ML compiler

Use the [Moscow ML compiler][MoscowML] to build from source and run the test suite by executing

    make mosml

The Metis executable can then be found at

    bin/mosml/metis

Test
----

A simple test is to display a usage message:

    path/to/metis --help

A more serious test is to check the status of a benchmark set of problems using the command

    make status-test

Troubleshoot
------------

You can use

    make clean

to clean out any object files.

To report a bug or request an enhancement, please file an issue at [GitHub][MetisIssues].

[Metis]: http://www.gilith.com/metis/ "Metis Theorem Prover"
[MetisDevelopment]: https://github.com/gilith/metis "Metis Development"
[MetisIssues]: https://github.com/gilith/metis/issues "Metis Issues"
[MetisRelease]: http://www.gilith.com/software/metis/ "Metis Release"
[MLton]: http://www.mlton.org/ "MLton compiler"
[PolyML]: http://www.polyml.org/ "Poly/ML compiler"
[MoscowML]: http://www.dina.dk/~sestoft/mosml.html "Moscow ML compiler"
[MITLicense]: https://github.com/gilith/metis/blob/master/LICENSE "MIT License"
[TPTP]: http://www.tptp.org "TPTP"
