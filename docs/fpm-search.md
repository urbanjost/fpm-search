NAME
====

**fpm-search**(1) - display available FPM packages

SYNOPSIS
========

syntax: **fpm-search** *SEARCH\_STRING* \[-**-verbose**\]
\[-**-registry** *URI*\] \[-**-force-download**\]

DESCRIPTION
===========

Search for and display information describing fpm (Fortran Package
Manager) packages registered in the fpm repository at
https://github.com/fortran-lang/fpm-registry

OPTIONS
=======

***SEARCH\_STRING***

:   A string used to match package descriptions. It is case-insensitive.
    The default is "", causing all registered packages to be displayed.

****--verbose**,**-V****

:   give more-detailed information about the packages matching
    *SEARCH\_STRING*.

DOCUMENTATION:

> ****--help**,**-h****
>
> :   display this help and exit
>
> ****--version**,**-v****
>
> :   output version information and exit
>
EXAMPLE
=======

Sample commands:

      fpm-search hash
      fpm-search pixel --verbose
      fpm-search     # list all package descriptions
      fpm-search -V  # describe all packages in detail
