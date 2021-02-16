# fpm-search
Available Packages

List available packages in the fpm-registry.

## Fortran dependencies
- fortran-curl
- fortran-pcre
- fhash
- M_CLI2

## C dependencies
- libjson-c
- libpcre
- libcurl

## Install dependencies
```
apt-get install libjson-c-dev libpcre3-dev libcurl4-openssl-dev
yum install json-c-devel libcurl-devel pcre-devel
```

## Build
```
git clone https://github.com/brocolis/fpm-search
cd fpm-search
fpm build
fpm install
```

## Usage
```
NAME
   fpm-search(1) - display available FPM packages
SYNOPSIS
   syntax:

    fpm-search SEARCH_STRING(s) [--verbose]
     or
    fpm-search --toml PACKAGE_NAME [TAG]
DESCRIPTION
   Search for and display information describing fpm (Fortran Package Manager)
   packages registered in the fpm repository at

      https://github.com/fortran-lang/fpm-registry
OPTIONS
 SEARCH MODE:
    SEARCH_STRING  string to perform a case-insensitive search for in the
                   package descriptions. The default is ".", causing all
                   registered packages to be displayed.
    --verbose,-V   give detailed information about packages located.

 TOML ENTRY MODE:
    --toml,-T      instead of an fpm project description give the line needed
                   to be added to the "fpm.toml" file in order to use the
                   specified external package in your fpm project.
    PACKAGE_NAME   when the --toml switch a string is required and is NOT
                   treated as a Regular Expression but as a specific
                   case-sensitive fpm package name.
    TAG            A git(1) tag name can optionally follow the PACKAGE_NAME
                   when using the --toml switch.

 DOCUMENTATION:
    --help,-h      display this help and exit
    --version,-v   output version information and exit
```

## Examples
```bash
fpm-search molecular
fpm-search "thermodynamics|mechanics" # look for either string
fpm-search weather --verbose
fpm-search "date|time"

fpm-search M_color --toml
fpm-search --toml datetime v1.7.0

fpm-search     # list all package descriptions
fpm-search -V  # describe all packages in detail
```