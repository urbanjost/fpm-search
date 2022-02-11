![string](docs/images/search.gif)
# fpm-search

List available packages in the
[fpm-registry](https://fortran-lang.org/packages/fpm) from the
command line.

This allows for quickly locating fpm(1) packages and
generating the associated dependency lines to insert into 
fpm(1) package manifest files (fpm.toml) that wish to use
them.

The fpm-search(1) command may be used stand-alone
or as a plugin for fpm(1).

This repository is a descendent of fpm-search(1) by @brocolis
(Carlos Une).

## Build
```
git clone https://github.com/urbanjost/fpm-search
cd fpm-search
fpm install
```
## Usage
```bash
fpm-search hash
fpm-search pixel -V
fpm-search     # list all package descriptions
fpm-search -V  # describe all packages in detail
```

## Configuration file

Additional repositories may be searched by creating a
configuration file.

1. Windows: `%HOMEPATH%\.fpm-search.conf`
2. Linux: `$HOME/.fpm-search.conf`

### Format
```
label1=https://my-alternate-registry/index.json
label2=https://other-registry/index.json
```
---
### References
## Fortran Package Manager (fpm) ![fpm](docs/images/fpm_wheel.gif)
 - [fpm developer documentation](https://fortran-lang.github.io/fpm)
 - [fpm github site](https://github.com/fortran-lang/fpm)
 - [repository instructions](https://github.com/fortran-lang/fpm-registry)
 - [package listing](https://fortran-lang.org/packages).
 - [fpm presentation](https://tcevents.chem.uzh.ch/event/12/contributions/39/attachments/35/104/Richardson-fpm-presentation.pdf)
 - [youtube:getting started with fpm](https://www.youtube.com/watch?v=pg77uR792YU)
 - [PackageCon]( https://www.youtube.com/watch?v=g7ECnr_Q7Ck)
