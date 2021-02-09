# avpkg
Available Packages

List available packages in the fpm-registry.

## Fortran dependencies
- fortran-curl
- fortran-pcre
- fhash

## C dependencies
- libjson-c
- libpcre
- libcurl

## Install dependencies
```bash
apt-get install libjson-c-dev libpcre3-dev libcurl4-openssl-dev
yum install json-c-devel libcurl-devel pcre-devel
```

```bash
git clone https://github.com/brocolis/avpkg
cd avpkg
fpm build
```

## Usage
```bash
avpkg search package
avpkg info package
```

## Examples
```bash
avpkg search lapack
avpkg search "thermodynamics|mechanics"
avpkg info datetime
avpkg search numeric
```
