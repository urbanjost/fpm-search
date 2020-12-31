# avpkg
Available Packages

List available packages in the fpm-registry.

## Build
```bash
git clone https://github.com/brocolis/avpkg
cd avpkg
fpm build
```

## Usage
`$ avpkg`


## Dependencies
fortran-curl = { git = "https://github.com/interkosmos/fortran-curl" }
M_strings = { git = "https://github.com/urbanjost/M_strings.git" }
fhash = {git="https://github.com/LKedward/fhash", tag="v0.1.0"}


## Warning
avpkg writes the chosen dependency to your fpm.toml file only if the [dependencies] section doesn't exist already.


