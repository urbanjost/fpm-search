# fpm-search
Available Packages

List available packages in the fpm-registry.

## Build
```
git clone https://github.com/brocolis/fpm-search
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
1. Windows: `%HOMEPATH%\.fpm-search.conf`
2. Linux: `$HOME/.fpm-search.conf`

### Format
```
label1=https://my-alternate-registry/index.json
label2=https://other-registry/index.json
```
