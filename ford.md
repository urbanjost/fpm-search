---
src_dir: src
output_dir: docs/fpm-ford
project: fpm-search
summary: fpm-search module
project_github: https://github.com/urbanjost/fpm-search
project_download:
author: John S. Urban
author_email: urbanjost@comcast.net
github: https://github.com/urbanjost/fpm-search
media_dir: ./docs/images
exclude_dir: ./archive
             ./FODDER
             ./build
             ./man
             ./app
             ./example
             ./test
             ./src/source
             ./docs/doxygen_out
display: public
         protected
source: true
proc_internals: true
sort: permission-alpha
favicon: docs/images/favicon.ico
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---
<!--
author_pic:
twitter:
website:
-->
{!README.md!}
---
