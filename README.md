# mdf

A simple markdown to PDF CLI tool. It supports a few templates.
See the library for a wrapper around the excellent camlpdf library.

To use the tool, simply call it from the command-line.


```sh
$ cat README.md | mdf -
```

This will read from `stdin` and output to `stdout`. 
