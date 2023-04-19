# c(a|d)ⁿr

[![CI](https://github.com/eutro/cadnr/actions/workflows/ci.yml/badge.svg)](https://github.com/eutro/racket-raylib/actions/workflows/ci.yml)
[![Collection](https://img.shields.io/badge/dynamic/json?color=blueviolet&label=collection&query=%24.collection&url=https%3A%2F%2Fpkgs.racket-lang.org%2Fpkg%2Fcadnr.json)](https://pkgs.racket-lang.org/package/cadnr)
[![Documentation](https://img.shields.io/badge/docs-published-teal)](https://docs.racket-lang.org/cadnr/)
[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue)](#license)

---

`car`, `cdr`, `caaaaddddr`, and everything in between.

See the [documentation](https://docs.racket-lang.org/cadnr/index.html) for details!

---

This module extends a number of built-in Racket functions that have obvious arbitrary extensions.

For instance, the caaaar-cddddr family of functions are defined for all combinations of as and ds, but only up to four! The obvious extension is to allow for an arbitrary number of each, and to simply generate them on the fly when they are referred to.

With this module required, it’s possible to use these functions just by naming them:

Examples:

```racket
> (caddddddddddr '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
11
> (succcccc 1)
6
> (sub1234 5678)
4444
```

## License

This software is distributed under the MIT license and the Apache
version 2.0 license, at your option. See [LICENSE-MIT](LICENSE-MIT)
and [LICENSE-APACHE](LICENSE-APACHE) for details.
