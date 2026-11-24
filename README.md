# manimR

Public source variant of the manimR R package.

Provides mathematical animation objects, scenes and renderers in R. LaTeX rendering requires a working TeX installation and dvisvgm; those capabilities are optional.

## Installation

Install with `remotes::install_github("GeoffreyManda/manimR-public")`, or use `R CMD INSTALL` on the source after installing the dependencies declared in `DESCRIPTION`.

## Licence and credits

The original licence and contributor notices are retained in the package metadata and licence files.

## Validation

The package compiled its C++ backend from source and passed its local tests on 24 November 2026. Four optional rendering tests were skipped because TeX or dvisvgm was unavailable. LaTeX expressions use the documented text fallback when TeX is absent.
