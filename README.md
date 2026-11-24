# manimR

Mathematical animations in R.

## Install

```r
install.packages("remotes")
remotes::install_github("GeoffreyManda/manimR-public")
```

See the [getting started guide](docs/getting-started.md) for system requirements, a worked example, output interpretation and troubleshooting.

## Run the example

```r
source(system.file("examples", "worked-example.R", package = "manimR"))
```

The example uses synthetic inputs and cleans up its temporary output. Its source is [inst/examples/worked-example.R](inst/examples/worked-example.R).

## Checks

[GitHub Actions](https://github.com/GeoffreyManda/manimR-public/actions/workflows/package-checks.yaml) installs the package, runs the installed tests and executes the example on Ubuntu with the current R release.

## Licence and contributors

Original licence and contributor notices are retained in `DESCRIPTION` and the licence files.
