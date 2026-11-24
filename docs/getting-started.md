# Mathematical animations in R

Use `manimR` to create scenes, add graphical objects, place animations on a timeline and render the result. The public variant includes source for the C++ backend and the optional Rust backend, with synthetic examples and tests.

## Requirements

R 4.1 or later and a C++17 compiler are required for installation from source. Windows users need the matching Rtools release; macOS users need the Xcode command-line tools. Linux needs a C++ build toolchain. Runtime R dependencies are listed in `DESCRIPTION`.

The `magick` package needs ImageMagick development libraries on systems without a binary package. The `av` package may require FFmpeg development libraries. The Ubuntu workflow installs the relevant system libraries before installing the R package.

LaTeX expressions require TeX; vector LaTeX rendering additionally requires `dvisvgm`. The worked circle example uses neither. The optional Rust backend needs Rust, Cargo and the relevant R extension tooling; the C++ backend supports the demonstrated workflow.

## Worked animation

```r
library(manimR)
s <- scene("growing-circle")
add_circle(s, radius = 1, color = "#007AFF")
grow_from_center(s, duration = 0.5)
render(s, output = "growing-circle.gif", format = "gif",
       quality = "low", fps = 4, open = FALSE,
       progress = FALSE, cache = FALSE)
```

The scene holds its objects and timeline. `add_circle()` creates the object; `grow_from_center()` animates the last added object and extends the scene duration. `render()` produces the GIF. Low quality and a low frame rate keep this first example quick; increase them for a presentation.

The executable example renders to a temporary GIF, checks that a nonempty output exists and removes it after validation. Use a permanent output path to retain an animation.

## Troubleshooting

A compiler error during installation requires the platform's R build tools and the dependencies declared in `DESCRIPTION` and `LinkingTo`. A missing-TeX message means text fallback is being used for LaTeX content. Install TeX and `dvisvgm` to exercise the full rendering path. The local release tests passed with four optional rendering skips in an environment without those tools.

For reproducible output, record the R/package versions, rendering dimensions, frame rate and fonts alongside your analysis. Keep generated media outside the package source repository.

## Run the complete installed example

```r
path <- system.file("examples", "worked-example.R", package = "manimR")
stopifnot(nzchar(path))
source(path)
```

## Development checks

GitHub Actions runs on pushes, pull requests and manual dispatch. It installs runtime and test dependencies, installs the package with tests, runs `testthat::test_package()` and executes the installed example. Failed steps fail the job. Optional capability skips remain visible in the log. This workflow covers Linux with the current R release; it does not establish cross-platform package acceptance.

Report an issue with the smallest synthetic example, your R version and the complete error message. Exclude private records and credentials.
