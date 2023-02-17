
# About

Lecture materials for the course 'Big Data Analytics' taught at the University of St.Gallen and the University of Lucerne.

The main lecture materials are provided in the following directories:

 - Lecture slides: `slides/`
 - Code examples used during lectures: `R_examples/`

# Prerequisites

## Install required R packages
```r

# Install packages from GitHub
devtools::install_github("cran/SparkR")
devtools::install_github("cdeterman/gpuR")
# (gpuR) requires a GPU and additional
# dependencies installed, see https://github.com/cdeterman/gpuR/wiki

# List all required packages for RMarkdown files in slides folder
required_packages <- unique(unlist(lapply(list.files("slides", pattern = "\\.Rmd$", full.names = TRUE), function(x) {
  rmd <- readLines(x)
  rmd <- rmd[grep("^library\\(", rmd)]
  gsub("^library\\((.*)\\)$", "\\1", rmd)
})))

# Check if required packages are installed, and install them if not
if (length(setdiff(required_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(required_packages, rownames(installed.packages())))
}
```
Note that some of these packages need lower-level software to be installed (see the output of the package installation messages). Some of the code examples in the slides require a GPU (correctly configured to work with R).

## Compile all materials

```bash
sh makeall.sh
```

# How to contribute

- Open issues:
  - report bugs and typos
  - suggest enhancements


