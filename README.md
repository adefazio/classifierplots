# Classifierplots

Generates a visualization of binary classifier performance as a grid of diagonstic plots with just one function call. Includes ROC curves, prediction density, accuracy, precision, recall and calibration plots, all using ggplot2 for easy modification.
Debug your binary classifiers faster and easier!

**classifierplots is on cran now**

Install with

	install.packages("classifierplots")

Tested on Windows, Mac OS and Linux.

### Usage

The main function to use when running R interactively is:

	classifierplots(test.y, pred.prob)

Where you pass in ground truth values test.y and predictions in [0,1] as pred.prob.

If you want to save the results to disk as folder of seperate plots as well as a single ALL.pdf grid, use

	classifierplots_folder(test.y, pred.prob, folder)

There are also functions to produce each individual plot which return ggplot2 objects.

##### Runnable example

The small example_predictions dataset is included with the package:

	library(classifierplots)
	# Plot to window
	classifierplots(example_predictions$test.y, example_predictions$pred.prob)
	# Save output directly to disk
	classifierplots_folder(example_predictions$test.y, example_predictions$pred.prob, "outfolder")

![Example](/man/figures/example.png?raw=true "Example")


# Instructions for using the repository version

We recommend using the CRAN version instead, but you can install from Github or from a local clone of the repository as well. The **devtools package is required** for working with the current development version.

### Installing from github with devtools

	library(devtools)
	install_github("ambiata/classifierplots")

### Building locally

Once you have cloned the repository, run from a shell in the project directory:

    R CMD build .

This produces a tarball: classifierplots_1.3.2.tar.gz. Checks:

	R CMD check --as-cran classifierplots_1.3.2.tar.gz

### Installing locally

Run:

    R CMD INSTALL classifierplots_1.3.2.tar.gz

If you need to install the dependencies as well (you probably do), then run first:

	install.packages(c('Rcpp', 'tibble', 'caret', 'gridExtra', 'ggplot2', 'ROCR', 'png', 'data.table'), dependencies=T, type='source')

If you're not using Linux you may be able to omit the type='source' part to speed up the install.

### Development

In R, just open an R session within the project's directory, then run:

	devtools::load_all()

To refresh their definitions without restarting R just run it again.
