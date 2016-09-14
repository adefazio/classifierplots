# Classifierplots

Generates a visualization of binary classifier performance as a grid of diagonstic plots with just one function call. Includes ROC curves, prediction density, precision-recall and calibration plots, all using  ggplot2 for easy modification. Debug your binary classifiers faster and easier!

### Usage

The main function to use when running R interactively is:

	classifierplots(test.y, pred.prob)
		
Where you pass in ground truth values test.y and predictions in [0,1] as pred.prob.

If you want to save the results to disk as folder of seperate plots as well as a single ALL.pdf grid, use 

	classifierplots_folder(test.y, pred.prob, folder)

If your test dataset is larger than about 100,000 instances, we recommend downsampling first to speed things up a little.

Running example_classifierplots() generates the following example plot. It's also a good test that everything is working correctly.
 
![Example](/example.png?raw=true "Example")
	
### Building

Run from bash in the project directory:

    R CMD build .
        
This produces a tarball: classifierplots_1.3.1.tar.gz

### Checking package

After, building run:

    R CMD check classifierplots_1.3.1.tar.gz
    
It will give you a list of things to fix. Ignore the warning related to licencing.
    
### Installing locally

Run:
    
    R CMD install classifierplots_1.3.1.tar.gz


### Development

In R, just open an R session with the project's directory. All the functions for the package will be imported automatically. To refresh their definitions without restarting R, just run:

	load_all()

