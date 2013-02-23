CDF\_and\_KS\_test
=================

Scripts for R statistics software.

Contains scripts for creating CDF plots and performing the Kolmogorov-Smirnov test on them to analyze the difference between distrubutions.

Usage
-----

* The main method in CDF\_and\_KS\_test\_functions.r is compare\_distributions(), defined at the bottom of the file.  It shows how to call the other functions in the file.
* To just create aggregated CDF plots, you can call render\_CDFs\_by\_variable().  This is easily used for any data set, not just the twitter data these functions were initially designed for.  You create a set of labels, then a list of variables you want to make plots for, then a map of labels to data frames.  For each variable, will loop over labels, pulling in data frame and adding CDF plot for that variable and label to an aggregated plot.  See header of this function for specifics on arguments.  See the function generate\_aggregate\_CDFs() for examples of how to set up arguments.

Files
-----

* CDF-scratchpad.r - This file contains a basic overview of how CDFs are created from normalized histograms.
* KS\_test-scratchpad.r - Contains a basic overview of how to run K-S test on two different distributions.
* CDF\_and\_KS\_test-scratch\_file.r - Code file where I first tried to run through all analysis, before realizing I needed to break it out into functions.
* CDF\_and\_KS\_test\_functions.r - Final, reusable and configurable functions for CDF and K-S test analysis.