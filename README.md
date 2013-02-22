r_scripts
=========

Scripts for R statistics software.

Nothing too fancy at this point, but wanted a centralized place to store R scripts.  What we have so far:


* /CDF_and_KS_test - this contains scripts for creating CDF plots and performing the Kolmogorov-Smirnov test on them to analyze the difference between distrubutions.  The main method in CDF_and_KS_test_functions.r is compare_distributions(), defined at the bottom of the file.  It shows how to call the other functions in the file.

* /mysql - contains a script that shows how to read data from a mysql database and then store that data as an R data file or as SPSS (though the SPSS routine can be finecky - the comments in there indicate that column names can't be longer than 8 characters, for example, a constraint in old versions of SPSS that went away four or five versions ago).
