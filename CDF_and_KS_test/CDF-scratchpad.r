# Load data set
setwd( "/Users/jonathanmorgan/Documents/work/research/projects/SoCS/papers/AEJMC/data/R" )
tweet_df <- readRDS( file = "news_tweets.rds" )

# get number of rows in the data frame
nrow( tweet_df )

# get rows for particular outlet or ideology
current_subset_df <- subset( tweet_df, outlet_label == "nyt" )

# check length
nrow( current_subset_df )

# Load hash package.
#install.packages( "hash" ) # if necessary
#library( hash )

#===============================================================================
# Examples
#===============================================================================

# output column names (they are the same as column names in database).
colnames( current_subset_df )

# set current_column
current_column <- current_subset_df$tweet_retweet_count
# current_column <- current_subset_df[,"tweet_retweet_count"] # - this IS EQUIVALENT to "$" notation.
# current_column <- current_subset_df[ "tweet_retweet_count" ] # - this IS NOT EQUIVALENT to "$" notation!

# For analysis, want to do the following:
# * tweet_retweet_count
# * user_follower_count
# * user_favorites_count
# tweet_user_mention_count
# tweet_hashtag_mention_count
# tweet_url_count
# tweet_text_length

# descriptives.
summary( current_column )

# More descriptives from Quick-R: http://www.statmethods.net/stats/descriptives.html
# - Using tweet_retweet_count as example.

# Install and load Hmisc package
install.packages( "Hmisc" ) # if necessary
library( Hmisc )

# describe a column.
Hmisc::describe( current_column )

# tweet_df$tweet_retweet_count 
#       n missing  unique    Mean     .05     .10     .25     .50     .75    .90     .95 
# 1378336       0    3141   18.63       0       0       0       0       0     17      73
#
# lowest :     0     1     2     3     4
# highest:  3584  3585 11030 11031 11032 

# Install and load pastecs package.
install.packages( "pastecs" ) # if necessary
library( pastecs )

# do this describe
pastecs::stat.desc( current_column )

#      nbr.val     nbr.null       nbr.na          min          max 
# 1.378336e+06 1.051107e+06 0.000000e+00 0.000000e+00 1.103200e+04 
#        range          sum       median         mean      SE.mean 
# 1.103200e+04 2.567952e+07 0.000000e+00 1.863081e+01 1.089788e-01 
# CI.mean.0.95          var      std.dev     coef.var 
# 2.135947e-01 1.636965e+04 1.279439e+02 6.867328e+00

# Install and load psych
install.packages( "psych" ) # if necessary.
library( psych )

# and this one - looks like latest package loaded wins when there are name collisions.
psych::describe( current_column )

#   var       n  mean     sd median trimmed mad min   max range  skew
# 1   1 1378336 18.63 127.94      0    0.77   0   0 11032 11032 17.42
#   kurtosis   se
# 1   472.85 0.11

# histogram (store for CDF)
column_hist <- hist( current_column )

# output so I can capture values.  Example:
column_hist
# $breaks
#  [1]     0   500  1000  1500  2000  2500  3000  3500  4000  4500  5000
# [12]  5500  6000  6500  7000  7500  8000  8500  9000  9500 10000 10500
# [23] 11000 11500
# 
# $counts
#  [1] 1369165    5437    1128     885     720     454     463      80
#  [9]       0       0       0       0       0       0       0       0
# [17]       0       0       0       0       0       0       4
# 
# $intensities
#  [1] 1.986693e-03 7.889223e-06 1.636756e-06 1.284157e-06 1.044738e-06
#  [6] 6.587654e-07 6.718246e-07 1.160820e-07 0.000000e+00 0.000000e+00
# [11] 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00
# [16] 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00
# [21] 0.000000e+00 0.000000e+00 5.804100e-09
# 
# $density
#  [1] 1.986693e-03 7.889223e-06 1.636756e-06 1.284157e-06 1.044738e-06
#  [6] 6.587654e-07 6.718246e-07 1.160820e-07 0.000000e+00 0.000000e+00
# [11] 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00
# [16] 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00
# [21] 0.000000e+00 0.000000e+00 5.804100e-09
# 
# $mids
#  [1]   250   750  1250  1750  2250  2750  3250  3750  4250  4750  5250
# [12]  5750  6250  6750  7250  7750  8250  8750  9250  9750 10250 10750
# [23] 11250
# 
# $xname
# [1] "retweet_count_vector"
# 
# $equidist
# [1] TRUE
# 
# attr(,"class")
# [1] "histogram"

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# add 0 to counts.  Could do this to get 0 value at beginning- would be nice to see how many were 0...  Not included in histogram by default, though.
#column_hist_counts <- c( 0, column_hist_counts, recursive=TRUE )

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
plot( column_hist_breaks, column_hist_cum_sum, type='l' ) # un-normalized.
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l' ) # normalized.