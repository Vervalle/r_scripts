# On first use, make sure to load the following:
#library( Hmisc )
#library( psych )

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#===============================================================================
current_outlet_label <- "wsj"
print( paste( "***", current_outlet_label, sep="", collapse=NULL ) )
sink()

# get rows for particular outlet or ideology
#current_subset_df <- subset( tweet_df, outlet_label == current_outlet_label )
current_subset_df <- subset( tweet_df, ideology == current_outlet_label )

# check length
current_case_count <- nrow( current_subset_df )

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
current_column_name <- "tweet_retweet_count"
print( paste( "**", current_column_name, sep="", collapse=NULL ) )
sink()

# set current_column
current_column <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
# descriptives

print( "*summary" )
summary( current_column )

print( "*Hmisc::describe" )
Hmisc::describe( current_column )

print( "*psych::describe" )
psych::describe( current_column )
sink()

# histogram (store for CDF)
column_hist <- hist( current_column, plot = FALSE )

# output so I can capture values.  Example:
sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
print( "*histogram" )
column_hist
sink()

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
main_label <- paste( current_outlet_label, " - ", current_column_name )
x_label <- paste( current_column_name )
y_label <- paste( "cases: ", current_case_count )

# output the histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, ".pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
dev.off()

# output the normalized histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-norm.pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
dev.off()

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
current_column_name <- "user_follower_count"
print( paste( "**", current_column_name, sep="", collapse=NULL ) )
sink()

# set current_column
current_column <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
# descriptives

print( "*summary" )
summary( current_column )

print( "*Hmisc::describe" )
Hmisc::describe( current_column )

print( "*psych::describe" )
psych::describe( current_column )
sink()

# histogram (store for CDF)
column_hist <- hist( current_column, plot = FALSE )

# output so I can capture values.  Example:
sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
print( "*histogram" )
column_hist
sink()

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
main_label <- paste( current_outlet_label, " - ", current_column_name )
x_label <- paste( current_column_name )
y_label <- paste( "cases: ", current_case_count )

# output the histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, ".pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
dev.off()

# output the normalized histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-norm.pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
dev.off()

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
current_column_name <- "user_favorites_count"
print( paste( "**", current_column_name, sep="", collapse=NULL ) )
sink()

# set current_column
current_column <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
# descriptives

print( "*summary" )
summary( current_column )

print( "*Hmisc::describe" )
Hmisc::describe( current_column )

print( "*psych::describe" )
psych::describe( current_column )
sink()

# histogram (store for CDF)
column_hist <- hist( current_column, plot = FALSE )

# output so I can capture values.  Example:
sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
print( "*histogram" )
column_hist
sink()

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
main_label <- paste( current_outlet_label, " - ", current_column_name )
x_label <- paste( current_column_name )
y_label <- paste( "cases: ", current_case_count )

# output the histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, ".pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
dev.off()

# output the normalized histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-norm.pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
dev.off()

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
current_column_name <- "tweet_user_mention_count"
print( paste( "**", current_column_name, sep="", collapse=NULL ) )
sink()

# set current_column
current_column <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
# descriptives

print( "*summary" )
summary( current_column )

print( "*Hmisc::describe" )
Hmisc::describe( current_column )

print( "*psych::describe" )
psych::describe( current_column )
sink()

# histogram (store for CDF)
column_hist <- hist( current_column, plot = FALSE )

# output so I can capture values.  Example:
sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
print( "*histogram" )
column_hist
sink()

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
main_label <- paste( current_outlet_label, " - ", current_column_name )
x_label <- paste( current_column_name )
y_label <- paste( "cases: ", current_case_count )

# output the histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, ".pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
dev.off()

# output the normalized histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-norm.pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
dev.off()

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
current_column_name <- "tweet_hashtag_mention_count"
print( paste( "**", current_column_name, sep="", collapse=NULL ) )
sink()

# set current_column
current_column <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
# descriptives

print( "*summary" )
summary( current_column )

print( "*Hmisc::describe" )
Hmisc::describe( current_column )

print( "*psych::describe" )
psych::describe( current_column )
sink()

# histogram (store for CDF)
column_hist <- hist( current_column, plot = FALSE )

# output so I can capture values.  Example:
sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
print( "*histogram" )
column_hist
sink()

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
main_label <- paste( current_outlet_label, " - ", current_column_name )
x_label <- paste( current_column_name )
y_label <- paste( "cases: ", current_case_count )

# output the histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, ".pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
dev.off()

# output the normalized histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-norm.pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
dev.off()

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
current_column_name <- "tweet_url_count"
print( paste( "**", current_column_name, sep="", collapse=NULL ) )
sink()

# set current_column
current_column <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
# descriptives

print( "*summary" )
summary( current_column )

print( "*Hmisc::describe" )
Hmisc::describe( current_column )

print( "*psych::describe" )
psych::describe( current_column )
sink()

# histogram (store for CDF)
column_hist <- hist( current_column, plot = FALSE )

# output so I can capture values.  Example:
sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
print( "*histogram" )
column_hist
sink()

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
main_label <- paste( current_outlet_label, " - ", current_column_name )
x_label <- paste( current_column_name )
y_label <- paste( "cases: ", current_case_count )

# output the histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, ".pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
dev.off()

# output the normalized histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-norm.pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
dev.off()

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
current_column_name <- "tweet_text_length"
print( paste( "**", current_column_name, sep="", collapse=NULL ) )
sink()

# set current_column
current_column <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.

sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
#-----------------------------------------------------------------------------
# descriptives

print( "*summary" )
summary( current_column )

print( "*Hmisc::describe" )
Hmisc::describe( current_column )

print( "*psych::describe" )
psych::describe( current_column )
sink()

# histogram (store for CDF)
column_hist <- hist( current_column, plot = FALSE )

# output so I can capture values.  Example:
sink("render_dist_data-output.txt", append=TRUE, split=TRUE)
print( "*histogram" )
column_hist
sink()

# extract vectors we need for CDF
column_hist_counts <- column_hist$counts
column_hist_breaks <- column_hist$breaks

# remove first $breaks entry, which contains 0.
column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

# make vector that is cumulative sum of counts
column_hist_cum_sum <- cumsum( column_hist_counts )
column_sum <- sum( column_hist_counts )

# use sapply (list version of "apply" function that returns a vector) to normalize.
column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )

# plot the cumulative sum by retweet_count_vector
main_label <- paste( current_outlet_label, " - ", current_column_name )
x_label <- paste( current_column_name )
y_label <- paste( "cases: ", current_case_count )

# output the histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, ".pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
dev.off()

# output the normalized histogram.
pdf( paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-norm.pdf", sep="", collapse=NULL ) )
plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
dev.off()