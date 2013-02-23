# load the functions file
source( "render_dist_data_functions.r" )

# try out the first function, render_column_data.
render_column_data( data_frame_IN = current_subset_df, column_name_IN = "tweet_retweet_count", outlet_label_IN = "cbs", sink_file_IN = "render_dist_data-output-news_outlet.txt" )

# test out Kolmogorov-Smirnov, Mann-Whitney tests.

# make data frames of individual outlet subsets.
bbc_df <- subset( tweet_df, outlet_label == "bbc" )