# load the functions file
source( "render_dist_data_functions.r" )

# try out the first function, render_column_data.
render_column_data( data_frame_IN = current_subset_df, column_name_IN = "tweet_retweet_count", outlet_label_IN = "cbs", sink_file_IN = "render_dist_data-output-news_outlet.txt" )

# test out Kolmogorov-Smirnov, Mann-Whitney tests.

# make data frames of individual outlet subsets.
bbc_df <- subset( tweet_df, outlet_label == "bbc" )cbs_df <- subset( tweet_df, outlet_label == "cbs" )# grab column vectorsbbc_tweet_retweet_count <- bbc_df[ , "tweet_retweet_count" ]cbs_tweet_retweet_count <- cbs_df[ , "tweet_retweet_count" ]# Run Kolmogorov-Smirnovks_results <- ks.test( bbc_tweet_retweet_count, cbs_tweet_retweet_count )# Warning message:# In ks.test(bbc_tweet_retweet_count, cbs_tweet_retweet_count) :#   p-values will be approximate in the presence of ties# output resultsks_results#	Two-sample Kolmogorov-Smirnov test## data:  bbc_tweet_retweet_count and cbs_tweet_retweet_count # D = 0.0641, p-value < 2.2e-16# alternative hypothesis: two-sided # individual informationks_results$statistic#          D # 0.06411715 ks_results$p.value# [1] 0# Mann-Whitney testmw_results <- wilcox.test( bbc_tweet_retweet_count, cbs_tweet_retweet_count )# outputmw_results# 	Wilcoxon rank sum test with continuity correction## data:  bbc_tweet_retweet_count and cbs_tweet_retweet_count # W = 414511181, p-value < 2.2e-16# alternative hypothesis: true location shift is not equal to 0 # individual informationmw_results$statistic#         W # 414511181 mw_results$p.value# [1] 6.750742e-104