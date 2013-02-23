# On first use, make sure to load the following:
# install.packages( 'Hmisc' )
library( Hmisc )
# install.packages( 'psych' )
library( psych )


#===============================================================================
# function: render_column_data
# purpose: Generates descriptive statistics, histograms, and CDFs for named
#    column read from data frame passed in.
# preconditions: Must pass in a data frame and a column name. Also need to have
#    directories set up:
#    - one each of "output/<every_label>" for each outlet and ideology.
#    - other directories if paths are passed in.
#===============================================================================

render_column_data <- function( data_frame_IN = NULL, column_name_IN = "", outlet_label_IN = "", sink_file_IN = "render_dist_data-output.txt", hist_pdf_path_IN = "", norm_hist_pdf_path_IN = "", cdf_pdf_path_IN = "", norm_cdf_pdf_path_IN = "" ) {

    # got a column name?
    if ( column_name_IN != "" ) {

        # got a data frame?
        if ( is.null( data_frame_IN ) == FALSE ) {

            # place data frame, column name into their variables.
    
            sink( sink_file_IN, append=TRUE, split=TRUE )
            #-------------------------------------------------------------------
            current_column_name <- column_name_IN
            print( paste( "**", current_column_name, sep="", collapse=NULL ) )
            sink()
            
            # get data for current_column
            current_subset_df <- data_frame_IN
            current_column_vector <- current_subset_df[ , current_column_name ] # - this IS EQUIVALENT to "$" notation.
    
            # check length
            current_case_count <- nrow( current_subset_df )

            # set the current outlet label
            current_outlet_label <- outlet_label_IN
 
            sink( sink_file_IN, append=TRUE, split=TRUE )
            #-------------------------------------------------------------------
            # descriptives
            
            print( "*summary" )
            print( summary( current_column_vector ) )
            
            print( "*Hmisc::describe" )
            print( Hmisc::describe( current_column_vector ) )
            
            print( "*psych::describe" )
            print( psych::describe( current_column_vector ) )
            sink()

            # labels for histogram, CDF plots
            main_label <- paste( current_outlet_label, " - ", current_column_name )
            x_label <- paste( current_column_name )
            y_label <- paste( "cases: ", current_case_count )
            
            # histogram path (store for CDF)
            hist_pdf_path <- paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-hist.pdf", sep="", collapse=NULL )

            # overridden from outside?
            if ( hist_pdf_path_IN != "" ) {
                hist_pdf_path <- hist_pdf_path_IN
            }
            
            # histogram
            pdf( hist_pdf_path, onefile = TRUE )
            column_hist <- hist( current_column_vector, labels = TRUE, xlab = x_label, ylab = y_label, main = main_label )
            dev.off()
            #column_hist <- hist( current_column_vector, plot = FALSE )

            # normalize the histogram (percentages instead of counts)
            column_hist$density = ( column_hist$counts / sum( column_hist$counts ) )

            # normalized histogram path (store for CDF)
            norm_hist_pdf_path <- paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-hist-norm.pdf", sep="", collapse=NULL )

            # overridden from outside?
            if ( norm_hist_pdf_path_IN != "" ) {
                norm_hist_pdf_path <- norm_hist_pdf_path_IN
            }
            
            # plot normalized histogram.
            pdf( norm_hist_pdf_path, onefile = TRUE )
            plot( column_hist, freq = FALSE, labels = TRUE, xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) )
            dev.off()
            
            # output so I can capture values.  Example:
            sink( sink_file_IN, append=TRUE, split=TRUE )
            print( "*histogram" )
            print( column_hist )
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
            
            # CDF path
            cdf_pdf_path <- paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-cdf.pdf", sep="", collapse=NULL )

            # overridden from outside?
            if ( cdf_pdf_path_IN != "" ) {
                cdf_pdf_path <- cdf_pdf_path_IN
            }
            
            # output the CDF.
            pdf( cdf_pdf_path, onefile = TRUE )
            plot( column_hist_breaks, column_hist_cum_sum, type = 'l', xlab = x_label, ylab = y_label, main = main_label ) # un-normalized.
            dev.off()
            
            # normalized CDF path
            norm_cdf_pdf_path <- paste( "output/", current_outlet_label, "/", current_outlet_label, "-", current_column_name, "-cdf-norm.pdf", sep="", collapse=NULL )

            # overridden from outside?
            if ( norm_cdf_pdf_path_IN != "" ) {
                norm_cdf_pdf_path <- norm_cdf_pdf_path_IN
            }
            
            # output the normalized CDF.
            pdf( norm_cdf_pdf_path, onefile = TRUE )
            plot( column_hist_breaks, column_hist_cum_sum_normal, type='l', xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) ) # normalized.
            dev.off()

        } #-- END check to see if we have a data set. --#

    } #-- END check to see if we have a column name --#

} #-- END function render_column_data() --#


#===============================================================================
# function: render_all_dist_data
# purpose: Plot normalized, log-transformed CDFs for each variable, where each
#    variable has a single graph that contains all plots.
# preconditions: Must pass in a data frame and a boolean that says whether we
#    group plots in output folders by variable or label.  Also need to have
#    directories set up:
#    - output/by_variable/outlets/
#    - output/by_variable/ideology/
#    - one each of "output/<every_label>" for each outlet and ideology.
#===============================================================================

render_all_dist_data <- function( data_frame_IN = NULL, group_plots_by_variable_IN = FALSE ) {

    # make sure that the data frame isn't NULL
    if ( is.null( data_frame_IN ) == FALSE ) { 
    
        # make a vector of news outlet identifiers
        news_outlet_labels <- c( "bbc", "cbs", "cnn", "drudge", "fox", "huffington", "msnbc", "npr", "nyt", "usatoday", "wpo", "wsj" )
        ideology_labels <- c( "conservative", "liberal", "control" )

        # also make a vector of all the column names that we will be processing.
        column_names <- c( "tweet_retweet_count", "user_follower_count", "user_favorites_count", "tweet_user_mention_count", "tweet_hashtag_mention_count", "tweet_url_count", "tweet_text_length" )
    
        #loop over news outlet labels
        for ( current_label in news_outlet_labels ) {
    
            # set path to news outlet output file.
            news_outlet_output_file <- "render_dist_data-output-news_outlets.txt"

            # log current outlet.
            sink( news_outlet_output_file, append=TRUE, split=TRUE)
            print( paste( "***", current_label, sep="", collapse=NULL ) )
            sink()
    
            # get rows for particular outlet
            current_subset_df <- subset( tweet_df, outlet_label == current_label )

            # loop over column names, rendering data for each.
            for ( current_column in column_names ) {

                print( current_column )

                # group plots by variable?
                if ( group_plots_by_variable_IN == TRUE ) {

                    # make file name for current variable.
                    current_var_hist_file = paste(  "output/by_variable/outlets/", current_label , "-", current_column, "-hist.pdf", sep = "", collapse = NULL )
                    current_var_hist_norm_file = paste(  "output/by_variable/outlets/", current_label , "-", current_column, "-hist-norm.pdf", sep = "", collapse = NULL )
                    current_var_cdf_file = paste(  "output/by_variable/outlets/", current_label , "-", current_column, "-cdf.pdf", sep = "", collapse = NULL )
                    current_var_cdf_norm_file = paste(  "output/by_variable/outlets/", current_label , "-", current_column, "-cdf-norm.pdf", sep = "", collapse = NULL )

                    # for each column, call render_column_data
                    render_column_data( data_frame_IN = current_subset_df, column_name_IN = current_column, outlet_label_IN = current_label, sink_file_IN = news_outlet_output_file, hist_pdf_path_IN = current_var_hist_file, norm_hist_pdf_path_IN = current_var_hist_norm_file, cdf_pdf_path_IN = current_var_cdf_file, norm_cdf_pdf_path_IN = current_var_cdf_norm_file )

                } else {

                    # for each column, call render_column_data
                    render_column_data( data_frame_IN = current_subset_df, column_name_IN = current_column, outlet_label_IN = current_label, sink_file_IN = news_outlet_output_file )

                }

            } #-- END loop over column names. --#
    
        } #-- END for loop over news outlets. --#

        #loop over ideological categories
        for ( current_label in ideology_labels ) {
    
            # set path to ideology output file.
            ideology_output_file <- "render_dist_data-output-ideology.txt"

            sink( ideology_output_file, append=TRUE, split=TRUE)
            print( paste( "***", current_label, sep="", collapse=NULL ) )
            sink()
    
            # get rows for particular outlet
            #current_subset_df <- subset( tweet_df, outlet_label == current_outlet_label )
            current_subset_df <- subset( tweet_df, ideology == current_label )
    
            # loop over column names, rendering data for each.
            for ( current_column in column_names ) {

                # for each column, call render_column_data
                print( current_column )

                # group plots by variable?
                if ( group_plots_by_variable_IN == TRUE ) {

                    # make file name for current variable.
                    current_var_hist_file = paste(  "output/by_variable/ideology/", current_label , "-", current_column, "-hist.pdf", sep = "", collapse = NULL )
                    current_var_hist_norm_file = paste(  "output/by_variable/ideology/", current_label , "-", current_column, "-hist-norm.pdf", sep = "", collapse = NULL )
                    current_var_cdf_file = paste(  "output/by_variable/ideology/", current_label , "-", current_column, "-cdf.pdf", sep = "", collapse = NULL )
                    current_var_cdf_norm_file = paste(  "output/by_variable/ideology/", current_label , "-", current_column, "-cdf-norm.pdf", sep = "", collapse = NULL )

                    # for each column, call render_column_data
                    render_column_data( data_frame_IN = current_subset_df, column_name_IN = current_column, outlet_label_IN = current_label, sink_file_IN = news_outlet_output_file, hist_pdf_path_IN = current_var_hist_file, norm_hist_pdf_path_IN = current_var_hist_norm_file, cdf_pdf_path_IN = current_var_cdf_file, norm_cdf_pdf_path_IN = current_var_cdf_norm_file )

                } else {

                    # for each column, call render_column_data
                    render_column_data( data_frame_IN = current_subset_df, column_name_IN = current_column, outlet_label_IN = current_label, sink_file_IN = news_outlet_output_file )

                }

            } #-- END loop over column names. --#

        } #-- END for loop over ideology buckets. --#

    } #-- END check to see if we have a data frame --#

} #-- END function render_all_dist_data --#


#===============================================================================
# function: render_CDFs_by_variable
# purpose: Plot normalized, log-transformed CDFs for each variable, where each
#    variable has a single graph that contains all plots.
# preconditions: Must pass in a list of labels and a data hash thta maps each
#    label to a dataframe.
#===============================================================================

render_CDFs_by_variable <- function( variable_list_IN = NULL, variable_to_max_hash_IN = NULL, label_list_IN = NULL, label_to_df_hash_IN = NULL, output_file_type_IN = "", color_count_IN = NULL, output_directory_path_IN = NULL, font_size_multiplier_IN = NULL ) {

    # process input parameters
    ok_to_process <- TRUE
    label_list_length <- 0
    color_count <- 0
    output_directory_path <- ""
    font_size_multiplier <- 1

    # got variable list?
    if ( is.null( variable_list_IN ) == TRUE ) { 

        # no variables to loop over - error.
        ok_to_process <- FALSE

    }

    # got label list?
    if ( is.null( label_list_IN ) == TRUE ) { 

        # no labels to loop over - error.
        ok_to_process <- FALSE

    } else {

        # got a list.  Get its length.
        label_list_length <- length( label_list_IN )

    }

    # got map of labels to data frames?
    if ( is.null( label_to_df_hash_IN ) == TRUE ) {

        # no data frames - error.
        ok_to_process <- FALSE

    }

    # color count?
    if ( is.null( color_count_IN ) == FALSE ) {

        # got a color count - set palette.
        color_count <- color_count_IN

    } else {

        # no count passed in - use length of list.
        color_count <- label_list_length

    }
    
    # output directory path?
    if ( is.null( output_directory_path_IN ) == FALSE ) {

        # got an output path - use it.
        output_directory_path <- output_directory_path_IN

    } else {

        # no output path passed in.  Use default.
        output_directory_path <- "output/aggregated_cdfs/"

    }
    
    # font size multiplier
    if ( is.null( font_size_multiplier_IN ) == FALSE ) {
    
        # Value passed in.  Use it.
        font_size_multiplier <- font_size_multiplier_IN
        
    }

    # Set color palette using rainbow, either length of list or number passed in
    #    above.
    palette( rainbow( color_count ) )

    # Good to go?
    if ( isTRUE( isTRUE( ok_to_process ) ) ) {

        #loop over variable names
        current_index <- 0
        for ( current_variable in variable_list_IN ) {

            # for each variable, open an outlet pdf
            variable_pdf_path <- paste( output_directory_path, current_variable, "-", output_file_type_IN, "-CDFs.pdf", sep="", collapse=NULL )
            pdf( variable_pdf_path, onefile = TRUE )

            # got a max value?
            variable_max <- 0
            if ( is.null( variable_to_max_hash_IN ) == FALSE ) {

                # there is a hash, just pull max from pre-generated hash.
                variable_max <- variable_to_max_hash_IN[[ current_variable ]]

            } else {

                # we don't.  Loop over the labels to generate max.
                for ( current_label in label_list_IN ) {

                    # get data set and vector for current variable.
                    current_subset_df <- label_to_df_hash_IN[[ current_label ]]
                    current_variable_vector <- current_subset_df[ , current_variable ] # - this IS EQUIVALENT to "$" notation.

                    # set current_max to greater of current_max and max of vector.
                    variable_max <- max( variable_max, max( current_variable_vector ) )

                }

            } #-- END check to see if we have a way to create a max. ---#

            print( paste( "variable_max: ", variable_max ) )

            # did we find a max?
            if ( variable_max > 0 ) {

                # we did - set break points for high resolution.
                break_points <- seq( 0, variable_max, by = 1 )

            } else {

                # no max - set break_points to hist default.
                break_points <- "Sturges"           

            }

            # loop over labels - make histogram, CDF, then output.
            current_index <- 0
            for ( current_label in label_list_IN ) {

                # increment current_index
                current_index <- current_index + 1

                # get data for current_column
                current_subset_df <- label_to_df_hash_IN[[ current_label ]]
                current_variable_vector <- current_subset_df[ , current_variable ] # - this IS EQUIVALENT to "$" notation.
        
                # check length
                current_case_count <- nrow( current_subset_df )
                
                print( paste( "Variable: ", current_variable, "; outlet: ", current_label, "; case count: ", current_case_count ) )

                # to start, generate histogram object.
                # break_points = seq( 0, 2000000, 10 ) # setting break_points above
                column_hist <- hist( current_variable_vector, plot = FALSE, breaks = break_points )
                
                # normalize the histogram (percentages instead of counts)
                #column_hist$density = ( column_hist$counts / sum( column_hist$counts ) )
                #plot( column_hist, freq = FALSE, labels = TRUE, xlab = x_label, ylab = paste( y_label, " (%)" ), main = paste( main_label, " (normalized)" ) )

                # extract vectors we need for CDF
                column_hist_counts <- column_hist$counts
                column_hist_breaks <- column_hist$breaks
                
                # remove first $breaks entry, which contains 0.
                #column_hist_breaks <- column_hist_breaks[ 2 : length( column_hist_breaks ) ]

                # add entry to $counts which contains 0.
                column_hist_counts <- c( 1, column_hist_counts )
                
                # make vector that is cumulative sum of counts
                column_hist_cum_sum <- cumsum( column_hist_counts )
                column_sum <- sum( column_hist_counts )
                
                # use sapply (list version of "apply" function that returns a
                #   vector) to normalize Y.
                column_hist_cum_sum_normal <- sapply( column_hist_cum_sum, function( i ) { i / column_sum } )
                
                # use sapply to log-transform X.
                column_hist_breaks_log <- sapply( column_hist_breaks, log )

                # first time through?
                if ( current_index == 1 ) {

                    # labels for histogram, CDF plots
                    main_label <- paste( current_variable )
                    x_label <- paste( current_variable, " (log-transformed)" )
                    y_label <- paste( "percentage of cases" )

                    # Make a plot
                    plot( column_hist_breaks_log, column_hist_cum_sum_normal, type='l', col = current_index, lwd = 2, xlab = x_label, ylab = y_label, main = main_label, cex = font_size_multiplier, cex.lab = font_size_multiplier, cex.axis = font_size_multiplier, cex.main = font_size_multiplier, cex.sub = font_size_multiplier )

                } else {

                    # add line to plot.
                    lines( column_hist_breaks_log, column_hist_cum_sum_normal, type='l', col = current_index, lwd = 2 )

                }

            } #-- END loop over news outlets. --#

            # output the legend.
            legend( "bottomright", legend = label_list_IN, col = palette( rainbow( 12 ) ), inset = 0.05, lty = "solid", lwd = 3 )

            # close the PDF file.
            dev.off()

        } #-- END loop over variable names. --#

    } #-- END check to see if input arguments are OK --#

} #-- END function render_CDFs_by_variable() --#


#===============================================================================
# function: render_aggregate_CDFs
# purpose: Plot normalized, log-transformed CDFs for each variable for outlets
#    and ideologies, where each variable has a single graph that contains all
#    plots.
# preconditions: Must have loaded our data_frame, pass it in.
#===============================================================================

render_aggregate_CDFs <- function( data_frame_IN = NULL, variable_names_IN = NULL, output_directory_path_IN = NULL, font_size_multiplier_IN = NULL ) {

    # declare variables
    variable_names <- vector()
    output_directory_path <- NULL
    font_size_multiplier <- 1

    # make sure that the data frame isn't NULL
    if ( is.null( data_frame_IN ) == FALSE ) { 
    
        # if vector of variable names not passed in, set to default vector.
        if ( is.null( variable_names_IN ) == TRUE ) { 

            # make a vector of all the variable names that we will be processing.
            variable_names <- c( "tweet_retweet_count", "user_follower_count", "user_favorites_count", "tweet_user_mention_count", "tweet_hashtag_mention_count", "tweet_url_count", "tweet_text_length" )
            
        } else {
         
            variable_names <- variable_names_IN
            
        }
        
        # check to see if there is a directory path passed in.
        if ( is.null( output_directory_path_IN ) == FALSE ) {
        
            # Value passed in, use it.
            output_directory_path <- output_directory_path_IN
           
        } 
    
        # check to see if there is a font size multiplier passed in.
        if ( is.null( font_size_multiplier_IN ) == FALSE ) {
        
            # Value passed in, set to empty.
            font_size_multiplier <- font_size_multiplier_IN
           
        }

        # make a vector of news outlet identifiers
        news_outlet_labels <- c( "bbc", "cbs", "cnn", "drudge", "fox", "huffington", "msnbc", "npr", "nyt", "usatoday", "wpo", "wsj" )

        # make subsets for each outlet.
        bbc_df <- subset( tweet_df, outlet_label == "bbc" )
        cbs_df <- subset( tweet_df, outlet_label == "cbs" )
        cnn_df <- subset( tweet_df, outlet_label == "cnn" )
        drudge_df <- subset( tweet_df, outlet_label == "drudge" )
        fox_df <- subset( tweet_df, outlet_label == "fox" )
        huffington_df <- subset( tweet_df, outlet_label == "huffington" )
        msnbc_df <- subset( tweet_df, outlet_label == "msnbc" )
        npr_df <- subset( tweet_df, outlet_label == "npr" )
        nyt_df <- subset( tweet_df, outlet_label == "nyt" )
        usatoday_df <- subset( tweet_df, outlet_label == "usatoday" )
        wpo_df <- subset( tweet_df, outlet_label == "wpo" )
        wsj_df <- subset( tweet_df, outlet_label == "wsj" )

        # make list of outlet data frames
        outlet_df_hash_list <- list( "bbc" = bbc_df, "cbs" = cbs_df, "cnn" = cnn_df, "drudge" = drudge_df, "fox" = fox_df, "huffington" = huffington_df, "msnbc" = msnbc_df, "npr" = npr_df, "nyt" = nyt_df, "usatoday" = usatoday_df, "wpo" = wpo_df, "wsj" = wsj_df )

        # render CDFs for news outlets.
        render_CDFs_by_variable( variable_list_IN = variable_names, label_list_IN = news_outlet_labels, label_to_df_hash_IN = outlet_df_hash_list, output_file_type_IN = "outlet", color_count_IN = 12, output_directory_path_IN = output_directory_path, font_size_multiplier_IN = font_size_multiplier )

        # make a vector of ideology identifiers
        ideology_labels <- c( "conservative", "liberal", "control" )

        # make subsets for ideological category.
        conservative_df <- subset( tweet_df, ideology == "conservative" )
        control_df <- subset( tweet_df, ideology == "control" )
        liberal_df <- subset( tweet_df, ideology == "liberal" )

        # make list of ideology data frames
        ideology_df_hash_list <- list( "conservative" = conservative_df, "control" = control_df, "liberal" = liberal_df )

        # render CDFs for ideologies.
        render_CDFs_by_variable( variable_list_IN = variable_names, label_list_IN = ideology_labels, label_to_df_hash_IN = ideology_df_hash_list, output_file_type_IN = "ideology", output_directory_path_IN = output_directory_path, font_size_multiplier_IN = font_size_multiplier )

        # make a vector of heterogeneity identifiers
        heterogeneity_labels <- c( "low", "medium", "high" )

        # make subsets for heterogeneity category.
        h_low_df <- subset( tweet_df, heterogeneity == "low" )
        h_medium_df <- subset( tweet_df, heterogeneity == "medium" )
        h_high_df <- subset( tweet_df, heterogeneity == "high" )

        # make list of heterogeneity data frames
        heterogeneity_df_hash_list <- list( "low" = h_low_df, "medium" = h_medium_df, "high" = h_high_df )

        # render CDFs for heterogeneity.
        render_CDFs_by_variable( variable_list_IN = variable_names, label_list_IN = heterogeneity_labels, label_to_df_hash_IN = heterogeneity_df_hash_list, output_file_type_IN = "heterogeneity", output_directory_path_IN = output_directory_path, font_size_multiplier_IN = font_size_multiplier )

    } #-- END check to see if we have a data frame --#

} #-- END function render_aggregate_CDFs --#


#===============================================================================
# function: compare_variable_distributions
# purpose: Creates a CSV file of different comparison statistics for each
#    variable in variable_list_IN, compared across the different data frames
#    whose identifiers are stored in label_list_IN (generally different subsets
#    of a larger master data set), 
# preconditions: Must pass in a list of variables to compare, data frame ids,
#    and each data frame ID should map to a data frame in the id_to_df_hash_IN
#    list.
#===============================================================================

compare_variable_distributions <- function( variable_list_IN = NULL, df_id_list_IN = NULL, id_to_df_hash_IN = NULL, output_directory_path_IN = "." ) {

    # process input parameters
    ok_to_process <- TRUE

    # declare variables
    output_data_frame <- data.frame( id = c() )
    values_per_pair <- 4 # total things appended to a given row for each comparison.

    # got variable list?
    if ( is.null( variable_list_IN ) == TRUE ) { 

        # no variables to loop over - error.
        ok_to_process <- FALSE

    }

    # got data frame ID list?
    if ( is.null( df_id_list_IN ) == TRUE ) { 

        # no IDs to loop over - error.
        ok_to_process <- FALSE

    } else {

        # got a list.  Get its length.
        df_list_length <- length( df_id_list_IN )

    }

    # got map of labels to data frames?
    if ( is.null( id_to_df_hash_IN ) == TRUE ) {

        # no data frames - error.
        ok_to_process <- FALSE

    }

    # Good to go?
    if ( isTRUE( isTRUE( ok_to_process ) ) ) {

        #loop over variable names
        current_index <- 0
        for ( current_variable in variable_list_IN ) {

            # first, need to make data frame.
            output_data_frame <- data.frame( id = character( df_list_length ), variable = character( df_list_length ), stringsAsFactors = FALSE )

            # !!! If you will have character columns, you MUST set
            #    stringsAsFactors = FALSE, else they will be turned into
            #    factors, causing mayhem (all strings assigned to this column
            #    will be NA).

            # ALSO - R likes it better when you pre-allocate your memory, rather
            #    than just append things as you go.  So, pre-allocating data
            #    frame, then placing values into it.  Ugly.  I actually didn't
            #    do this below.  I commented this efficient code out, because
            #    it just makes for more complicated code - having to set offsets
            #    and counters by hand.  Again, ugly.

            # also make vector of column names
            column_name_vector <- c( "id", "variable" )
            

            print( paste( "current variable: ", current_variable ) ) 

            # then, add columns for each statistic we will compute and each ID.
            for ( current_df_id in df_id_list_IN ) {

                # add columns for each statistic/ID pair.

                # Kolmogorov-Smirnov
                current_column <- paste( current_df_id, "-KS_test", sep="", collapse=NULL )
                output_data_frame[ , current_column ] <- numeric( df_list_length ) # - this IS EQUIVALENT to "$" notation.
                column_name_vector <- c( column_name_vector, current_column )

                current_column <- paste( current_df_id, "-KS_test-p", sep="", collapse=NULL )
                output_data_frame[ , current_column ] <- numeric( df_list_length ) # - this IS EQUIVALENT to "$" notation.
                column_name_vector <- c( column_name_vector, current_column )

                # Mann-Whitney
                current_column <- paste( current_df_id, "-MW_test", sep="", collapse=NULL )
                output_data_frame[ , current_column ] <- numeric( df_list_length ) # - this IS EQUIVALENT to "$" notation.
                column_name_vector <- c( column_name_vector, current_column )

                current_column <- paste( current_df_id, "-MW_test-p", sep="", collapse=NULL )
                output_data_frame[ , current_column ] <- numeric( df_list_length ) # - this IS EQUIVALENT to "$" notation.
                column_name_vector <- c( column_name_vector, current_column )

            }

            #print( "--- initial output_data_frame:" )
            #print( output_data_frame ) 

            # loop over data frame ids twice, outer loop = get current id, inner
            #    loop = get IDs to compare it to.  Add results to a vector that
            #    we'll then add to our data frame.
            outer_id_counter <- 0
            for ( current_df_id in df_id_list_IN ) {

                # increment outer_id_counter
                outer_id_counter <- outer_id_counter + 1

                print( paste( "* current outer id: ", current_df_id ) ) 

                # get data for current_column
                current_df <- id_to_df_hash_IN[[ current_df_id ]]
                current_variable_vector <- current_df[ , current_variable ] # - this IS EQUIVALENT to "$" notation.

                # make vector to hold values we will add to data frame.
                #row_vector <- c( outer_id_counter )
                row_vector <- c( current_df_id, current_variable )
                #output_data_frame[ outer_id_counter, 1 ] <- as.character( current_df_id )

                # do inner loop over id list for comparison vectors.
                inner_id_counter <- 0
                for ( current_comparison_df_id in df_id_list_IN ) {

                    print( paste( "** current inner id: ", current_comparison_df_id ) )

                    # housekeeping
                    current_base_index <- inner_id_counter * values_per_pair + 2
                    inner_id_counter <- inner_id_counter + 1

                    # get data frame and variable vector for comparison variable.
                    comparison_df <- id_to_df_hash_IN[[ current_comparison_df_id ]]
                    comparison_vector <- comparison_df[ , current_variable ] # - this IS EQUIVALENT to "$" notation.

                    # run tests, add to output data frame.

                    # Kolmogorov-Smirnov
                    ks_results <- ks.test( current_variable_vector, comparison_vector )
                    ks_statistic <- ks_results$statistic
                    ks_p_value <- ks_results$p.value
                    
                    # instead of building rows, just plunk into properly sized data.frame
                    row_vector <- c( row_vector, ks_statistic, ks_p_value )
                    #output_data_frame[ outer_id_counter, current_base_index ] <- ks_statistic
                    #output_data_frame[ outer_id_counter, ( current_base_index + 1 ) ] <- ks_p_value

                    # Mann-Whitney
                    mw_results <- wilcox.test( current_variable_vector, comparison_vector )
                    mw_statistic <- mw_results$statistic
                    mw_p_value <- mw_results$p.value

                    # instead of building rows, just plunk into properly sized data.frame
                    row_vector <- c( row_vector, mw_statistic, mw_p_value )
                    #output_data_frame[ outer_id_counter, ( current_base_index + 2 ) ] <- mw_statistic
                    #output_data_frame[ outer_id_counter, ( current_base_index + 3 ) ] <- mw_p_value

                    #print( "*** --- current row_vector: " )
                    #print( row_vector ) 

                } #-- END inner comparison loop. --#

                # Add row vector to data frame
                output_data_frame[ outer_id_counter, ] <- row_vector

                #print( "** --- current output_data_frame:" ) 
                #print( output_data_frame ) 

            } #-- END loop over data frame ids. --#

            # for each variable, create path for output file
            variable_output_path <- paste( output_directory_path_IN, "/", current_variable, "-comparisons.csv", sep="", collapse=NULL )

            # set column names on data frame
            colnames( output_data_frame ) <- column_name_vector

            # output the data frame as a CSV file
            write.table( output_data_frame, file = variable_output_path, sep = ",", col.names = NA )
    
        } #-- END loop over variable names. --#

    } #-- END check to see if input arguments are OK --#

} #-- END function compare_variable_distributions() --#


#===============================================================================
# function: compare_distributions
# purpose: Plot normalized, log-transformed CDFs for each variable for outlets
#    and ideologies, where each variable has a single graph that contains all
#    plots.
# preconditions: Must have loaded our data_frame, pass it in.
#===============================================================================

compare_distributions <- function( data_frame_IN = NULL, variable_names_IN = NULL, output_directory_path_IN = NULL ) {

    # declare variables
    variable_names <- vector()
    output_directory_path <- NULL

    # if vector of variable names not passed in, set to default vector.
    if ( is.null( variable_names_IN ) == TRUE ) { 

        # make a vector of all the variable names that we will be processing.
        variable_names <- c( "tweet_retweet_count", "user_follower_count", "user_favorites_count", "tweet_user_mention_count", "tweet_hashtag_mention_count", "tweet_url_count", "tweet_text_length" )
        
    } else {
        
        variable_names <- variable_names_IN
        
    }
    
    # check to see if there is a directory path passed in.
    if ( is.null( output_directory_path_IN ) == FALSE ) {
    
        # Value passed in, use it.
        output_directory_path <- output_directory_path_IN
        
    } else {
        
        # No value passed in, use default.
        output_directory_path <- "output/dist_stats"
        
    }

    # make sure that the data frame isn't NULL
    if ( is.null( data_frame_IN ) == FALSE ) { 
    
        # make a vector of news outlet identifiers
        news_outlet_labels <- c( "bbc", "cbs", "cnn", "drudge", "fox", "huffington", "msnbc", "npr", "nyt", "usatoday", "wpo", "wsj" )

        # make subsets for each outlet.
        bbc_df <- subset( tweet_df, outlet_label == "bbc" )
        cbs_df <- subset( tweet_df, outlet_label == "cbs" )
        cnn_df <- subset( tweet_df, outlet_label == "cnn" )
        drudge_df <- subset( tweet_df, outlet_label == "drudge" )
        fox_df <- subset( tweet_df, outlet_label == "fox" )
        huffington_df <- subset( tweet_df, outlet_label == "huffington" )
        msnbc_df <- subset( tweet_df, outlet_label == "msnbc" )
        npr_df <- subset( tweet_df, outlet_label == "npr" )
        nyt_df <- subset( tweet_df, outlet_label == "nyt" )
        usatoday_df <- subset( tweet_df, outlet_label == "usatoday" )
        wpo_df <- subset( tweet_df, outlet_label == "wpo" )
        wsj_df <- subset( tweet_df, outlet_label == "wsj" )

        # make list of outlet data frames
        outlet_df_hash_list <- list( "bbc" = bbc_df, "cbs" = cbs_df, "cnn" = cnn_df, "drudge" = drudge_df, "fox" = fox_df, "huffington" = huffington_df, "msnbc" = msnbc_df, "npr" = npr_df, "nyt" = nyt_df, "usatoday" = usatoday_df, "wpo" = wpo_df, "wsj" = wsj_df )

        # compare variable distributions for news outlets.
        compare_variable_distributions( variable_list_IN = variable_names, df_id_list_IN = news_outlet_labels, id_to_df_hash_IN = outlet_df_hash_list, output_directory_path_IN = paste( output_directory_path, "/outlets" ) )

        # make a vector of ideology identifiers
        ideology_labels <- c( "conservative", "liberal", "control" )

        # make subsets for ideological category.
        conservative_df <- subset( tweet_df, ideology == "conservative" )
        control_df <- subset( tweet_df, ideology == "control" )
        liberal_df <- subset( tweet_df, ideology == "liberal" )

        # make list of ideology data frames
        ideology_df_hash_list <- list( "conservative" = conservative_df, "control" = control_df, "liberal" = liberal_df )

        # compare variable distributions for news outlets.
        compare_variable_distributions( variable_list_IN = variable_names, df_id_list_IN = ideology_labels, id_to_df_hash_IN = ideology_df_hash_list, output_directory_path_IN = paste( output_directory_path, "/ideology" ) )

        # make a vector of heterogeneity identifiers
        heterogeneity_labels <- c( "low", "medium", "high" )

        # make subsets for heterogeneity category.
        h_low_df <- subset( tweet_df, heterogeneity == "low" )
        h_medium_df <- subset( tweet_df, heterogeneity == "medium" )
        h_high_df <- subset( tweet_df, heterogeneity == "high" )

        # make list of heterogeneity data frames
        heterogeneity_df_hash_list <- list( "low" = h_low_df, "medium" = h_medium_df, "high" = h_high_df )

        # compare variable distributions for news outlets.
        compare_variable_distributions( variable_list_IN = variable_names, df_id_list_IN = heterogeneity_labels, id_to_df_hash_IN = heterogeneity_df_hash_list, output_directory_path_IN = paste( output_directory_path, "/heterogeneity" ) )

    } #-- END check to see if we have a data frame --#

} #-- END function compare_distributions --#