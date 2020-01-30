# DESCRIPTION ############################################################
# This function uses the "block average" method to calculate standard
# errors of spatially or temporally correlated data (e.g. time series).
#
# ARGUMENTS ##############################################################
# The user must provide a numeric vector in x (e.g. a time series), and
#   can optionally provide a numeric vector of block sizes (in block_sizes) 
#   or a numeric vector of number of blocks (in n_blocks) to be evaluated.
# If both vectors are provided, the function prioritize block_sizes.
# If no vectors are provided, the function automatically generates a 
#   vector for block_sizes (it may be correct or not depending on the vector 
#   provided in x)
#
# VALUE ##################################################################
# The function returns a plot with points for each value of standard errors 
#   obtained for each evaluated block size.
# If the function is assigned to an object, it also returns a data. frame
#   with the information needed to make the plot. This may be useful to 
#   fit a mathematical model in order to obtain an estimated asymptote.
#
# REFERENCES
# Here you will find information explaining the "block average" method
# http://sachinashanbhag.blogspot.com/2013/08/block-averaging-estimating-uncertainty.html
# https://www.quora.com/What-is-the-block-average-method
# http://realerthinks.com/block-averaging-bootstrapping-estimating-mean-autocorrelated-data/

block_average <- function(x, block_sizes = NULL, n_blocks = NULL){
  
  # Standard deviation function (for populations)
  sd.p <- function(vec){ sd(vec)*sqrt((length(vec) - 1)/length(vec)) }
  
  if(!is.numeric(n_blocks)) stop("x must be a numeric vector")
  
  data_length <- length(x)
  
  if(is.null(block_sizes))
  {
    if(is.null(n_blocks))
    {
      n_blocks <- seq(from = 5, to = data_length, by = 1)
    }
    else
    {
      if(!is.numeric(n_blocks))
      {
        stop("n_blocks must be a numeric vector")
      }
      else
      {
        if(!all(n_blocks > 0 & n_blocks <= data_length))
          stop("n_blocks is meanless. Provide a proper numeric vector")
      }
    }
    # Calculates the block sizes
    block_sizes <- floor(data_length / n_blocks)
  }
  else
  {
    if(!is.numeric(block_sizes))
    {
      stop("block_sizes must be a numeric vector")
    }
    else
    {
      if(!all(block_sizes > 0 & block_sizes <= data_length))
        stop("block_sizes is meanless. Provide a proper numeric vector")
    }
      
  }
  
  block_sizes <- sort(unique(block_sizes))
  
  # Initialize final data frame
  df <- data.frame(block_size = block_sizes,
                   num_blocks = floor(data_length / block_sizes),
                   mean = rep(NA, length(block_sizes)),
                   se = rep(NA, length(block_sizes)))
  
  # Main loop that makes stuff done
  for(i in 1:length(block_sizes))
  {
    tmp_id <- 1
    tmp_bsize <- df$block_size[i]
    tmp_means <- rep(0, df$num_blocks[i])
    for(j in 1:df$num_blocks[i])
    {
      if((tmp_id + tmp_bsize) > length(x))
      {
        tmp_data   <- x[(tmp_id):length(x)]
      }
      else
      {
        tmp_data   <- x[(tmp_id):(tmp_id + tmp_bsize - 1)]
      }
      
      tmp_means[j] <- mean(tmp_data)
      tmp_id <- tmp_id + tmp_bsize
    }
    
    df$mean[i] <- mean(tmp_means)
    if(length(tmp_means) > 1)
      df$se[i] <- sd.p(tmp_means) / sqrt(df$num_blocks[i] - 1)
    else
      df$se[i] <- sd.p(data$y) / sqrt(df$num_blocks[i] - 1)
  }
  
  # Plots standard error in relation to the block size
  plot(se ~ block_sizes, data = df, xlab = "Block size", ylab = "Standard Error")
  
  invisible(df)
}
