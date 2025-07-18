fc_time_difference <- function(data) {
  # data to data.table
  data_dt <- data.table(data)
  
  n <- nrow(data_dt)
  
  # empty matrix
  time <- matrix(NA, nrow=n, ncol=n)
  
  #for loop for pairwise difference
  for (i in 1:n) {
    time[i, ] <- abs(as.numeric(difftime(data_dt$admitdate, data_dt$admitdate[i], units="days")))
  }
  
  #make a data frame
  time_df <- as.data.frame(time)
  
  #rows and columns naming 
  rownames(time_df) <- data$studyno
  colnames(time_df) <- data$studyno
  
  return(time_df)
}