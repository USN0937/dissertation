fc_fasta_to_df <- function(file_path) {
  # Read the FASTA file as a DNAStringSet object
  sequences <- readDNAStringSet(filepath = file_path)
  
  # Create a data frame with two columns: 'id' and 'sequence'
  df <- data.frame(
    id = names(sequences),
    sequence = as.character(sequences),
    stringsAsFactors = FALSE
  )
  
  return(df)
}