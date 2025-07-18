library(dplyr)
library(stringr)

#kpp_data = metadata 
#mapping_data1 = mapping data to map to kpp_data

fc_fasta_mapping <- function(fasta_df, mapping_data) {
  mapping_keys <- mapping_data %>%
    #makes the Laboratory log number standardized
    mutate(
      LabLogRaw = toupper(str_trim(`Laboratory log number`))
    ) %>%
    #replaces dashes and slashes with and X 
    mutate(LabKey = str_replace_all(LabLogRaw, "[-/]", "X")) %>%
    bind_rows(
      mapping_data %>%
    #replaces all Xs with a slash (makes standard naming scheme)
        mutate(
          LabLogRaw = toupper(str_trim(`Laboratory log number`)),
          LabKey    = str_replace_all(LabLogRaw, "[-X]", "/")
        )
    ) %>%
    #removes duplicates 
    distinct(LabKey, .keep_all = TRUE)
  #extracts fasta ID (makes capital, and then uses regex to extract)
  fasta_df2 <- fasta_df %>%
    mutate(
      NormID     = toupper(id) %>% str_replace_all("[-/]", "X"),
      Specimenno = str_extract(
        NormID,
        "(?:FJ|AY)\\d+|[A-Z0-9]+X[A-Z0-9]+"
      ) %>% str_trim()
    )
  #shows first few extracted lines 
  cat("▶ Sample extracted Specimenno values:\n")
  print(head(fasta_df2$Specimenno))
  #Matches the codes and makes a match column 
  merged_df <- fasta_df2 %>%
    left_join(
      mapping_keys,
      by = c("Specimenno" = "LabKey")
    ) %>%
    mutate(
      MatchStatus = if_else(
        is.na(`study number`),
        "Unmatched", "Matched"
      )
    )
  #prints match summary 
  cat("\n▶ Match summary:\n")
  print(table(merged_df$MatchStatus))
  #returns the df 
  return(merged_df)
}
