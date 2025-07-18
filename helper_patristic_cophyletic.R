#PATRISTIC DISTANCE FUNCTION
library(ape)
library(dplyr)
library(stringr)

fc_patristic_consensus <- function(consensus_tree, mapping_data) {
  mapping_keys <- mapping_data %>%
    mutate(LabLogRaw = toupper(str_trim(`Laboratory log number`)),
           LabKey    = str_replace_all(LabLogRaw, "[-/]", "X")) %>%
    bind_rows(
      mapping_data %>%
        mutate(LabLogRaw = toupper(str_trim(`Laboratory log number`)),
               LabKey    = str_replace_all(LabLogRaw, "[-X]", "/"))
    ) %>%
    distinct(LabKey, .keep_all = TRUE)
  
  consensus_tree <- as.phylo(consensus_tree)
  dm             <- dist.nodes(consensus_tree)
  nt             <- Ntip(consensus_tree)
  tip_mat        <- dm[1:nt, 1:nt]
  rownames(tip_mat) <- colnames(tip_mat) <- consensus_tree$tip.label
  pat_df         <- as.data.frame(as.matrix(tip_mat))
  
  pat_lookup <- tibble(
    OriginalName = rownames(pat_df),
    LabKey = str_extract(
      toupper(OriginalName) %>% str_replace_all("[-/]", "X"),
      "(?:FJ|AY)\\d+|[A-Z0-9]+X[A-Z0-9]+"
    ) %>% str_trim()
  )
  
  merged_meta <- pat_lookup %>%
    left_join(mapping_keys, by = "LabKey") %>%
    filter(!is.na(`study number`)) %>%
    group_by(`study number`) %>%
    dplyr::slice(1) %>%  # explicitly use dplyr’s slice()
    ungroup()
  
  keep     <- merged_meta$OriginalName
  sub_mat  <- pat_df[keep, keep, drop = FALSE]
  studymap <- setNames(merged_meta$`study number`, merged_meta$OriginalName)
  rownames(sub_mat) <- studymap[rownames(sub_mat)]
  colnames(sub_mat) <- studymap[colnames(sub_mat)]
  
  as.data.frame(sub_mat)
}
