library(treeio)   # for treedata
library(ape)      # for drop.tip
library(dplyr)
library(stringr)

fc_tree_renamed <- function(td, tree_index = 1) {
  if (!inherits(td, "treedata")) {
    stop("Input must be a treeio `treedata` object.")
  }
  if (!exists("mapping_data_1")) {
    stop("`mapping_data_1` not found in your environment.")
  }
  
  mapping_keys <- mapping_data_1 %>%
    mutate(
      LabLogRaw = toupper(str_trim(`Laboratory log number`)),
      LabKey    = str_replace_all(LabLogRaw, "[-/]", "X")
    ) %>%
    bind_rows(
      mapping_data_1 %>%
        mutate(
          LabLogRaw = toupper(str_trim(`Laboratory log number`)),
          LabKey    = str_replace_all(LabLogRaw, "[-X]", "/")
        )
    ) %>%
    distinct(LabKey, .keep_all = TRUE)
  
  phy_slot <- td@phylo
  if (inherits(phy_slot, "phylo")) {
    phy <- phy_slot
  } else if (is.list(phy_slot) && all(sapply(phy_slot, inherits, "phylo"))) {
    if (tree_index > length(phy_slot) || tree_index < 1) {
      stop("`tree_index` out of bounds: treedata contains ", length(phy_slot), " trees.")
    }
    phy <- phy_slot[[tree_index]]
  } else {
    stop("Could not find a valid phylo in `td@phylo`.")
  }
  
  tip_map <- tibble(old = phy$tip.label) %>%
    mutate(
      NormID     = toupper(old) %>% str_replace_all("[-/]", "X"),
      Specimenno = str_trim(str_extract(
        NormID,
        "(?:FJ|AY)\\d+|[A-Z0-9]+X[A-Z0-9]+"
      ))
    ) %>%
    left_join(mapping_keys, by = c("Specimenno" = "LabKey")) %>%
    mutate(matched = !is.na(`study number`))
  
  keep   <- tip_map$old[ tip_map$matched ]
  new_lbl<- tip_map$`study number`[ tip_map$matched ]
  
  if (length(keep) == 0) {
    stop("None of the tip labels mapped to mapping_data_1$`Laboratory log number`.")
  }
  
  pruned_phy <- drop.tip(phy, setdiff(phy$tip.label, keep))
  
  pruned_phy$tip.label <- new_lbl[ match(pruned_phy$tip.label, keep) ]
  
  return(pruned_phy)
}
