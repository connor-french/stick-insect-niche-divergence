#calculate the centroid of a random sample of individuals. df = data frame, sp = string containing the species you are interested (e.g. "naomi"), len = the number of individuals to sample. Should be the number of individuals from the species that has fewer occurrences.
pc_centroid_fun <- function(df, sp, len) {
  pc_cent <- df %>%
    sample_n(len) %>%
    filter(species == sp) %>%
    select(PC1.1:PC3.1) %>%
    summarize_all(mean) %>% 
    rowMeans() %>%
    abs()
  return(pc_cent)
}

