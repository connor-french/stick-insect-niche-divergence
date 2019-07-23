#A lollipop plot to compare marginality scores between sexual and asexual populations for a species

#sex_marg = marginality score for sexual populations
#asex_marg = marginality score for asexual populations
#full_species_name = full genus and species name for the species being investigated
marginality_lollipop <- function(sex_marg, asex_marg, full_species_name) { 
  
  #make a tibble of the reproductive modes and their respective marginality scores
  marg_df <- tibble(reproductive_mode = c("Sexual", "Asexual"), 
                    marg = c(sex_marg, asex_marg)) %>% 
    mutate(reproductive_mode = fct_reorder(reproductive_mode, marg)) #order the scores by whichever is largest
  
  #plot. 
  marg_lollipop <- ggplot(data = marg_df, aes(x = reproductive_mode, y = marg)) +
    geom_segment(aes(x = reproductive_mode, xend = reproductive_mode, y = 0, yend = marg)) + 
    geom_point(col = "orange", size = 4) + 
    labs(x = "Reproductive mode", y = "Marginality", title = {{full_species_name}}) +
    coord_flip() + 
    theme_minimal()
  
  marg_lollipop
}
