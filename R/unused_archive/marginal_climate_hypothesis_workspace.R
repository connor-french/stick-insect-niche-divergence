### Marginal climate hypothesis workspace. Resample from sister species data set and calculate average per sample. Compare absolute value of range-restricted species to distribution of sister species averages. Need to work on a suitable statistical test. ----

#calculate the centroid of a random sample of individuals. df = data frame, sp = string containing the species you are interested (e.g. "naomi"), len = the number of individuals to sample. Should be the number of individuals from the species that has fewer occurrences.

#calculate the centroid from the range-restricted species. 
pc.centroid.nov <- summary.list.aste$loc.clim %>% 
  filter(species == "nov. sp.") %>%
  select(PC1.1:PC3.1) %>%
  summarize_all(mean) %>% 
  rowMeans() %>%
  abs()

#resample the more widespread species many times, calculating the centroid of three individuals (number of individuals in the range-restricted species)
pc.centroid.naomi <- replicate(1000, pc_centroid_fun(summary.list.aste$loc.clim, "naomi", 3), simplify = TRUE) %>% 
  as.data.frame()
names(pc.centroid.naomi) <- "centroid" #needs to be a data frame with a named column for ggplot

#sampling distribution of the centroids calculated from the generalist species. The red line indicates the centroid of the range-restricted species
ggplot(pc.centroid.naomi, aes(x = centroid)) +
  geom_histogram(bins = 30) +
  #scale_x_sqrt() +
  geom_vline(xintercept = pc.centroid.nov, color = "red") + 
  theme_linedraw()

