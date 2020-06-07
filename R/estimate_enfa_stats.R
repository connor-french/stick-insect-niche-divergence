### This script calculates matginality statistics and p-values for each species-reproductive mode pair

library(here)
library(adehabitatHS)
library(tidyverse)
library(furrr)
library(fs)

obj_path <- here("output", "objects")


# get all files to read in
dir_list_enfa <-
  dir_ls(obj_path, glob = "*.RDS",
         recurse = TRUE) 

dir_list_enfa <- dir_list_enfa[!str_detect(dir_list_enfa, "subset")]

# get reproductive mode
m_enfa <-
  map(dir_list_enfa, function(x)
    str_extract(x, "_*sexual")) %>% 
  unlist()


get_marg_stats <- function(x) {
  genus_sp_names <- str_split(x, "/") %>% unlist()
  genus_sp_names <- genus_sp_names[length(genus_sp_names) - 1]
  
  # read in the enfa
  enfa <- readRDS(x)
  
  # perform a permutation test
  enfa_sig <- randtest(enfa)
  
  sig_stats <- enfa_sig$expvar %>% 
    enframe(name = "stat", value = "value") %>%
    pivot_wider(names_from = stat, values_from = value)
  
  
  sig_table <- tibble(
    obs = enfa_sig$obs,
    alter = enfa_sig$alter,
    rep = enfa_sig$rep,
    pvalue = enfa_sig$pvalue
  ) %>%
    bind_cols(ahor_stats) %>%
    mutate(
      genus_sp = str_replace(genus_sp_names, "_", " ") %>% 
        str_to_sentence()
    )
  
  # remove large enfa from memory. idk if this does anything, but just as a safeguard
  rm(enfa)
  
  sig_table
}


marg_stats <- furrr::future_map(dir_list_enfa, get_marg_stats)

marg_stats_df <- bind_rows(marg_stats) %>%
  mutate(
    repro_mode = case_when(m_enfa == "sexual" ~ "asexual",
                           m_enfa == "_sexual" ~ "sexual") %>%
      str_to_sentence()
  )


marg_stats_df

write_csv(marg_stats_df, here("output", "spreadsheets", "marg_stats.csv"))