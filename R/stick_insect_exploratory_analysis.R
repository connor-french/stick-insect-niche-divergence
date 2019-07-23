###Exploring the stick insect data
library(tidyverse)
library(readxl)
library(tidylog)
library(janitor)
library(rnaturalearth)
library(leaflet)

stick_insects <- read_xlsx("/Users/connorfrench/Dropbox/Old_Mac/Science/Stick-Insect-Project/Original-spreadsheets/all species New_6-14-19.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(reproductive_mode = as.factor(reproductive_mode)) 


###Some quick stats about the dataset
#check for duplicates
janitor::get_dupes(stick_insects)

#make sure sexual and asexual are the only factors
unique(stick_insects$reproductive_mode)


#get the number of individuals, and the sexuality counts per species
stick_insects %>% 
  group_by(genus, species, reproductive_mode) %>% 
  count() %>% 
  mutate(genus_species = str_c(genus, species, sep = "_"),
         genus_species = str_replace_all(genus_species, " ", "_"),
         genus_species = str_replace_all(genus_species, "\\.", "")) %>% 
  ungroup() %>% 
  mutate(genus_species = fct_reorder(genus_species, n, sum)) %>% 
  ggplot(aes(x = genus_species, y = n, fill = reproductive_mode)) +
  geom_col() +
  coord_flip() + 
  theme_minimal()
  
#make a map
nz <- ne_countries(country = "new zealand", returnclass = "sf")

#quick plot
ggplot(data = nz) +
  geom_sf(fill = "white") +
  geom_point(data = stick_insects, aes(x = longitude, y = latitude, fill = reproductive_mode), shape = 21) +
  theme_minimal()
  
  
leaflet(stick_insects)


