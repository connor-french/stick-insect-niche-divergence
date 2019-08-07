require(ggplot2)
require(dplyr)
#df needs to be a dataframe that contains your Marginality axis and Specialization axes of interest ("li" from enfa output)
#it also needs a presence-absence vector ("pr" from enfa output)

enfa_hex_plot <- function(df, marg, spec, repro_mode) {
  
  ggplot() + 
    geom_hex(data = df %>% filter(pr < 1), aes(x = {{ marg }}, y = {{ spec }})) +
    scale_fill_gradient(low = "lightgray", high = "black") +
    geom_hex(data = df %>% filter(pr > 0), aes(x = {{ marg }}, y ={{ spec }}), fill = "red") +
    geom_point(data = df %>% filter (pr > 0) %>% dplyr::summarize({{ marg }} := mean({{ marg }}), {{spec}} := mean({{ spec }})), 
               aes(x = {{ marg }}, y = {{ spec }}), size = 4, color = "gold") +
    geom_vline(xintercept = 0, alpha = 0.5) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    labs(title = repro_mode) +
    theme_minimal()
}


