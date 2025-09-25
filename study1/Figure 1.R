# Load necessary libraries
library(tidyverse)   
library(ggnewscale)  
library(ggpubr)

# mm1 needs to be defined (study_1.R)

#'Do means matter morally?' Study 1 - Figure 1

fig1a = ggplot(mm1, aes(x = response, group = Methods, color = Methods))+
  geom_histogram(alpha = .5, aes(y = ..density..), fill = 'transparent') + 
  geom_density(linewidth = 1, aes(fill = Methods), alpha = .3) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = c(.2, .8), 
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

fig1c = ggplot(data = mm1, aes(x = response))+
  geom_rect(data = endorse, aes(fill = Endorsement), alpha = .2, xmin = 0, xmax = 100, ymin = 0, ymax = 0.75) +
  scale_fill_manual(values = c("firebrick", "darkgrey")) +
  new_scale_fill() +   # reset fill scale
  geom_histogram(data = mm1, aes(x = response, group = Methods, color = Methods, y = ..density..), alpha = .5, fill = 'transparent') + 
  geom_density(data = mm1, aes(x = response, group = Methods, color = Methods, fill = Methods), linewidth = 1, alpha = .3) + 
  theme_minimal() + facet_grid(Endorsement ~ .) +
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = 'none', 
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

endorse = data.frame(Endorsement = c('Equivalence', 'Difference'),
                     Proportion = c(140/334, 194/334),
                     response = 50)

fig1b = ggplot(endorse, aes(fill = Endorsement, y = Proportion, x = 0)) + 
  geom_hline(yintercept = .5, linetype = 1) +
  geom_col(position = 'stack', alpha = .5) + coord_flip() +
  annotate("rect", ymin = 0.3660039, ymax = 0.4742095, xmin = -Inf, xmax = Inf,
           alpha = .15, fill = "orange") +
  geom_hline(yintercept = 0.4191617, linetype = 2, color = "orange", linewidth = 1) +
  theme_minimal() + 
  scale_color_manual(values = c( 'firebrick', 'grey')) + 
  scale_fill_manual(values = c('firebrick', 'grey')) + 
  scale_x_continuous(name = NULL, breaks = NULL, limits = c(-.45, .45)) +
  scale_y_continuous(name = 'Proportion (Equivalence)', 
                     breaks = seq(0, 1, .2), 
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%'), 
                     sec.axis = dup_axis(name = 'Proportion (Difference)', 
                                         breaks = seq(0, 1, .2), 
                                         labels = c('100%', '80%', '60%', '40%', '20%', '0%'))) + 
  theme(legend.position = 'none',
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

ggarrange(fig1a, ggarrange(fig1b, fig1c, nrow = 2, heights = c(1, 2), 
                           labels = c('B', 'C')), 
          labels = c('A', NULL))

ggsave('Figures/Figure1.jpg', dpi = 300, width = 16, height = 12, units = 'cm')
