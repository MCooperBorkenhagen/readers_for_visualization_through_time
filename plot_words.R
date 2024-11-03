dwell_at_closing = 30

# ea
# also see IDA2024/ for additional plots of specific words

ea = words %>% 
  filter(str_detect(word, "ea"))

targets = tibble(word = c("beat", "seat", "eat", "beat", "heat", "great", "threat"),
                 condition = c(1, 1, 1, 1, 1, 2, 3)) 


# 100 hidden units
max_PC1 = max(pcs_100_hidden_units$PC1) + 1
max_PC2 = max(pcs_100_hidden_units$PC2) + 1
max_PC3 = max(pcs_100_hidden_units$PC3) + 1
min_PC1 = min(pcs_100_hidden_units$PC1) - 1
min_PC2 = min(pcs_100_hidden_units$PC2) - 1
min_PC3 = min(pcs_100_hidden_units$PC3) - 1
# plot all points (no labels)

for (e in sort(unique(pcs_100_hidden_units$epoch))){
  if (e > 0){
    
    this_epoch = pcs_100_hidden_units %>% 
      filter(epoch == e)
    
    performance_this_epoch = performance_100_hidden_units %>% 
      filter(epoch <= e) %>% 
      filter(epoch != 0)
    
    plot = this_epoch %>% 
      ggplot(aes(PC1, PC2)) +
      geom_point(size = .05) +
      ylim(c(min_PC2, max_PC2)) +
      xlim(c(min_PC1, max_PC1)) +
      labs(x = "Dimension 1",
           y = "Dimension 2") +
      theme_apa() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) 
    
    plot_2 = performance_this_epoch %>% 
      ggplot(aes(epoch, mse)) +
      geom_line() +
      geom_point() +
      labs(x = "Learning time", y = "Error") +
      theme_apa() +
      ylim(c(0, max(performance_this_epoch$mse))) +
      xlim(c(1, max(performance_100_hidden_units$epoch)))
    
    plots = plot_grid(plot, plot_2)
    
    filename = str_c("outputs/100_hidden_units/combined/plot_", e, ".png")
    ggsave(filename, plot = plots, width = 12, height = 4, dpi = 300)
    print(paste("Epoch", e, "done"))
    
    if (e == max((pcs_100_hidden_units$epoch))){
      
      for (i in seq(dwell_at_closing)){
        
        filename = str_c("outputs/100_hidden_units/combined/plot_", e + i, ".png")
        ggsave(filename, plot = plots, width = 12, height = 4, dpi = 300)
        print(paste("Extra plot", i, "done"))
      }
}}}



# ea
for (e in sort(unique(pcs_100_hidden_units$epoch))){
  if (e > 0){
    
    this_epoch = pcs_100_hidden_units %>% 
      filter(epoch == e) %>% 
      right_join(targets)
    
    plot = this_epoch %>% 
      ggplot(aes(PC1, PC2, label = word, fill = factor(condition))) +
      geom_label(size = 2, alpha = .8) +
      ylim(c(min_PC2, max_PC2)) +
      xlim(c(min_PC1, max_PC1)) +
      labs(x = "Dimension 1",
           y = "Dimension 2") +
      theme_apa() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      scale_fill_manual(values = c("deeppink3", "mediumslateblue", "peru"))
    
    filename = str_c("outputs/100_hidden_units/ea/plot_", e, ".png")
    ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
    print(paste("Epoch", e, "done"))
    
    if (e == max((pcs_100_hidden_units$epoch))){
      
      for (i in seq(dwell_at_closing)){
        
        filename = str_c("outputs/100_hidden_units/ea/plot_", e + i, ".png")
        ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
        print(paste("Extra plot", i, "done"))
      }
    }}}







# 5 hidden units
max_PC1 = max(pcs_5_hidden_units$PC1) + 1
max_PC2 = max(pcs_5_hidden_units$PC2) + 1
max_PC3 = max(pcs_5_hidden_units$PC3) + 1
min_PC1 = min(pcs_5_hidden_units$PC1) - 1
min_PC2 = min(pcs_5_hidden_units$PC2) - 1
min_PC3 = min(pcs_5_hidden_units$PC3) + 1
# plot all points (no labels)

for (e in sort(unique(pcs_5_hidden_units$epoch))){
  if (e > 0){
    
    this_epoch = pcs_5_hidden_units %>% 
      filter(epoch == e)
    
    performance_this_epoch = performance_5_hidden_units %>% 
      filter(epoch <= e) %>% 
      filter(epoch != 0)
    
    plot = this_epoch %>% 
      ggplot(aes(PC1, PC2)) +
      geom_point(size = .05) +
      ylim(c(min_PC2, max_PC2)) +
      xlim(c(min_PC1, max_PC1)) +
      labs(x = "Dimension 1",
           y = "Dimension 2") +
      theme_apa() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) 
    
    plot_2 = performance_this_epoch %>% 
      ggplot(aes(epoch, mse)) +
      geom_line() +
      geom_point() +
      labs(x = "Learning time", y = "Error") +
      theme_apa() +
      ylim(c(0, max(performance_this_epoch$mse))) +
      xlim(c(1, max(performance_5_hidden_units$epoch)))
    
    plots = plot_grid(plot, plot_2)
    
    filename = str_c("outputs/5_hidden_units/combined/plot_", e, ".png")
    ggsave(filename, plot = plots, width = 12, height = 4, dpi = 300)
    print(paste("Epoch", e, "done"))
    
    if (e == max((pcs_5_hidden_units$epoch))){
      
      for (i in seq(dwell_at_closing)){
        
        filename = str_c("outputs/5_hidden_units/combined/plot_", e + i, ".png")
        ggsave(filename, plot = plots, width = 12, height = 4, dpi = 300)
        print(paste("Extra plot", i, "done"))
      }
    }}}

# EA orthographic pattern

# ea
for (e in sort(unique(pcs_5_hidden_units$epoch))){
  if (e > 0){
    
    this_epoch = pcs_5_hidden_units %>% 
      filter(epoch == e) %>% 
      right_join(targets)
    
    plot = this_epoch %>% 
      ggplot(aes(PC2, PC3, label = word, fill = factor(condition))) +
      geom_label(size = 2, alpha = .8) +
      ylim(c(min_PC3, max_PC3)) +
      xlim(c(min_PC2, max_PC2)) +
      labs(x = "Dimension 1",
           y = "Dimension 2") +
      theme_apa() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      scale_fill_manual(values = c("deeppink3", "mediumslateblue", "peru"))
    
    filename = str_c("outputs/5_hidden_units/ea/plot_", e, ".png")
    ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
    print(paste("Epoch", e, "done"))
    
    if (e == max((pcs_5_hidden_units$epoch))){
      
      for (i in seq(dwell_at_closing)){
        
        filename = str_c("outputs/5_hidden_units/ea/plot_", e + i, ".png")
        ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
        print(paste("Extra plot", i, "done"))
      }
    }}}
