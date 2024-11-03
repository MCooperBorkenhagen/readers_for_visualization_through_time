



for (e in sort(unique(pcs$epoch))){
  
  this_epoch = pcs %>% 
    filter(epoch == e) %>%
    filter(word %in% targets) %>% 
    select(word, epoch, PC1, PC2)
  
  plot = this_epoch %>% 
    ggplot(aes(PC1, PC2, label = word)) +
    geom_label(size = 2) +
    ylim(c(-6, 6)) +
    xlim(c(-6, 6)) +
    labs(x = "Dimension 1",
         y = "Dimension 2") +
    theme_apa()
  
  filename = str_c("outputs/img1/plot_", e, ".png")
  
  print(paste(e, "done"))
  ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
  
}
