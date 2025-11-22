p <- ggplot(df, aes(x = dot_col, y = dot_row)) +
  
  # dots
  geom_point(
    aes(fill = fill_color),
    shape = 21, size = 1.8,
    colour = "grey20", 
    #colour = "black", 
    stroke = 0.3 
  ) +
  
  # number label in each facet
  geom_text(
    data = numbers_df,
    aes(x = x, y = y, label = number),
    inherit.aes = FALSE,
    color = rep(tens_palette_select,each=10),
    fontface="bold",
    size = 5
  ) +
  
  scale_fill_identity() +
  
  scale_x_continuous(limits = c(-0.5, 9.5), breaks = NULL, expand = c(0, 0)) +
  
  scale_y_continuous(limits = c(-0.5, 11),  breaks = NULL, expand = c(0, 0)) +
  
  facet_wrap(~ number, ncol = 10) +
  
  coord_equal() +
  
  theme_void() +
  
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    #plot.background = element_rect(fill = "lightgrey"),
    # strip.background = element_rect(fill = rep(tens_palette_transparent,10)),
    plot.margin = margin(10, 10, 10, 10)
  )

#p
# -------------- SAVE -------------- #

ggsave(outfile, p, width = 30, height = pictureheight, units = "cm", dpi = 300)

cat("Saved poster to", outfile, "\n")