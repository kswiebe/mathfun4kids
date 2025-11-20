# install.packages("ggplot2")   # if needed

library(ggplot2)

library(grid)   # for unit()

# ---------------- CONFIG ---------------- #

outfile <- "Numbers_1_100_poster.jpg"

# Colors for each tens row (1–10, 11–20, ..., 91–100)

tens_palette <- c(
  "#d80000", "#ff7f00", "#0078d4", "#00994c", "#a000a0",
  "#0000cc", "#e00088", "#ffb000", "#00aaaa", "#555555"
)

tens_palette <- c("red","blue","green","#ffb000","purple",
                  "violetred","cyan","green4","#ff7f00","magenta")

tens_palette_transparent = adjustcolor(tens_palette,alpha = 0.3)

numbers <- 1:100

# -------------- DATA BUILD -------------- #

df_list <- lapply(numbers, function(n) {
  
  dots <- 1:100
  
  data.frame(
    
    number = n,
    
    dot_id = dots,
    
    # 10×10 grid, left-to-right, top-to-bottom
    
    dot_row = 9 - ((dots - 1) %/% 10),   # 0..9 (top row is 9)
    
    dot_col = (dots - 1) %% 10,          # 0..9
    
    filled  = dots <= n
    
  )
  
})

df <- do.call(rbind, df_list)

df$tens_group <- floor((df$number - 1) / 10) + 1

df$fill_color <- ifelse(df$filled,
                        
                        tens_palette[df$tens_group],
                        
                        "white")

for(c in 1:10){
  id_range1 = (c-1)*10+1
  id_range2 = c*10
  df$fill_color[which(df$dot_id %in% c(id_range1:id_range2))] <- 
    ifelse(df$filled[which(df$dot_id %in% c(id_range1:id_range2))],
           
           tens_palette[c],
           
           "white")
}
# for drawing the number text in each facet

numbers_df <- data.frame(
  
  number = numbers,
  
  x = 4.5,      # horizontal center over the dots
  
  y = 10.4      # a bit above the top dot row
  
)

# -------------- PLOT -------------- #

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
    color = rep(tens_palette,each=10),
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

ggsave(outfile, p, width = 30, height = 40, units = "cm", dpi = 300)

cat("Saved poster to", outfile, "\n")
