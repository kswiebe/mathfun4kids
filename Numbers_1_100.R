# install.packages("ggplot2")   # if needed

library(ggplot2)

library(grid)   # for unit()

# ---------------- CONFIG ---------------- #


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
df_temp = df
numbers_df_temp = numbers_df

tens_palette_select = tens_palette
pictureheight = 35
outfile <- "Numbers_1_100_poster.jpg"
source("Numbers_1_100_plot.R")

df_1_50 = df_temp[which(df_temp$number <= 50),]
numbers_df_1_50 = numbers_df_temp[which(numbers_df_temp$number <= 50),]
df_51_100 = df_temp[which(df_temp$number > 50),]
numbers_df_51_100 = numbers_df_temp[which(numbers_df_temp$number > 50),]

df = df_1_50
numbers_df = numbers_df_1_50
tens_palette_select = tens_palette[1:5]
pictureheight = 17
outfile <- "Numbers_1_50_poster.jpg"
source("Numbers_1_100_plot.R")


df = df_51_100
numbers_df = numbers_df_51_100
tens_palette_select = tens_palette[6:10]
pictureheight = 17
outfile <- "Numbers_51_100_poster.jpg"
source("Numbers_1_100_plot.R")

