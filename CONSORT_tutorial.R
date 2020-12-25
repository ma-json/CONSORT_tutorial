
# A tutorial on dynamically drawing CONSORT diagrams from data using R and ggplot2

# Code and tutorial written by Matthew Johnson

# Tutorial text can be found at: 

# Inspired by an RPubs blog article by Peter Higgins: https://rpubs.com/phiggins/461686


#------------------------------------------------------------------------------
# Create a sample dataset
#------------------------------------------------------------------------------
id <- seq(1:100)

lyr_2 <- rep(2:3, 50)

lyr_3 <- rep(1:4, 25)

exclusion_lyr_4 <- c(rep(0, 40), rep(1, 20), rep(2, 20), rep(3, 20))

lyr_5 <- c(rep(1:4, 10), rep(0, 60))

df <- as.data.frame(cbind(id, lyr_2, lyr_3, exclusion_lyr_4, lyr_5))

rm(id, lyr_2, lyr_3, exclusion_lyr_4, lyr_5)


#------------------------------------------------------------------------------
# Set up the plot
#------------------------------------------------------------------------------
col_nr <- as.integer(4)

col_width <- as.integer(28)

col_spc <- as.integer(5)

lyr_nr <- as.integer(5)

lyr_depth <- as.integer(13)

lyr_spc <- as.integer(8)

plot_width <- as.integer((col_nr * col_width) + ((col_nr + 1) * col_spc))

plot_height <- as.integer((lyr_nr * lyr_depth) + ((lyr_nr + 1) * lyr_spc))

plot_area <- as.data.frame(matrix(ncol = 2, nrow = 2, dimnames = list(NULL, c("x", "y"))))
plot_area$x <- c(1, plot_width)
plot_area$y <- c(1, plot_height)


#------------------------------------------------------------------------------
# Set up the grid
#------------------------------------------------------------------------------
matr_col_dim <- matrix(
  c(
    list(c(NA, NA, NA)), 
    list(c(NA, NA, NA)), 
    list(c(NA, NA, NA)), 
    list(c(NA, NA, NA))
  ), 
  nrow = 1, 
  ncol = col_nr
)

for(i in col_nr:1) {
  {
    if(i == col_nr)
      matr_col_dim[[1, i]][3] = plot_width - col_spc
    else
      matr_col_dim[[1, i]][3] = matr_col_dim[[1, i + 1]][1] - col_spc
  }
  matr_col_dim[[1, i]][2] = matr_col_dim[[1, i]][3] - (col_width / 2)
  matr_col_dim[[1, i]][1] = matr_col_dim[[1, i]][3] - col_width
}

rm(i)

matr_lyr_dim <- matrix(
  c(
    list(c(NA, NA, NA)), 
    list(c(NA, NA, NA)), 
    list(c(NA, NA, NA)),
    list(c(NA, NA, NA)),
    list(c(NA, NA, NA))
  ), 
  nrow = lyr_nr, 
  ncol = 1
)

for(i in 1:lyr_nr) {
  {
    if(i == 1)
      matr_lyr_dim[[i, 1]][3] = plot_height - lyr_spc
    else
      matr_lyr_dim[[i, 1]][3] = matr_lyr_dim[[i - 1, 1]][1] - lyr_spc
  }
  matr_lyr_dim[[i, 1]][2] = matr_lyr_dim[[i, 1]][3] - (lyr_depth / 2)
  matr_lyr_dim[[i, 1]][1] = matr_lyr_dim[[i, 1]][3] - lyr_depth
}

rm(i)


#------------------------------------------------------------------------------
# Create more lookup tables
#------------------------------------------------------------------------------
matr_cell_text <- matrix(
  c(
    NA, "All observations\nN=", NA, NA, # layer 1
    NA, "Subgroup\nN=", "Subgroup\nN=", NA, # layer 2
    "Subgroup\nN=", "Subgroup\nN=", "Subgroup\nN=", "Subgroup\nN=", # layer 3
    NA, NA, NA, NA, # layer 4
    "Subgroup after exclusions\nN=", "Subgroup after exclusions\nN=", "Subgroup after exclusions\nN=", "Subgroup after exclusions\nN=" # layer 5
  ), 
  nrow = lyr_nr, 
  byrow = TRUE,
  dimnames = list(
    c(seq(1, lyr_nr)), 
    c(seq(1, col_nr)) 
  )
)

matr_cell_values <- matrix(
  c(
    NA, length(df$id), NA, NA, # layer 1
    NA, sum(df$lyr_2 == 2), sum(df$lyr_2 == 3), NA, # layer 2
    sum(df$lyr_3 == 1), sum(df$lyr_3 == 2), sum(df$lyr_3 == 3), sum(df$lyr_3 == 4), # layer 3
    NA, NA, NA, NA, # layer 4
    sum(df$lyr_5 == 1), sum(df$lyr_5 == 2), sum(df$lyr_5 == 3), sum(df$lyr_5 == 4) # layer 5
  ), 
  nrow = lyr_nr, 
  byrow = TRUE,
  dimnames = list(
    c(seq(1, lyr_nr)), 
    c(seq(1, col_nr)) 
  )
)

matr_lyr_4_content <- matrix(
  c(
    sum(df$lyr_3 == 1 & df$exclusion_lyr_4 == 1), sum(df$lyr_3 == 1 & df$exclusion_lyr_4 == 2), sum(df$lyr_3 == 1 & df$exclusion_lyr_4 == 3), # column 1
    sum(df$lyr_3 == 2 & df$exclusion_lyr_4 == 1), sum(df$lyr_3 == 2 & df$exclusion_lyr_4 == 2), sum(df$lyr_3 == 2 & df$exclusion_lyr_4 == 3), # column 2
    sum(df$lyr_3 == 3 & df$exclusion_lyr_4 == 1), sum(df$lyr_3 == 3 & df$exclusion_lyr_4 == 2), sum(df$lyr_3 == 3 & df$exclusion_lyr_4 == 3), # column 3
    sum(df$lyr_3 == 4 & df$exclusion_lyr_4 == 1), sum(df$lyr_3 == 4 & df$exclusion_lyr_4 == 2), sum(df$lyr_3 == 4 & df$exclusion_lyr_4 == 3) # column 4
  ), 
  ncol = col_nr, 
  byrow = FALSE,
  dimnames = list(
    c("First exclusion, ", "Second exclusion, ", "Third exclusion, "), 
    c(seq(1, 4)) 
  )
)


#------------------------------------------------------------------------------
# Define draw functions
#------------------------------------------------------------------------------
# (a) closed cell
geom_closed_cell <- function(
  lyr, 
  col, 
  arrow_x_start = matr_col_dim[[1, col]][2], 
  arrow_x_end = arrow_x_start, 
  arrow_y_start = matr_lyr_dim[[lyr - 1, 1]][1] 
) {
  
  assign("lyr_f", lyr, envir = .GlobalEnv)
  assign("col_f", col, envir = .GlobalEnv)
  
  list(
    geom_rect(
      xmin = matr_col_dim[[1, col]][1], 
      xmax = matr_col_dim[[1, col]][3], 
      ymin = matr_lyr_dim[[lyr, 1]][1], 
      ymax = matr_lyr_dim[[lyr, 1]][3], 
      color = 'black', 
      fill = 'azure2', 
      size = 0.25
    ), 
    geom_text(
      x = matr_col_dim[[1, col]][2], 
      y = matr_lyr_dim[[lyr, 1]][2], 
      label = paste0(
        matr_cell_text[lyr, col], 
        matr_cell_values[lyr, col]
      ), 
      size = 2.5, 
      fontface = 'bold'
    ), 
    geom_segment(
      x = arrow_x_start, 
      xend = arrow_x_end, 
      y = arrow_y_start, 
      yend = matr_lyr_dim[[lyr, 1]][3], 
      size = 0.15, 
      linejoin = "mitre", 
      lineend = "butt",
      arrow = arrow(length = unit(1.5, "mm"), type = "closed") 
    )
  )
}

# (b) open cell
geom_open_cell <- function(
  lyr, 
  col, 
  txt_x, 
  txt_align, 
  arrow_x_start = matr_col_dim[[1, col]][2] + (col_spc * 2), 
  arrow_x_end = matr_col_dim[[1, col]][2] + (col_spc * 1.2) 
) {
  
  assign("lyr_f", lyr, envir = .GlobalEnv)
  assign("col_f", col, envir = .GlobalEnv)
  
  list(
    geom_text(
      x = txt_x, 
      y = matr_lyr_dim[[lyr, 1]][2], 
      label = paste0(
        rownames(matr_lyr_4_content)[1],
        matr_lyr_4_content[1, col], 
        "\n",
        rownames(matr_lyr_4_content)[2],
        matr_lyr_4_content[2, col], 
        "\n",
        rownames(matr_lyr_4_content)[3],
        matr_lyr_4_content[3, col]
      ), 
      size = 2.5,
      hjust = txt_align
    ), 
    geom_segment(
      x = arrow_x_start, 
      xend = arrow_x_end, 
      y = matr_lyr_dim[[lyr, 1]][2], 
      yend = matr_lyr_dim[[lyr, 1]][2], 
      size = 0.15, 
      linejoin = "mitre", 
      lineend = "butt",
      arrow = arrow(length = unit(1.5, "mm"), type = "closed") 
    )
  )
}


#------------------------------------------------------------------------------
# Draw the final CONSORT diagram
#------------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

CONSORT_diag <- ggplot(plot_area, aes(x, y)) + 
  scale_x_continuous(
    breaks = c(unlist(matr_col_dim), plot_width), 
    labels = c(unlist(matr_col_dim), plot_width), 
    limits = c(0, plot_width), 
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(unlist(matr_lyr_dim), plot_height), 
    labels = c(unlist(matr_lyr_dim), plot_height), 
    limits = c(0, plot_height), 
    expand = c(0, 0)
  ) +
  
  # layer 1: 
  geom_rect(
    xmin = matr_col_dim[[1, 2]][2] + (col_spc / 2), 
    xmax = matr_col_dim[[1, 3]][2] - (col_spc / 2), 
    ymin = matr_lyr_dim[[1, 1]][1], 
    ymax = matr_lyr_dim[[1, 1]][3], 
    color = 'black', 
    fill = 'azure2', 
    size = 0.25
  ) +
  geom_text(
    x = matr_col_dim[[1, 2]][3] + (col_spc / 2), 
    y = matr_lyr_dim[[1, 1]][2], 
    label = paste0(
      matr_cell_text[1, 2], 
      matr_cell_values[1, 2]
    ), 
    size = 2.5, 
    fontface = 'bold'
  ) +
  
  # layer 2:
  geom_closed_cell(
    lyr = 2, 
    col = 2, 
    arrow_x_start = matr_col_dim[[1, col_f]][3] + (col_spc / 2), 
    arrow_x_end = matr_col_dim[[1, col_f]][2]
  ) +
  geom_closed_cell(
    lyr = 2, 
    col = 3, 
    arrow_x_start = matr_col_dim[[1, (col_f - 1)]][3] + (col_spc / 2), 
    arrow_x_end = matr_col_dim[[1, col_f]][2]
  ) +
  
  # layer 3:
  geom_closed_cell(
    lyr = 3, 
    col = 1, 
    arrow_x_start = matr_col_dim[[1, (col_f + 1)]][2], 
    arrow_x_end = matr_col_dim[[1, col_f]][2]
  ) + 
  geom_closed_cell(
    lyr = 3, 
    col = 2
  ) +
  geom_closed_cell(
    lyr = 3, 
    col = 3
  ) +
  geom_closed_cell(
    lyr = 3, 
    col = 4, 
    arrow_x_start = matr_col_dim[[1, (col_f - 1)]][2], 
    arrow_x_end = matr_col_dim[[1, col_f]][2]
  ) + 
  
  # layer 4: 
  geom_open_cell(
    lyr = 4, 
    col = 1, 
    txt_x = matr_col_dim[[1, col_f]][2] + col_spc,
    txt_align = 'right'
  ) +
  geom_open_cell(
    lyr = 4, 
    col = 2, 
    txt_x = matr_col_dim[[1, col_f]][2] + col_spc,
    txt_align = 'right'
  ) +
  geom_open_cell(
    lyr = 4, 
    col = 3, 
    txt_x = matr_col_dim[[1, col_f]][2] - col_spc, 
    txt_align = 'left', 
    arrow_x_start = matr_col_dim[[1, col_f]][2] - (col_spc * 2), 
    arrow_x_end = matr_col_dim[[1, col_f]][2] - (col_spc * 1.2)
  ) +
  geom_open_cell(
    lyr = 4, 
    col = 4, 
    txt_x = matr_col_dim[[1, col_f]][2] - col_spc, 
    txt_align = 'left', 
    arrow_x_start = matr_col_dim[[1, col_f]][2] - (col_spc * 2), 
    arrow_x_end = matr_col_dim[[1, col_f]][2] - (col_spc * 1.2)
  ) + 
  
  # layer 5:
  geom_closed_cell(
    lyr = 5, 
    col = 1, 
    arrow_x_start = matr_col_dim[[1, col_f]][2] + (col_spc * 2), 
    arrow_y_start = matr_lyr_dim[[(lyr_f - 2), 1]][1]
  ) +
  geom_closed_cell(
    lyr = 5, 
    col = 2, 
    arrow_x_start = matr_col_dim[[1, col_f]][2] + (col_spc * 2), 
    arrow_y_start = matr_lyr_dim[[(lyr_f - 2), 1]][1]
  ) +
  geom_closed_cell(
    lyr = 5, 
    col = 3, 
    arrow_x_start = matr_col_dim[[1, col_f]][2] - (col_spc * 2), 
    arrow_y_start = matr_lyr_dim[[(lyr_f - 2), 1]][1]
  ) +
  geom_closed_cell(
    lyr = 5, 
    col = 4, 
    arrow_x_start = matr_col_dim[[1, col_f]][2] - (col_spc * 2), 
    arrow_y_start = matr_lyr_dim[[(lyr_f - 2), 1]][1]
  ) +
  
  theme_void()

CONSORT_diag

