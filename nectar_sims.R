
library(tidyverse)

max_t = 1000
n_plants = 10      # total number plants
n_flowers = 5      # number flowers per plant
land_width = 1000   # width of landscape, in m
land_length = 1000  # length of landscape, in m
plant_rad = 0.75    # radius of flowering area on plant, in m
Y0 = 0.1            # starting proportion of flowers with yeast


total_flowers <- n_plants * n_flowers
# Creating xy coordinates for flowers and plants on landscape
plant_xy <- cbind(x = runif(n_plants, 0, land_width),
                  y = runif(n_plants, 0, land_length))
flower_xy <- cbind(x = numeric(total_flowers),
                   y = numeric(total_flowers))
for (i in 1:n_plants) {
    j <- (i - 1) * n_flowers + 1
    k <- i * n_flowers
    x <- plant_xy[i,"x"]
    y <- plant_xy[i,"y"]
    flower_xy[j:k,"x"] <- runif(n_flowers, x - plant_rad, x + plant_rad)
    flower_xy[j:k,"y"] <- runif(n_flowers, y - plant_rad, y + plant_rad)
}; rm(i, j, k, x, y)

replace_flower <- function(idx) {
    i <- (idx - 1) %/% n_flowers + 1
    p_x <- plant_xy[i,"x"]
    p_y <- plant_xy[i,"y"]
    x <- runif(1, p_x - plant_rad, p_x + plant_rad)
    y <- runif(1, p_y - plant_rad, p_y + plant_rad)
    return(c(x, y))
}

# Non-pollinator
d_mat <- matrix(0, total_flowers, total_flowers)




# Starting bacteria proportion
B0 <- 1 - Y0

# exponential dispersal model: P_disp(d) = exp(-d / theta)


Y <- numeric(max_t + 1)
B <- numeric(max_t + 1)



