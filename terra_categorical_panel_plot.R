library(terra)

r <- rast(nrows=5, ncols=9)
values(r) <- sample(3, ncell(r), replace = TRUE)
x <- c(r, r, r)

levels_df <- data.frame(id=1:3, type = c("river", "land", "ocean"))

levels(x) <- list(levels_df, levels_df, levels_df)

plot(x)

panel(x)