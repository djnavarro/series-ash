
# load packages -----------------------------------------------------------

library(here)
library(ggplot2)
library(tibble)
library(stringr)
cpp11::cpp_source(here("source", "ash_01.cpp"))

save_path <- function(sys_id, sys_version, seed, fmt = ".png") {
  sys_version <- sys_version %>% str_pad(width = 2, pad = "0")
  seed <- seed %>% str_pad(width = 3, pad = "0")
  base <- paste(sys_id, sys_version, seed, sep = "_")
  file <- paste0(base, fmt)
  path <- here("image", file)
  return(path)
}

# plot parameters ---------------------------------------------------------

for(seed in 3:10) {

  sys_id <- "ash"
  sys_version <- 1

  bg <- "white"


  # generate image ----------------------------------------------------------

  set.seed(seed)

  angle <- seq(from = 0, to = 2*pi, length.out = 10)

  poly <- tibble(
    x = cos(angle),
    y = sin(angle)
  )
  poly <- as.matrix(poly)

  rep_deform <- function(poly, n, stdev) {
    for(i in 1:n) {
      poly <- deform(poly[, 1], poly[, 2], stdev)
    }
    return(poly)
  }

  base <- rep_deform(poly, 7, .4)

  dat <- list()
  for(i in 1:150) {
    poly <- rep_deform(base, 5, .6)
    dat[[i]] <- tibble(x = poly[,1], y = poly[,2], id = i)
  }
  dat <- dplyr::bind_rows(dat)

  border <- 1.3
  pic <- ggplot(dat, aes(x, y, group = id)) +
    geom_polygon(alpha = .02, fill = "black") +
    coord_equal(
      xlim = c(-1, 1) * border,
      ylim = c(-1, 1) * border
    ) +
    theme_void()

  ggsave(
    filename = save_path(sys_id, sys_version, seed),
    bg = "white",
    plot = pic,
    width = 10,
    height = 10,
    dpi = 300
  )
}

