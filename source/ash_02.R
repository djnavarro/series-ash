
# load packages -----------------------------------------------------------

library(here)
library(ggplot2)
library(tibble)
library(stringr)
library(dplyr)
cpp11::cpp_source(here("source", "ash_02.cpp"))

save_path <- function(sys_id, sys_version, seed, fmt = ".png") {
  sys_version <- sys_version %>% str_pad(width = 2, pad = "0")
  seed <- seed %>% str_pad(width = 3, pad = "0")
  base <- paste(sys_id, sys_version, seed, sep = "_")
  file <- paste0(base, fmt)
  path <- here("image", file)
  return(path)
}

# plot parameters ---------------------------------------------------------

for(seed in 1:10) {

  sys_id <- "ash"
  sys_version <- 2

  bg <- "grey60"


  # generate image ----------------------------------------------------------

  set.seed(seed)
  cat("making", seed, "\n")

  rep_deform <- function(poly, n, stdev) {
    for(i in 1:n) {
      poly <- deform(poly[, 1], poly[, 2], stdev)
    }
    return(poly)
  }

  make_base_shape <- function() {
    angle <- seq(from = 0, to = 2*pi, length.out = 4)
    base <- tibble(x = cos(angle), y = sin(angle)) %>%
      as.matrix() %>%
      rep_deform(n = 7, stdev = .4)
    return(base)
  }

  make_blurred_shape <- function(base) {
    dat <- list()
    for(i in 1:50) {
      poly <- rep_deform(base, 7, .5)
      dat[[i]] <- tibble(x = poly[,1], y = poly[,2], id = i) %>%
        mutate(pos = row_number())
    }
    dat <- bind_rows(dat)
    return(dat)
  }

  obj1 <- make_base_shape() %>%
    make_blurred_shape() %>%
    mutate(x = x - .1) %>%
    mutate(shade = "black")
  obj2 <- make_base_shape() %>%
    make_blurred_shape() %>%
    mutate(x = x + .6, x = x * 2, y = y * 2) %>%
    mutate(shade = "white")
  obj3 <- make_base_shape() %>%
    make_blurred_shape() %>%
    mutate(y = y - .3, x = x * 1.5, y = y * 1.5) %>%
    mutate(shade = "grey30")
  dat <- bind_rows(obj1, obj2, obj3) %>%
    mutate(
      group = id +
        if_else(shade == "black", 45.5, 0) +
        if_else(shade == "grey30", 15.3, 0)
    )

  border <- 1.3
  pic <- ggplot(dat, aes(x, y, group = factor(group), fill = shade)) +
    geom_polygon(alpha = .03) +
    coord_equal(
      xlim = c(-1, 1) * border,
      ylim = c(-1, 1) * border
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = bg, colour = bg)) +
    scale_fill_identity()

  ggsave(
    filename = save_path(sys_id, sys_version, seed),
    bg = bg,
    plot = pic,
    width = 10,
    height = 10,
    dpi = 300
  )
}

