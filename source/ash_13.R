
# load packages -----------------------------------------------------------

library(here)
library(ggplot2)
library(tibble)
library(stringr)
library(dplyr)
cpp11::cpp_source(here("source", "ash_13.cpp"))

save_path <- function(sys_id, sys_version, seed, fmt = ".png") {
  sys_version <- sys_version %>% str_pad(width = 2, pad = "0")
  seed <- seed %>% str_pad(width = 3, pad = "0")
  base <- paste(sys_id, sys_version, seed, sep = "_")
  file <- paste0(base, fmt)
  path <- here("image", file)
  return(path)
}

# plot parameters ---------------------------------------------------------

for(seed in 8) {

  sys_id <- "ash"
  sys_version <- 13

  bg <- "grey10"


  # generate image ----------------------------------------------------------

  set.seed(seed)
  cat("making", seed, "\n")

  rep_deform <- function(poly, n, stdev) {
    for(i in 1:n) {
      poly <- deform(poly[, 1], poly[, 2], stdev)
      stdev <- stdev * .8
    }
    return(poly)
  }

  make_heart <- function(n = 10) {
    angle <- seq(from = 0, to = 2*pi, length.out = n)
    base <- tibble(
      x = (16 * sin(angle)^3)/17,
      y = (13 * cos(angle) - 5 * cos(2 * angle) - 2 * cos(3 * angle) - cos(4 * angle))/17
    ) %>%
      mutate(
        x = x - mean(x),
        y = y - mean(y)
      ) %>%
      as.matrix()
    return(base)
  }

  make_circle <- function(n = 10) {
    angle <- seq(from = 0, to = 2*pi, length.out = n)
    base <- tibble(
      x = cos(angle),
      y = sin(angle)
    ) %>%
      mutate(
        x = x - mean(x),
        y = y - mean(y)
      ) %>%
      as.matrix()
    return(base)
  }


  make_base_shape <- function(shape = make_circle(), recurse = 5, stdev = 1) {
    shape  %>% rep_deform(n = recurse, stdev = stdev)
  }

  make_blurred_shape <- function(base, layers) {
    dat <- list()
    for(i in 1:layers) {
      poly <- rep_deform(base, 4, .5)
      dat[[i]] <- tibble(x = poly[,1], y = poly[,2], id = i) %>%
        mutate(pos = row_number())
    }
    dat <- bind_rows(dat)
    return(dat)
  }

  make_object <- function(base, shade, layers = 50, scale = NULL, elevation = NULL) {
    xsh <- 0 #runif(1, -1 , 1)
    ysh <- 0 #runif(1, -1 , 1)
    if(is.null(scale)) scale <- runif(1, .9, 1.5)
    if(is.null(elevation)) elevation <- runif(1, 0, 1)

    base %>%
      make_blurred_shape(layers = layers) %>%
      mutate(
        x = (x + xsh) * scale,
        y = (y + ysh) * scale,
        group = id + elevation
      )  %>%
      mutate(shade = shade)
  }

  add_texture <- function(shade = bg, scale = 1) {
    make_base_shape() %>%
      make_object(
        shade = shade,
        scale = scale,
        layers = 30,
        elevation = -.1 + runif(1, 0, .1)
      ) %>%
      mutate(
        x = x + rnorm(1, 0, .5),
        y = y + rnorm(1, 0, .5)
      )
  }

  dat <- bind_rows(
    make_base_shape() %>%
      make_object(
        shade = "red",
        scale = 1.5,
        layers = 30,
        elevation = -.1
      ),
    make_base_shape() %>%
      make_object(
        shade = "orange",
        scale = 1,
        layers = 15,
        elevation = -.2
      ),
    make_circle(n = 6) %>%
      make_base_shape(recurse = 6, stdev = 1.5) %>%
      make_object(
        shade = "black",
        scale = 1.2,
        layers = 100,
        elevation = 50.1
      ),
    add_texture(),
    add_texture(),
    add_texture(),
    add_texture(),
    add_texture(),
    add_texture("red", scale = .8),
    add_texture("orange", scale = .8),
    add_texture("red", scale = .8)
  )

  border <- 1.3
  pic <- ggplot(dat, aes(x, y, group = factor(group), fill = shade)) +
    geom_polygon(alpha = .02) +
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

