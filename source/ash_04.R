
# load packages -----------------------------------------------------------

library(here)
library(ggplot2)
library(tibble)
library(stringr)
library(dplyr)
cpp11::cpp_source(here("source", "ash_04.cpp"))

save_path <- function(sys_id, sys_version, seed, fmt = ".png") {
  sys_version <- sys_version %>% str_pad(width = 2, pad = "0")
  seed <- seed %>% str_pad(width = 3, pad = "0")
  base <- paste(sys_id, sys_version, seed, sep = "_")
  file <- paste0(base, fmt)
  path <- here("image", file)
  return(path)
}

# plot parameters ---------------------------------------------------------

for(seed in 1) {

  sys_id <- "ash"
  sys_version <- 4

  bg <- "white"


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
      )
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
      )
    return(base)
  }


  make_base_shape <- function(n = 30, iter = 4, stdev = .5) {
    make_circle(n) %>%
      # mutate(id = row_number()) %>%
      # slice_sample(prop = .5) %>%
      # arrange(id) %>%
      # select(-id) %>%
      as.matrix() %>%
      rep_deform(n = iter, stdev = stdev)
  }

  make_blurred_shape <- function(base) {
    dat <- list()
    for(i in 1:50) {
      poly <- rep_deform(base, 4, .5)
      dat[[i]] <- tibble(x = poly[,1], y = poly[,2], id = i) %>%
        mutate(pos = row_number())
    }
    dat <- bind_rows(dat)
    return(dat)
  }

  make_object <- function(base, shade, idsh = NULL) {
    xsh <- 0 #runif(1, -1 , 1)
    sc <- runif(1, .9, 1.2)
    ysh <- 0 #runif(1, -1 , 1)
    if(is.null(idsh)) idsh <- runif(1, 0, 1)

    base %>%
      make_blurred_shape() %>%
      mutate(
        x = (x + xsh) * sc,
        y = (y + ysh) * sc,
        group = id + idsh
      )  %>%
      mutate(shade = shade)
  }

  dat <- bind_rows(
    #make_base_shape(iter = 4, stdev = 3) %>% make_object("black"),
    make_base_shape(iter = 4, stdev = .5) %>% make_object("blue")
  )

  border <- 1.3
  pic <- ggplot(dat, aes(x, y, group = factor(group), fill = shade)) +
    geom_polygon(alpha = .08) +
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

