
# load packages -----------------------------------------------------------

library(here)
library(ggplot2)
library(tibble)
library(stringr)
library(dplyr)
library(purrr)

sys_id <- "ash"
sys_version <- 10

cppfile <- paste0(sys_id, "_", str_pad(sys_version, width = 2, pad = 0), ".cpp")

cat("compiling", cppfile, "\n")
cpp11::cpp_source(here("source", cppfile))

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


  bg <- "grey10"


  # generate image ----------------------------------------------------------

  set.seed(seed)
  cat("making", seed, "\n")

  sample_shades <- function(n) {
    sample(colours(distinct = TRUE), size = n)
  }

  shades <- c("#E70000", "#FF8C00", "#FFEF00",
          "#00811F", "#0044FF", "#760089")

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


  make_base_shape <- function(shape = make_circle(), recurse = 5, stdev = .1) {
    deform(old_x = shape[,1], old_y = shape[,2], stdev = stdev, depth = recurse)
  }

  make_blurred_shape <- function(base, layers, recurse = 4, stdev = .1) {
    dat <- list()
    for(i in 1:layers) {
      out <- deform(base[,1], base[,2], stdev = stdev, depth = recurse)
      dat[[i]] <- tibble(
        x = out[,1],
        y = out[,2],
        gen = out[,3],
        pos = out[,4],
        sd = out[,5],
        id = i
      ) %>%
        arrange(pos)
    }
    dat <- bind_rows(dat)
    return(dat)
  }

  make_object <- function(base, shade, recurse = 4, stdev = .1, layers = 50, scale = NULL, elevation = NULL) {
    xsh <- 0 #runif(1, -1 , 1)
    ysh <- 0 #runif(1, -1 , 1)
    if(is.null(scale)) scale <- runif(1, .9, 1.5)
    if(is.null(elevation)) elevation <- runif(1, 0, 1)

    base %>%
      make_blurred_shape(layers = layers, recurse = recurse, stdev = stdev) %>%
      mutate(
        x = (x + xsh) * scale,
        y = (y + ysh) * scale,
        group = id + elevation
      )  %>%
      mutate(shade = shade)
  }

  cat("  data step\n")

  droplet <- function(shade, index, size = .15, scatter = 2) {
    x_shift <- runif(1, -scatter/2, scatter/2)
    y_shift <- runif(1, -scatter/2, scatter/2)
    el_shift <- runif(1)
    layers <- 30
    make_circle(n = 10) %>%
      make_base_shape(recurse = 4, stdev = .4) %>%
      make_object(
        shade = shade,
        scale = size + runif(1, -size * .75, size * .75),
        layers = layers,
        elevation = (index * layers) + el_shift,
        stdev = .4
      ) %>%
      mutate(
        x = x + x_shift,
        y = y + y_shift
      )
  }


  dat <- shades %>%
   sample(size = 30, replace = TRUE) %>%
    imap_dfr(droplet)

  cat("  render step\n")

  border <- 1
  pic <- ggplot(dat, aes(x, y, group = factor(group), fill = shade)) +
    geom_polygon(alpha = .1, show.legend = FALSE) +
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

