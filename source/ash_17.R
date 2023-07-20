

# setup -------------------------------------------------------------------

library(here)
library(ggplot2)
library(tibble)
library(stringr)
library(dplyr)
library(purrr)
library(cpp11)

sys_id <- "ash"
sys_version <- 17

cppfile <- paste0(
  sys_id,
  "_",
  str_pad(sys_version, width = 2, pad = 0),
  ".cpp"
)

cat("compiling", cppfile, "\n")
cpp_source(here("source", cppfile))

save_path <- function(sys_id, sys_version, seed, fmt = ".png") {
  sys_version <- sys_version %>% str_pad(width = 2, pad = "0")
  seed <- seed %>% str_pad(width = 3, pad = "0")
  base <- paste(sys_id, sys_version, seed, sep = "_")
  file <- paste0(base, fmt)
  path <- here("image", file)
  return(path)
}

# plot parameters ---------------------------------------------------------

for(seed in 29) {

  bg <- "grey10"


  # generate image ----------------------------------------------------------

  set.seed(seed)
  cat("making", seed, ": data\n")

  shades <- sample(ggthemes::canva_palettes, 1)[[1]]
  shades <- sample(shades)

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
      as.matrix()
    return(base)
  }

  make_base_shape <- function(shape = make_circle(), recurse = 6, stdev = 1) {
    base <- deform(old_x = shape[,1], old_y = shape[,2], stdev = stdev, depth = recurse)
    return(base)
  }

  make_blurred_shape <- function(base, layers, recurse = 6, stdev = .1) {
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


  make_object <- function(base, shade, layers = 50, elevation = NULL) {
    if(is.null(elevation)) elevation <- runif(1, 0, 1)

    base %>%
      make_blurred_shape(layers = layers, recurse = 6) %>%
      mutate(
        group = id + elevation
      )  %>%
      mutate(shade = shade)
  }

  make_layout <- function(shade1, shade2) {

    dat <- bind_rows(
      make_heart(n = 20) %>%
        make_base_shape(stdev = .1) %>%
        make_object(
          shade = shade1,
          layers = 5,
          elevation = -.1 + runif(1, 0, .1)
        ),
      make_heart(n = 20) %>%
        make_base_shape(stdev = .1) %>%
        make_object(
          shade = shade2,
          layers = 5,
          elevation = -.2 + runif(1, 0, .1)
        ),
      make_heart(n = 20) %>%
        make_base_shape(recurse = 6, stdev = .1) %>%
        make_object(
          shade = "black",
          layers = 5,
          elevation = 50.1 + runif(1, 0, .1)
        )
    )
  }

  dat <- make_layout(shades[1], shades[2])

  border <- 1
  pic <- ggplot(dat, aes(x, y, group = factor(group), fill = shade)) +
    geom_polygon(alpha = .08) +
    coord_equal(
      xlim = c(-1, 1) * border,
      ylim = c(-1, 1) * border
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = bg, colour = bg)) +
    scale_fill_identity()

  cat("making", seed, ": render\n")
  ggsave(
    filename = save_path(sys_id, sys_version, seed),
    bg = bg,
    plot = pic,
    width = 10,
    height = 10,
    dpi = 300
  )
}

