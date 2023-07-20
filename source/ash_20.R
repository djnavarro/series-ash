

# setup -------------------------------------------------------------------

library(here)
library(ggplot2)
library(tibble)
library(stringr)
library(dplyr)
library(purrr)
library(cpp11)
library(ambient)
library(scales)

sys_id <- "ash"
sys_version <- 20

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

for(seed in 1:10) {

  bg <- "grey10"


  # generate image ----------------------------------------------------------

  set.seed(seed)
  cat("making", seed, ": data\n")

  shades <- sample(ggthemes::canva_palettes, 1)[[1]]
  shades <- sample(shades)

  perlin_circle <- function(cx = 0, cy = 0, n = 100, noise_max = 0.5,
                            octaves = 2, r_min = 0.5, r_max = 1,
                            frequency = 1) {
    circ <- tibble(
      angle = seq(0, 2*pi, length.out = n),
      xoff = cos(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
      yoff = sin(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
      r = gen_simplex %>%
        fracture(
          fractal = fbm,
          x = xoff,
          y = yoff,
          octaves = octaves,
          frequency = frequency
        ) %>%
        rescale(from = c(-0.5, 0.5), to = c(r_min, r_max)),
      x = r * cos(angle) + cx,
      y = r * sin(angle) + cy,
      xbase = mean(r) * cos(angle) + cx,
      ybase = mean(r) * sin(angle) + cy
    )

    return(circ)
    #keep <- with(circ, chull(x, y))
    #return(circ[keep,])
  }


  make_circle <- function(n = 10) {
    angle <- seq(from = 0, to = 2*pi, length.out = n)
    perlin <- perlin_circle(n = n, r_min = .9, r_max = 1.1)
    base <- tibble(
      x = perlin$xbase,
      y = perlin$ybase,
      drift_x = 1 + (perlin$x - x),
      drift_y = 1 + (perlin$y - y)
    ) %>%
      as.matrix()
    return(base)
  }

  make_base_shape <- function(shape, recurse = 6, stdev = 1) {
    base <- deform(
      old_x = shape[,1],
      old_y = shape[,2],
      drift_x = shape[,3],
      drift_y = shape[,4],
      stdev = stdev,
      depth = recurse
    )
    return(base)
  }

  make_blurred_shape <- function(base, layers, recurse = 6, stdev = .1) {
    dat <- list()
    for(i in 1:layers) {
      out <- deform(
        base[,1],
        base[,2],
        drift_x = base[,6],
        drift_y = base[,7],
        stdev = stdev,
        depth = recurse
      )
      dat[[i]] <- tibble(
        x = out[,1],
        y = out[,2],
        gen = out[,3],
        pos = out[,4],
        sd = out[,5],
        x_drift = out[,6],
        y_drift = out[,7],
        id = i
      ) %>%
        arrange(pos)
    }
    dat <- bind_rows(dat)
    return(dat)
  }

  dat <- make_circle(n = 12) %>%
    make_base_shape(stdev = 1, recurse = 4) %>%
    make_blurred_shape(stdev = .4, layers = 50, recurse = 7) %>%
    mutate(group = id + runif(1, 0, .1))  %>%
    mutate(x = x * .6, y = y * .6) %>%
    mutate(shade = "red")

  make_texture <- function(...) {
    sz <- runif(1, min = .1, max = 1)
    x_sh <- runif(1, min = -1, max = 1)
    y_sh <- runif(1, min = -1, max = 1)
    make_circle(n = 12) %>%
      make_base_shape(stdev = 1, recurse = 4) %>%
      make_blurred_shape(stdev = .4, layers = 5, recurse = 7) %>%
      mutate(group = id + runif(1, 0, .1) + 50)  %>%
      mutate(x = x * sz + x_sh, y = y * sz + y_sh) %>%
      mutate(shade = "grey10")
  }

  dat <- bind_rows(dat, map_dfr(1:20, make_texture))

  border <- 1
  pic <- ggplot(dat, aes(x, y, group = factor(group), fill = shade)) +
    geom_polygon(alpha = .01) +
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

