source("r/header.R")

# Color figure
# color1 <- "tomato"
# color2 <- "steelblue"

# Greu-scale figure
color1 <- "black"
color2 <- "black"

# A. one-to-one trait-environment mapping ----

env <- tibble(
  x = seq(-3, 3, length.out = 1e2)
) %>% mutate(
  y = dnorm(x)
) %>%
  bind_rows(
    tibble(
      x = rev(.$x),
      y = rep(0, length(.$x))
    )
  )

gpA1 <- ggplot(env, aes(x = x, y = y)) +
  geom_polygon(fill = color1, color = color1, alpha = 0.5) +
  xlab("Environment") +
  ylab("Frequency") +
  theme_bw() + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()) +
  NULL

env2trait <- tibble(
  x = c(0, 1),
  y = c(0, 1)
)

gpA2 <- ggplot(env2trait, aes(x, y)) +
  geom_line(size = 1.1) +
  xlab("Environmental Gradient") +
  ylab("Trait Value") +
  theme_bw() + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()) +
  NULL

trait <- tibble(
  x = seq(-3, 3, length.out = 1e2)
) %>% mutate(
  y = dnorm(x)
) %>%
  bind_rows(
    tibble(
      x = rev(.$x),
      y = rep(0, length(.$x))
    )
  )

gpA3 <- ggplot(trait, aes(x = x, y = y)) +
  geom_polygon(fill = color2, color = color2, alpha = 0.5) +
  xlab("Trait Value") +
  ylab("Frequency") +
  theme_bw() + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()) +
  NULL

# B. Intermediate environment is rare ----

env <- tibble(
  x = seq(0, 1, length.out = 1e2)
) %>% mutate(
  y = dbeta(x, 0.5, 0.5)
) %>%
  bind_rows(
    tibble(
      x = rev(.$x),
      y = rep(0, length(.$x))
    )
  )

gpB1 <- ggplot(env, aes(x = x, y = y)) +
  geom_polygon(fill = color1, color = color1, alpha = 0.5) +
  xlab("Environment") +
  ylab("Frequency") +
  theme_bw() + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()) +
  NULL

gpB3 <- ggplot(env, aes(x = x, y = y)) +
  geom_polygon(fill = color2, color = color2, alpha = 0.5) +
  xlab("Trait Value") +
  ylab("Frequency") +
  theme_bw() + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()) +
  NULL

# C. Intermediate trait is rarely fit ----

env <- tibble(
  x = seq(-3, 3, length.out = 1e2)
) %>% mutate(
  y = dnorm(x)
) %>%
  bind_rows(
    tibble(
      x = rev(.$x),
      y = rep(0, length(.$x))
    )
  )

env2trait <- tibble(
  x = seq(-3, 3, length.out = 1e2)
) %>%
  mutate(y = 1 / (1 + exp(-10 * x)))

gpC2 <- ggplot(env2trait, aes(x, y)) +
  geom_line(size = 1.1) +
  xlab("Environmental Gradient") +
  ylab("Trait Value") +
  theme_bw() + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()) +
  NULL

my_dist <- 1 / (1 + exp(-2.4 * Norm(0, 1)))
trait <- tibble(
  x = seq(0, 1, length.out = 1e2)
) %>% mutate(
  y = d(my_dist)(x)
) %>%
  bind_rows(
    tibble(
      x = rev(.$x),
      y = rep(0, length(.$x))
    )
  )

gpC3 <- ggplot(trait, aes(x = x, y = y)) +
  geom_polygon(fill = color2, color = color2, alpha = 0.5) +
  xlab("Trait Value") +
  ylab("Frequency") +
  theme_bw() + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()) +
  NULL

plot_grid(
  gpA1, gpA2, gpA3,
  gpB1, gpA2, gpB3,
  gpA1, gpC2, gpC3,
  ncol = 3, align = "hv", labels = LETTERS[1:9], axis = "t"
  )

ggsave("ms/figures/fig1-hypotheses.pdf", width = 6.5, height = 6.5)
