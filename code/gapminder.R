library(tidyverse)
library(gganimate)
library(gghighlight)
library(extrafont)
extrafont::loadfonts()
library(gapminder)
library(gifski)

# 1. gapminder ------------------------------------------------------------

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

gapminder_gif <- p + transition_time(year) +
  labs(title = "Year: {frame_time}")

anim_save(filename = "images/gapminder.gif")


# 2. airquality -----------------------------------------------------------

mean.temp <- airquality %>%
  group_by(Month) %>%
  summarise(Temp = mean(Temp))
mean.temp

p <- ggplot(mean.temp, aes(Month, Temp, fill = Temp)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
p

p + transition_states(Month, wrap = FALSE) +
  shadow_mark()

anim_save(filename = "images/airquality.gif", device = 'ragg_png')


# 3. high quality ---------------------------------------------------------

# 3.1. 좋지 않음 --------------------------------------------------------------

gplot <- 
  ggplot(
    gapminder,
    aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
  ) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  transition_time(year)

magick::image_write(
  anim_save("images/high_quality.gif", gplot, width = 1000, height = 1000)
)


# 3.2. iris ---------------------------------------------------------------

p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point() +
  labs(title = "한글") +
  theme_minimal(base_family = "NanumSquare_ac") 

anim <- p +
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

iris_ani <- animate(anim, duration = 3.3, 
                    fps = 10, width = 1400, height = 865, 
                    renderer = gifski_renderer(), res = 200, type = "cairo")

anim_save("images/iris.gif", animation = iris_ani)
