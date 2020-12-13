library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)
library(ggsci)
library(here)

font_add(
  family = "Atkinson Hyperlegible",
  regular = here("fonts", "Atkinson-Hyperlegible-Regular-102.otf"),
  bold = here("fonts", "Atkinson-Hyperlegible-Bold-102.otf"),
  bolditalic = here("fonts", "Atkinson-Hyperlegible-BoldItalic-102.otf")
)

bob_ross <- read_csv(here("data", "elements-by-episode.csv"))
  
top <- 
  bob_ross %>% 
  # Excluding these else we will be double counting "tree" 
  filter(! element %in% c("Trees", "Deciduous", "Conifer")) %>% 
  group_by(element) %>% 
  summarise(total = sum(included)) %>% 
  slice_max(total, n = 5) %>% 
  pull(element)

showtext_auto()

# Wrangle -----------------------------------------------------------------
plot_data <- 
  bob_ross %>% 
  filter(element %in% top) %>% 
  # Extracting the season and episode from the text
  separate(
    col = episode, 
    into = c("season", "episode"),
    sep = "E"
  ) %>% 
  mutate(
    season = parse_number(season),
    episode = parse_number(episode)
  ) %>%
  # Count the number of occurrences for each of the 
  # top 5 elements over the seasons
  group_by(season, element) %>% 
  summarise(occurance = sum(included)) %>% 
  ungroup() %>% 
  # Adding some "blanks" at the end points
  bind_rows(
    tibble(
      season = rep(c(-8:0, 32:40), each = 5),
      element = rep(top, times = length(c(-8:0, 32:40))),
      occurance = 0
    )
  ) %>%
  arrange(season) %>% 
  mutate(element = factor(element, levels = top, labels = c("Trees", "Mountains", "Lakes", "Grass", "Clouds")))
  
# Plot --------------------------------------------------------------------
ggplot(plot_data, aes(season, occurance, fill = element, color = element)) +
  geom_stream(
    size = 0.75,
    bw = 0.9,
    alpha = 0.5,
  ) +
  geom_text(
    data = tibble(x = 8, y = c(12, 2, -5.5, -10.5, -16), label = c("Trees", "Mountains", "Lakes", "Grass", "Clouds")),
    aes(x = x, y = y, label = label, color = label),
    fontface = "bold",
    inherit.aes = FALSE,
    family = "Atkinson Hyperlegible",
    size = 3.5,
    hjust = 0
  ) +
  annotate("segment", x = -8, xend = 40, y = -20, yend = -20, color = "grey60", size = 0.75) +
  annotate("segment", x = -8, xend = -8, y = -19, yend = -21, color = "grey60", size = 0.75) +
  annotate("segment", x = 40, xend = 40, y = -19, yend = -21, color = "grey60", size = 0.75) +
  annotate("richtext", x = 15, y = -20, label = "Seasons", family = "Atkinson Hyperlegible", fontface = "bold", label.color = NA) +
  annotate("text", x = 20, y = 10, label = "Higher height means\nhigher frequency", lineheight = 0.9, family = "Atkinson Hyperlegible", size = 3, hjust = 0, color = colorspace::lighten("#800000FF")) +
  annotate("segment", x = 19.5, xend = 19.5, y = 4, yend = 15, size = 0.5, arrow = arrow(length = unit(0.20, "cm"), ends = "both", type = "closed"), color = colorspace::lighten("#800000FF")) +
  labs(
    title = "What will you find in a Boss Ross painting?",
    subtitle = "Top 5 elements used in the paintings over the seasons of ***The Joy of Painting***",
    caption = "**Data:** Github | **Plot:** Kaustav Sen"
    
  ) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 20),
    plot.title = element_text(
      family = "Atkinson Hyperlegible",
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_markdown(
      family = "Atkinson Hyperlegible",
      size = 10,
      color = "grey35"
    ),
    plot.caption = element_markdown(
      family = "Atkinson Hyperlegible",
      size = 8,
      color = "grey65"
    )
  ) +
  ggsave(here("plots", "2020_week_50.pdf"), width = 9, height = 5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = here("plots", "2020_week_50.pdf"),
  filenames = here("plots", "2020_week_50.png"),
  dpi = 150
)
