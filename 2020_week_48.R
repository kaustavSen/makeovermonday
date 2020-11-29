# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rockthemes)
library(showtext)
library(ggtext)

# Load Fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Roboto")

# Get Data ----------------------------------------------------------------

pilot_data <- read_csv("https://query.data.world/s/ggiv2m6wkeqos6svumerpykoebnoiz")

# Wrangle -----------------------------------------------------------------

gain_by_age <- 
  pilot_data %>% 
  mutate(
    age_group = cut(Age, breaks = c(-Inf, 20, 30, 40, 50, Inf), labels = c("less than 20", "20-30", "30-40", "40-50", "more than 50")),
    age_group = fct_rev(age_group)
  ) %>% 
  group_by(age_group) %>%
  summarise(
    avg_years_lost = -mean(`Years lost`),
    avg_years_gained = mean(`Years gained`)
  ) %>% 
  pivot_longer(cols = c(avg_years_gained, avg_years_lost), values_to = "years", names_to = "type")

gain_overall <- 
  pilot_data %>% 
  mutate(age_group = "overall") %>% 
  group_by(age_group) %>% 
  summarise(
    avg_years_lost = mean(`Years lost`),
    avg_years_gained = mean(`Years gained`)
  ) %>% 
  pivot_longer(cols = c(avg_years_gained, avg_years_lost), values_to = "years", names_to = "type")


# Plot --------------------------------------------------------------------

showtext_auto()

ggplot() +
  geom_linerange(
    data = filter(gain_by_age, type == "avg_years_gained"),
    aes(
      xmin = 5,
      xmax = years + 5,
      y = age_group
    ),
    size = 10,
    color = "#6dbac6"
  ) +
  geom_text(
    data = filter(gain_by_age, type == "avg_years_gained"),
    aes(
      x = years + 5,
      y = age_group,
      label = format(years, digits = 2)
    ),
    nudge_x = 0.75,
    color = "#6dbac6",
    hjust = 0,
    size = 3.5,
    family = "Roboto",
    fontface = "bold"
  ) +
  geom_linerange(
    data = filter(gain_by_age, type == "avg_years_lost"),
    aes(
      xmin = -5,
      xmax = years - 5,
      y = age_group
    ),
    size = 10,
    color = "#dac190"
  ) +
  geom_text(
    data = filter(gain_by_age, type == "avg_years_lost"),
    aes(
      x = years - 5,
      y = age_group,
      label = format(-years, digits = 1)
    ),
    nudge_x = -0.75,
    color = "#dac190",
    hjust = 1,
    size = 3.5,
    family = "Roboto",
    fontface = "bold"
  ) +
  geom_linerange(
    data = filter(gain_overall, type == "avg_years_gained"),
    aes(
      xmin = 5,
      xmax = years + 5,
      y = 0
    ),
    size = 10,
    color = colorspace::darken("#6dbac6", amount = 0.2)
  ) +
  geom_text(
    data = filter(gain_overall, type == "avg_years_gained"),
    aes(
      x = years + 5,
      y = 0,
      label = format(years, digits = 2, nsmall = 1)
    ),
    nudge_x = 0.75,
    color = colorspace::darken("#6dbac6", amount = 0.2),
    size = 3.5,
    hjust = 0,
    family = "Roboto",
    fontface = "bold"
  ) +
  geom_linerange(
    data = filter(gain_overall, type == "avg_years_lost"),
    aes(
      xmin = -5,
      xmax = -years - 5,
      y = 0
    ),
    size = 10,
    color = colorspace::darken("#dac190", amount = 0.2)
  ) +
  geom_text(
    data = filter(gain_overall, type == "avg_years_lost"),
    aes(
      x = -years - 5,
      y = 0,
      label = format(years, digits = 2, nsmall = 1)
    ),
    nudge_x = -0.75,
    color = colorspace::darken("#dac190", amount = 0.2),
    size = 3.5,
    hjust = 1,
    family = "Roboto",
    fontface = "bold"
  ) +
  annotate("text", x = 0, y = 1:5, label = c("More than\n50", "40-50", "30-40", "20-30", "Less than\n20"), fontface = "bold", color = "grey45", size = 3.5, family = "Roboto") +
  annotate("text", x = 0, y = 0, label = "Overall", fontface = "bold", color = "black", size = 3.5, family = "Roboto") +
  annotate("text", x = 0, y = 5.75, label = "Age Group", fontface = "bold", color = "black", size = 4, family = "Oswald") +
  annotate("text", x = -10, y = 5.65, label = "Years Lost", fontface = "bold", color = "#dac190", size = 3.25, family = "Oswald") +
  annotate("text", x = 10, y = 5.65, label = "Years Gained", fontface = "bold", color = "#6dbac6", size = 3.25, family = "Oswald") +
  annotate("richtext", x = -17, y = 6.5, label = "Women are <span style = 'color:#dac190'>losing</span> more than 2 healthy years of their lives as a result of fistula", hjust = 0, fontface = "bold", color = "black", size = 4.5, family = "Oswald", fill = NA, label.colour = NA) +
  annotate("richtext", x = -17, y = 6.15, label = "However, access to proper surgical treatment can help then <span style = 'color:#6dbac6'>**gain**</span> 12 years of a healthy and fistual-free life", hjust = 0, color = "grey45", size = 4, family = "Oswald", fill = NA, label.colour = NA) +
  annotate("richtext", x = -17, y = 4.5, label = "**Fistule** is a childbirth injury<br>caused by prolonged,<br>obstructed labour without<br>prompt medical attention", hjust = 0, color = "grey30", size = 3, family = "Roboto", fill = "grey90", label.colour = NA, lineheight = 0.9) +
  labs(
    caption = "**Data:** Operation Fistula | **Plot:** Kaustav Sen"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(20, 20, 10, 20),
    plot.background = element_rect(fill = "grey92", color = "grey92"),
    plot.caption = element_markdown(family = "Roboto", color = "grey35", size = 7, margin = margin(t = 25))
  ) +
  ggsave(here::here("plots", "2020_week_48.pdf"), height = 5, width = 8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = here::here("plots", "2020_week_48.pdf"),
  filenames = here::here("plots", "2020_week_48.png"),
  dpi = 150
)
