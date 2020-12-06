# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rockthemes)
library(showtext)
library(ggtext)
library(patchwork)
library(ggimage)

# Load Fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Roboto Condensed")

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

logo <- here::here("img", "20190227-Logos_Operation Fistula Logo Black.png")
viz5 <- here::here("img", "20200305 Viz5 Logo.png")

plot_1 <- ggplot() +
  geom_image(data = tibble(logo = logo), aes(x = 0, y = 0.8, image = logo), hjust = 0, size = 0.25) +
  geom_image(data = tibble(logo = viz5), aes(x = 1, y = 0.9, image = logo), hjust = 1, size = 0.10, asp = 1.5) +
  annotate("richtext", x = 0, y = 0.65, label = "**Fistula** is a childbirth injury caused by prolonged, obstructed labour without<br>prompt medical attention.", hjust = 0, family = "Roboto Condensed", size = 5, fill = NA, label.color = NA) +
  annotate("richtext", x = 0, y = 0.475, label = "A Pilot Program run from 2012 to 2015 in the African continent proved to be<br>immensely successful.", hjust = 0, family = "Roboto Condensed", size = 5, fill = NA, label.color = NA) +
  annotate("richtext", x = 0.15, y = 0.3, label = "Countries covered<br><span style='color:black'>5</span>", hjust = 0.5, family = "Oswald", size = 6, color = "grey60", fill = NA, label.color = NA) +
  annotate("richtext", x = 0.525, y = 0.3, label = "Patients treated<br><span style='color:black'>638</span>", hjust = 0.5, family = "Oswald", size = 6, color = "grey60", fill = NA, label.color = NA) +
  annotate("richtext", x = 0.85, y = 0.3, label = "Success Rate<br><span style='color:black'>88%</span>", hjust = 0.5, family = "Oswald", size = 6, color = "grey60", fill = NA, label.color = NA) +
  annotate("richtext", x = 0, y = 0.15, label = "Women are <span style = 'color:#dac190'>losing</span> more than 2 healthy years of their lives as a result of fistula.", hjust = 0, size = 5, family = "Roboto Condensed", fill = NA, label.colour = NA) +
  annotate("richtext", x = 0, y = 0.05, label = "However, access to proper surgical treatment can help them <span style = 'color:#6dbac6'>**gain**</span> 12 years of a<br>healthy and fistula-free life.", hjust = 0, size = 5, family = "Roboto Condensed", fill = NA, label.colour = NA)+
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(0, 10, 10, 10),
    plot.background = element_rect(fill = "grey92", color = "grey92"),
  )

plot_2 <- 
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
    size = 5,
    family = "Roboto Condensed",
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
    size = 5,
    family = "Roboto Condensed",
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
    size = 5,
    hjust = 0,
    family = "Roboto Condensed",
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
    size = 5,
    hjust = 1,
    family = "Roboto Condensed",
    fontface = "bold"
  ) +
  annotate("text", x = 0, y = 1:5, label = c("More than\n50", "40-50", "30-40", "20-30", "Less than\n20"), fontface = "bold", color = "grey45", size = 5, family = "Roboto Condensed") +
  annotate("text", x = 0, y = 0, label = "Overall", fontface = "bold", color = "black", size = 5, family = "Roboto Condensed") +
  annotate("text", x = 0, y = 5.75, label = "Age Group", fontface = "bold", color = "black", size = 5.75, family = "Oswald") +
  annotate("text", x = -10, y = 5.65, label = "Years Lost", fontface = "bold", color = "#dac190", size = 5, family = "Oswald") +
  annotate("text", x = 10, y = 5.65, label = "Years Gained", fontface = "bold", color = "#6dbac6", size = 5, family = "Oswald") +
  annotate("text", x = -14, y = -1, label = "Now you can help create a fistula free world", hjust = 0, fontface = "bold", color = "black", size = 5.75, family = "Oswald") +
  annotate("richtext", x = -10, y = -2, label = "$1 can give back 23 days of fistuala-free live to a person", hjust = 0, fontface = "bold", color = "#611612", size = 5.75, family = "Oswald", fill = "grey88", label.color = NA, label.padding = unit(rep(0.5, 4), "lines")) +
  annotate("richtext", x = -14, y = -2.65, label = "Donate now: opfistula.giv.sh/39cb", hjust = 0, fontface = "bold", color = "#EFA200", size = 5.75, family = "Oswald", fill = "NA", label.color = NA) +
  labs(caption = "**Data:** Operation Fistula | **Plot:** Kaustav Sen") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(0, 10, 5, 10),
    plot.background = element_rect(fill = "grey92", color = "grey92"),
    plot.caption = element_markdown(family = "Roboto Condensed", color = "grey35", size = 7, margin = margin(t = 25))
  )

plot_final <- plot_1 / plot_2 &
  theme(
    plot.background = element_rect(fill = "grey92", color = "grey92")
  ) 

ggsave(here::here("plots", "2020_week_48_alt.pdf"), plot_final, height = 10, width = 8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = here::here("plots", "2020_week_48_alt.pdf"),
  filenames = here::here("plots", "2020_week_48_alt.png"),
  dpi = 72
)
