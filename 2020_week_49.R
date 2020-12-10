library(tidyverse)
library(readxl)
library(gggibbous)
library(glue)
library(ggtext)
library(showtext)

font_add(
  family = "Atkinson Hyperlegible",
  regular = here::here("fonts", "Atkinson-Hyperlegible-Regular-102.otf"),
  bold = here::here("fonts", "Atkinson-Hyperlegible-Bold-102.otf")
)

df <- 
  read_excel(here::here("data", "Week49.xlsx")) %>% 
  group_by(Respondent, Year) %>% 
  mutate(prop = Value / sum(Value)) 

df_germany <- 
  df %>% 
  filter(Respondent == "Germany") %>% 
  add_column(
    y = 2.5,
    x = rep(1:4, 2),
    right = rep(c(FALSE, TRUE), each = 4)
  ) %>% 
  mutate(
    desc = glue("<span style='color:#DA8C78'>{Response} [{round(prop*100)}%]</span>") %>% as.character()
  )

df_us <- 
  df %>% 
  filter(Respondent == "United States") %>% 
  add_column(
    y = 0.8,
    x = rep(1:4, 2),
    right = rep(c(FALSE, TRUE), each = 4)
  ) %>% 
  mutate(
    desc = glue("<span style='color:#146C71'>{Response} [{round(prop*100)}%]</span>") %>% as.character()
  )

showtext_auto()

ggplot() +
  # Plot heading
  annotate("text", x = 0.75, y = 3.75, label = "How are foreign relations between\nGermany and the United States?", hjust = 0, size = 10, fontface = "bold", color = "#151D44", family = "Atkinson Hyperlegible") +
  # Germany bubbles
  geom_moon(data = df_germany, aes(x, y, ratio = prop, right = right, fill = right), size = 35, color = NA) +
  # Annotations Germany
  geom_richtext(data = filter(df_germany, Response == "Bad"), aes(x, y + 0.4, label = desc), family = "Atkinson Hyperlegible", size = 5, fontface = "bold", fill = NA, label.color = NA) +
  geom_text(data = df_germany, aes(x, y - 0.5, label = Year), family = "Atkinson Hyperlegible", size = 5, color = "grey40") +
  annotate("text", x = 0.75, y = 3.15, label = "Ask a German...", hjust = 0, size = 8, family = "Atkinson Hyperlegible", color = "#340D35") +
  # USA bubbles
  geom_moon(data = df_us, aes(x, y, ratio = prop, right = right, fill = right), size = 35, color = NA) +
  # Annotations USA
  geom_richtext(data = filter(df_us, Response == "Good"), aes(x, y + 0.4, label = desc), family = "Atkinson Hyperlegible", size = 5, fontface = "bold", fill = NA, label.color = NA) +
  geom_text(data = df_us, aes(x, y - 0.5, label = Year), family = "Atkinson Hyperlegible", size = 5, color = "grey40") +
  annotate("text", x = 0.75, y = 1.45, label = "Ask an American...", hjust = 0, size = 8, family = "Atkinson Hyperlegible", color = "#340D35") +
  # Formatting and stuff
  labs(caption = "**Data:** Koerber-Stiftung | **Plot:** Kaustav Sen") +
  scale_x_continuous(limits = c(0.5, 4.5)) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_fill_manual(values = c("#146C71", "#DA8C78")) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FDF5F3", color = "#FDF5F3"),
    plot.caption = element_markdown(size = 11, family = "Atkinson Hyperlegible", color = "grey40", margin = margin(r = 15, b = 15))
  ) +
  # Save plot as PDF
  ggsave(here::here("plots", "2020_week_49.pdf"), height = 8, width = 8, device = cairo_pdf)

# Convert to png
pdftools::pdf_convert(
  pdf = here::here("plots", "2020_week_49.pdf"),
  filenames = here::here("plots", "2020_week_49.png")
)