# Library
library(ggtext)
library(RColorBrewer)
library(tidyverse)

leaderboard_w <- leaderboard %>%
  filter(Date == "2025_Oct_W2")

pal <- c(
  "#FFD92F", # vivid yellow 
  "#1F78B4", # strong blue
  "#33A02C", # rich green
  "#E31A1C", # red
  "#FF7F00", # orange
  "#6A3D9A", # purple
  "#A6CEE3", # light sky blue
  "#B2DF8A", # soft green
  "#FB9A99", # soft red
  "#FDBF6F", # peach
  "#CAB2D6"  # lilac
)


max_miles <- max(leaderboard_w$miles, na.rm = TRUE)
icon <- "🏃‍➡️"   # Right-facing running emoji

# Calculate total accumulated miles automatically
total_miles <- sum(leaderboard_w$miles, na.rm = TRUE)

plt_lollipop <- ggplot(leaderboard_w, aes(x = reorder(Athlete, miles), y = miles, color = Athlete)) +
  # stem (colored by Athlete)
  geom_segment(
    aes(xend = reorder(Athlete, miles), y = 0, yend = miles),
    linewidth = 1.8, lineend = "round", alpha = 0.85
  ) +
  # icon at the tip
  geom_text(aes(label = icon), size = 6, vjust = 0.5) +
  # value label (colored by Athlete)
  geom_text(aes(label = scales::number(miles, accuracy = 0.1)),
            nudge_y = max_miles * 0.06, size = 4.8, fontface = "bold") +
  coord_flip(ylim = c(0, max_miles * 1.2)) +
  scale_color_manual(values = pal) +
  labs(
    title = "Weekly Miles",
    subtitle = glue::glue("1st Week of October 2025 — {round(total_miles, 1)} accumulated miles"), #change
    x = NULL, y = "Miles", color = "Athlete"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey35"),
    axis.text.y = element_markdown(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "#FAFAFC", color = NA),
    panel.background = element_rect(fill = "#FAFAFC", color = NA),
    plot.margin = margin(10, 20, 10, 20)
  )

print(plt_lollipop)
