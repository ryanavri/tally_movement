# (C) Lollipop — icon/emoji enhanced version (colorful)
library(ggtext)
library(RColorBrewer)

max_miles <- max(leaderboard$total_miles, na.rm = TRUE)
icon <- "🏃‍➡️"   # Right-facing running emoji

# Discrete palette per athlete
pal <- brewer.pal(max(3, length(unique(leaderboard$athlete))), "Set2")

plt_lollipop <- ggplot(leaderboard, aes(x = reorder(athlete, total_miles), y = total_miles, color = athlete)) +
  # stem (colored by athlete)
  geom_segment(
    aes(xend = reorder(athlete, total_miles), y = 0, yend = total_miles),
    linewidth = 1.8, lineend = "round", alpha = 0.85
  ) +
  # icon at the tip
  geom_text(aes(label = icon), size = 6, vjust = 0.5) +
  # value label (colored by athlete)
  geom_text(aes(label = scales::number(total_miles, accuracy = 0.1)),
            nudge_y = max_miles * 0.06, size = 4.8, fontface = "bold") +
  coord_flip(ylim = c(0, max_miles * 1.2)) +
  scale_color_manual(values = pal) +
  labs(
    title = "Weekly Miles",
    subtitle = "1st Week of October 2025",
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
