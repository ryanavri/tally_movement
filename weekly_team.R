# ============================================================
# STRAVA WEEKLY CHALLENGE: TEAM VS TEAM + INDIVIDUAL (MILES)
# FOURTH WEEK OF OCTOBER
# ============================================================

library(tidyverse)
library(ggtext)
library(scales)
library(patchwork)
library(glue)

# 1. INPUT DATA ----------------------------------------------

leaderboard <- tribble(
  ~Team,    ~Athlete,        ~km,  ~activities, ~minutes,
  
  # TEAM A: PANTHER PURRSUIT
  "Team A", "Marsya",        40.2,  9,          537,
  "Team A", "Ester",         19.3,  8,          507,
  "Team A", "Wantoso",       32.8,  4,          408,
  "Team A", "Ryan",          27.4,  6,          266,
  "Team A", "Veda",          12.4,  5,          232,
  "Team A", "Tutus",         20.0,  3,          189,
  "Team A", "Oche",          20.8,  2,          163,
  "Team A", "Ratna",         10.0,  2,          122,
  "Team A", "Awaludin",       1.6,  1,           37,
  
  # TEAM B: PAWSITIVE PACE
  "Team B", "Ruly",          53.7, 11,          961,
  "Team B", "Ola",           65.8,  6,          507,
  "Team B", "Tita",          26.1,  5,          363,
  "Team B", "Maris",         21.2,  5,          326,
  "Team B", "Ayu",           16.4,  3,          220,
  "Team B", "Tarida",        10.4,  2,          141,
  "Team B", "Rahman",        28.3,  2,          132,
  "Team B", "Jefta",         14.6,  6,          125,
  "Team B", "Chloe",         16.2,  2,           98,
  "Team B", "Aidil",         10.4,  3,           83,
  "Team B", "Edy",           10.8,  2,           64,
  "Team B", "David",          2.4,  2,           33,
  "Team B", "Doddy",          2.9,  1,           26,
  "Team B", "Fazlurrahman",   1.8,  1,           23
)

# Convert kilometres to miles
leaderboard <- leaderboard %>%
  mutate(miles = km * 0.621371)


# 2. TEAM COLOURS --------------------------------------------

team_colors <- c(
  "Team A" = "#1F78B4",
  "Team B" = "#FF7F00"
)

theme_competition <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold", size = 17
    ),
    plot.subtitle = element_text(
      color = "grey40", size = 11
    ),
    plot.caption = element_text(
      color = "grey50", size = 9
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(
      fill = "#FAFAFC", color = NA
    ),
    panel.background = element_rect(
      fill = "#FAFAFC", color = NA
    ),
    plot.margin = margin(15, 25, 15, 15)
  )


# 3. SUMMARISE TEAM PERFORMANCE ------------------------------

team_summary <- leaderboard %>%
  group_by(Team) %>%
  summarise(
    athletes = n(),
    distance = sum(miles),
    activities = sum(activities),
    hours = sum(minutes) / 60,
    avg_miles = mean(miles),
    .groups = "drop"
  ) %>%
  arrange(desc(distance))

winner <- team_summary$Team[1]
gap_miles <- diff(range(team_summary$distance))

# Team name for subtitle
winner_name <- case_when(
  winner == "Team A" ~ "Panther Purrsuit",
  winner == "Team B" ~ "Pawsitive Pace"
)

print(team_summary)


# 4. PLOT 1: TEAM VS TEAM ------------------------------------

p_team <- ggplot(
  team_summary,
  aes(
    x = reorder(Team, distance),
    y = distance,
    fill = Team
  )
) +
  geom_col(
    width = 0.55,
    alpha = 0.95
  ) +
  geom_text(
    aes(
      label = paste0(
        number(distance, accuracy = 0.1),
        " miles"
      )
    ),
    hjust = -0.12,
    size = 5.2,
    fontface = "bold"
  ) +
  coord_flip(
    ylim = c(0, max(team_summary$distance) * 1.25)
  ) +
  scale_fill_manual(values = team_colors) +
  scale_x_discrete(
    labels = c(
      "Team A" = "Panther Purrsuit",
      "Team B" = "Pawsitive Pace"
    )
  ) +
  labs(
    subtitle = glue(
      "{winner_name} leads by {round(gap_miles, 1)} miles"
    ),
    x = NULL,
    y = "Total Distance (miles)",
    caption = "Weekly Movement Challenge"
  ) +
  theme_competition +
  theme(
    axis.text.y = element_text(
      face = "bold", size = 13
    )
  )


# 5. PLOT 2: INDIVIDUAL LOLLIPOP -----------------------------

individual <- leaderboard %>%
  mutate(
    athlete_id = paste(Team, Athlete, sep = " - ")
  ) %>%
  arrange(miles) %>%
  mutate(
    athlete_id = factor(
      athlete_id,
      levels = unique(athlete_id)
    )
  )

p_individual <- ggplot(
  individual,
  aes(
    x = athlete_id,
    y = miles,
    color = Team
  )
) +
  geom_segment(
    aes(
      xend = athlete_id,
      y = 0,
      yend = miles
    ),
    linewidth = 1.7,
    lineend = "round",
    alpha = 0.8
  ) +
  geom_point(
    size = 4.5
  ) +
  geom_text(
    aes(
      label = paste0(
        number(miles, accuracy = 0.1),
        " mi"
      )
    ),
    nudge_y = 1,
    size = 3.7,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_flip(
    ylim = c(0, max(individual$miles) * 1.22)
  ) +
  facet_grid(
    Team ~ .,
    scales = "free_y",
    space = "free_y",
    labeller = as_labeller(
      c(
        "Team A" = "Panther Purrsuit",
        "Team B" = "Pawsitive Pace"
      )
    )
  ) +
  scale_color_manual(values = team_colors) +
  scale_x_discrete(
    labels = function(x) {
      sub("^Team [AB] - ", "", x)
    }
  ) +
  labs(
    title = "INDIVIDUAL LEADERBOARD",
    x = NULL,
    y = "Distance (miles)"
  ) +
  theme_competition +
  theme(
    strip.text.y = element_text(
      face = "bold", size = 12
    ),
    strip.background = element_rect(
      fill = "#EEEEF2", color = NA
    ),
    axis.text.y = element_text(
      face = "bold", size = 10
    )
  )


# 6. COMBINE THE TWO CHARTS ----------------------------------

final_plot <- p_team / p_individual +
  plot_layout(
    heights = c(1, 3.5)
  ) +
  plot_annotation(
    title = "WEEKLY MOVEMENT CHALLENGE",
    subtitle = "First Week of November",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 21,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        color = "grey40"
      ),
      plot.background = element_rect(
        fill = "#FAFAFC",
        color = NA
      )
    )
  )

print(final_plot)


# 7. EXPORT --------------------------------------------------

ggsave(
  filename = "Miles_Nov_1W.png",
  plot = final_plot,
  width = 12,
  height = 16,
  dpi = 300,
  bg = "#FAFAFC"
)
