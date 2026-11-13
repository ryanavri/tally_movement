
# ============================================================
# STRAVA MOVEMENT CHALLENGE - OVERALL 4-WEEK RESULTS
# OCTOBER WEEK 2 - NOVEMBER WEEK 1
# ============================================================

library(tidyverse)
library(scales)
library(patchwork)
library(glue)

# 1. INPUT WEEKLY INDIVIDUAL DATA ----------------------------

leaderboard <- tribble(
  ~Week, ~Team, ~Athlete, ~miles,
  
  # OCTOBER - WEEK 2
  "Oct W2", "Team A", "Marsya",        20.1,
  "Oct W2", "Team A", "Oche",          17.9,
  "Oct W2", "Team A", "Tutus",         11.8,
  "Oct W2", "Team A", "Ester",          7.8,
  "Oct W2", "Team A", "Ryan",           5.0,
  "Oct W2", "Team A", "Veda",           4.5,
  "Oct W2", "Team A", "Awaludin",       2.7,
  
  "Oct W2", "Team B", "Ola",           11.7,
  "Oct W2", "Team B", "Tita",          11.7,
  "Oct W2", "Team B", "Maris",          9.3,
  "Oct W2", "Team B", "Chloe",          8.8,
  "Oct W2", "Team B", "David",          5.7,
  "Oct W2", "Team B", "Jefta",          3.8,
  "Oct W2", "Team B", "Shomat",         3.6,
  
  # OCTOBER - WEEK 3
  "Oct W3", "Team A", "Marsya",        15.2,
  "Oct W3", "Team A", "Tutus",          9.6,
  "Oct W3", "Team A", "Oche",           6.8,
  "Oct W3", "Team A", "Ester",          6.3,
  "Oct W3", "Team A", "Shafia",         5.7,
  "Oct W3", "Team A", "Ryan",           3.8,
  "Oct W3", "Team A", "Veda",           3.3,
  "Oct W3", "Team A", "Wantoso",        2.5,
  "Oct W3", "Team A", "Yuke",           1.3,
  "Oct W3", "Team A", "Cahyo",          1.1,
  "Oct W3", "Team A", "Ratna",          0.6,
  
  "Oct W3", "Team B", "Ola",           29.8,
  "Oct W3", "Team B", "David",          6.5,
  "Oct W3", "Team B", "Tita",           6.3,
  "Oct W3", "Team B", "Shomat",         6.1,
  "Oct W3", "Team B", "Chloe",          4.5,
  "Oct W3", "Team B", "Jefta",          4.0,
  "Oct W3", "Team B", "Najma",          3.0,
  
  # OCTOBER - WEEK 4
  "Oct W4", "Team A", "Marsya",        25.0,
  "Oct W4", "Team A", "Wantoso",       20.4,
  "Oct W4", "Team A", "Ryan",          17.0,
  "Oct W4", "Team A", "Oche",          12.9,
  "Oct W4", "Team A", "Tutus",         12.4,
  "Oct W4", "Team A", "Ester",         12.0,
  "Oct W4", "Team A", "Veda",           7.7,
  "Oct W4", "Team A", "Ratna",          6.2,
  "Oct W4", "Team A", "Yuke",          26.5,
  "Oct W4", "Team A", "Awaludin",       1.0,
  
  "Oct W4", "Team B", "Ola",           40.9,
  "Oct W4", "Team B", "Ruly",          33.4,
  "Oct W4", "Team B", "Rahman",        17.6,
  "Oct W4", "Team B", "Tita",          16.2,
  "Oct W4", "Team B", "Maris",         13.2,
  "Oct W4", "Team B", "Ayu",           10.2,
  "Oct W4", "Team B", "Chloe",         10.1,
  "Oct W4", "Team B", "Jefta",          9.1,
  "Oct W4", "Team B", "Edy",            6.7,
  "Oct W4", "Team B", "Aidil",          6.5,
  "Oct W4", "Team B", "Tarida",         6.5,
  "Oct W4", "Team B", "Doddy",          1.8,
  "Oct W4", "Team B", "David",          1.5,
  "Oct W4", "Team B", "Shomat",         1.1,
  
  # NOVEMBER - WEEK 1
  "Nov W1", "Team A", "Ester",          7.7,
  "Nov W1", "Team A", "Ratna",          7.6,
  "Nov W1", "Team A", "Tutus",          6.3,
  "Nov W1", "Team A", "Marsya",         5.1,
  "Nov W1", "Team A", "Wantoso",        4.8,
  "Nov W1", "Team A", "Awaludin",       4.1,
  "Nov W1", "Team A", "Cahyo",          3.6,
  "Nov W1", "Team A", "Oche",           2.8,
  "Nov W1", "Team A", "Ryan",           0.7,
  
  "Nov W1", "Team B", "Najma",         11.2,
  "Nov W1", "Team B", "Maris",         10.3,
  "Nov W1", "Team B", "Agung",         10.2,
  "Nov W1", "Team B", "Chloe",          9.1,
  "Nov W1", "Team B", "Ayu",            8.8,
  "Nov W1", "Team B", "Tarida",         8.6,
  "Nov W1", "Team B", "Edy",            8.5,
  "Nov W1", "Team B", "Ola",            7.0,
  "Nov W1", "Team B", "Ruly",           5.7,
  "Nov W1", "Team B", "Tita",           1.6,
  "Nov W1", "Team B", "David",          0.6
)

# 2. TEAM NAMES AND COLOURS ----------------------------------

team_names <- c(
  "Team A" = "Panther Purrsuit",
  "Team B" = "Pawsitive Pace"
)

team_colors <- c(
  "Panther Purrsuit" = "#1F78B4",
  "Pawsitive Pace" = "#FF7F00"
)

week_levels <- c(
  "Oct W2", "Oct W3", "Oct W4", "Nov W1"
)

leaderboard <- leaderboard %>%
  mutate(
    Team = recode(Team, !!!team_names),
    Week = factor(Week, levels = week_levels)
  )

# 3. OFFICIAL WEEKLY TEAM TOTALS -----------------------------

# Use the totals shown in the original charts.
# These may differ slightly from sums of individually
# rounded distances.

weekly_team <- tribble(
  ~Week,    ~Team,               ~miles,
  "Oct W2", "Panther Purrsuit",    69.9,
  "Oct W2", "Pawsitive Pace",      54.6,
  "Oct W3", "Panther Purrsuit",    56.2,
  "Oct W3", "Pawsitive Pace",      60.2,
  "Oct W4", "Panther Purrsuit",   114.6,
  "Oct W4", "Pawsitive Pace",     174.6,
  "Nov W1", "Panther Purrsuit",    42.8,
  "Nov W1", "Pawsitive Pace",      81.5
) %>%
  mutate(
    Week = factor(Week, levels = week_levels)
  ) %>%
  arrange(Team, Week) %>%
  group_by(Team) %>%
  mutate(cumulative = cumsum(miles)) %>%
  ungroup()

team_summary <- weekly_team %>%
  group_by(Team) %>%
  summarise(
    total_miles = sum(miles),
    .groups = "drop"
  ) %>%
  arrange(desc(total_miles))

# Individual cumulative results
individual_summary <- leaderboard %>%
  group_by(Team, Athlete) %>%
  summarise(
    total_miles = sum(miles),
    active_weeks = n_distinct(Week),
    .groups = "drop"
  ) %>%
  arrange(Team, desc(total_miles))

winner <- team_summary$Team[1]

gap_miles <- diff(range(team_summary$total_miles))

print(team_summary)
print(individual_summary)


# 4. SHARED THEME --------------------------------------------

theme_competition <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold", size = 17
    ),
    plot.subtitle = element_text(
      size = 11, color = "grey40"
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


# 5. PLOT 1: OVERALL TEAM COMPETITION ------------------------

p_team <- ggplot(
  team_summary,
  aes(
    x = reorder(Team, total_miles),
    y = total_miles,
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
        number(total_miles, accuracy = 0.1),
        " miles"
      )
    ),
    hjust = -0.12,
    size = 5,
    fontface = "bold"
  ) +
  coord_flip(
    ylim = c(0, max(team_summary$total_miles) * 1.25)
  ) +
  scale_fill_manual(values = team_colors) +
  labs(
    title = "OVERALL TEAM COMPETITION",
    subtitle = glue(
      "{winner} leads by {round(gap_miles, 1)} miles"
    ),
    x = NULL,
    y = "Total Distance (miles)"
  ) +
  theme_competition +
  theme(
    axis.text.y = element_text(
      face = "bold", size = 12
    )
  )


# 6. PLOT 2: CUMULATIVE TEAM PROGRESS ------------------------

p_progress <- ggplot(
  weekly_team,
  aes(
    x = Week,
    y = cumulative,
    group = Team,
    color = Team
  )
) +
  geom_line(
    linewidth = 2,
    lineend = "round"
  ) +
  geom_point(
    size = 4
  ) +
  geom_text(
    data = weekly_team %>%
      filter(Week == "Nov W1"),
    aes(
      label = paste0(
        number(cumulative, accuracy = 0.1),
        " mi"
      )
    ),
    hjust = -0.15,
    fontface = "bold",
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_manual(values = team_colors) +
  scale_y_continuous(
    limits = c(0, 420),
    breaks = seq(0, 400, 100),
    expand = c(0, 0)
  ) +
  labs(
    title = "CUMULATIVE TEAM PROGRESS",
    subtitle = "How the competition evolved each week",
    x = NULL,
    y = "Cumulative Distance (miles)"
  ) +
  theme_competition +
  theme(
    panel.grid.major.y = element_line(
      color = "grey90"
    ),
    panel.grid.major.x = element_blank()
  )


# 7. PLOT 3: OVERALL INDIVIDUAL LEADERBOARD ------------------

individual <- individual_summary %>%
  mutate(
    athlete_id = paste(Team, Athlete, sep = " - ")
  ) %>%
  arrange(total_miles) %>%
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
    y = total_miles,
    color = Team
  )
) +
  geom_segment(
    aes(
      xend = athlete_id,
      y = 0,
      yend = total_miles
    ),
    linewidth = 1.7,
    lineend = "round",
    alpha = 0.8
  ) +
  geom_point(
    size = 4.2
  ) +
  geom_text(
    aes(
      label = paste0(
        number(total_miles, accuracy = 0.1),
        " mi"
      )
    ),
    nudge_y = 2,
    hjust = -0.12,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_flip(
    ylim = c(0, max(individual$total_miles) * 1.22)
  ) +
  facet_grid(
    Team ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_color_manual(values = team_colors) +
  scale_x_discrete(
    labels = function(x) {
      sub("^.* - ", "", x)
    }
  ) +
  labs(
    title = "OVERALL INDIVIDUAL LEADERBOARD",
    subtitle = "Accumulated distance across four weeks",
    x = NULL,
    y = "Total Distance (miles)"
  ) +
  theme_competition +
  theme(
    strip.text.y = element_text(
      face = "bold", size = 12
    ),
    strip.background = element_rect(
      fill = "#EEEEF2",
      color = NA
    ),
    axis.text.y = element_text(
      face = "bold", size = 10
    )
  )


# 8. COMBINE DASHBOARD ---------------------------------------

final_plot <- (p_team / p_progress / p_individual) +
  plot_layout(
    heights = c(1, 1.2, 4)
  ) +
  plot_annotation(
    title = "WEEKLY MOVEMENT CHALLENGE",
    subtitle = "Four-Week Recap | October - November 2026",
    caption = paste(
      "Source: Strava weekly leaderboard.",
      "Individual totals are based on rounded chart values."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        color = "grey40"
      ),
      plot.caption = element_text(
        size = 9,
        color = "grey50"
      ),
      plot.background = element_rect(
        fill = "#FAFAFC",
        color = NA
      )
    )
  )

print(final_plot)


# 9. EXPORT --------------------------------------------------

ggsave(
  filename = "strava_overall_4week_challenge.png",
  plot = final_plot,
  width = 12,
  height = 20,
  dpi = 300,
  bg = "#FAFAFC"
)

# Optional: export individual cumulative results
write_csv(
  individual_summary,
  "strava_overall_individual_results.csv"
)

# Optional: export weekly team progress
write_csv(
  weekly_team,
  "strava_weekly_team_progress.csv"
)
