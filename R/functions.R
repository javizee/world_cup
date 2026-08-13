#' Load and inspect both World Cup tables
#'
#' We inspect both tables before joining to understand their structure
#' independently — what's the grain of each table, what columns overlap,
#' and what the join key is.
#'
#' @return List with wcmatches and worldcups tibbles
load_and_inspect <- function() {
  
  glimpse(wcmatches)
  glimpse(worldcups)
  
  cat("\nNumber of tournaments:\n")
  print(worldcups %>% count())
  
  cat("\nNumber of matches:\n")
  print(wcmatches %>% count())
  
  cat("\nYears covered:\n")
  print(
    worldcups %>%
      summarise(
        min_year = min(year),
        max_year = max(year)
      )
  )
  
  cat("\nStages:\n")
  print(wcmatches %>% count(stage))
  
  list(
    wcmatches = wcmatches,
    worldcups = worldcups
  )
}


#' Count World Cup wins per country
#'
#' We count tournament wins rather than match wins because tournament wins
#' are the ultimate measure of dominance.
#'
#' @param cups worldcups tibble
#' @return Tibble with winner and titles columns, sorted descending
count_titles <- function(cups) {
  cups %>%
    count(winner, name = "titles") %>%
    arrange(desc(titles))
}

#' Plot World Cup titles by country
#'
#' We use a horizontal bar chart because country names are easier to read
#' on the y-axis. reorder() sorts countries by their number of titles.
#'
#' @param df Output of count_titles()
#' @return ggplot object
plot_titles <- function(df) {
  df %>%
    ggplot(aes(x = titles, y = reorder(winner, titles))) +
    geom_col(fill = "#457B9D") +
    labs(
      title = "Brazil leads all countries with five World Cup titles",
      x = "Number of titles",
      y = NULL,
      caption = "Source: TidyTuesday 2022-11-29"
    ) +
    theme_minimal()
}

join_match_context <- function(matches, cups) {
  matches %>%
    left_join(
      cups %>% select(year, host, winner, attendance),
      by = "year"
    )
}

#' Identify whether each match involves the host nation
#'
#' We flag host involvement at the match level so we can compare
#' host win rates against non-host win rates.
#'
#' @param df Joined match + tournament tibble
#' @return Tibble containing host matches and whether the host won
flag_host_matches <- function(df) {
  df %>%
    mutate(
      is_host_playing = (home_team == host | away_team == host),
      host_won = case_when(
        home_team == host & outcome == "H" ~ TRUE,
        away_team == host & outcome == "A" ~ TRUE,
        is_host_playing & outcome == "D" ~ NA,
        TRUE ~ FALSE
      )
    ) %>%
    filter(is_host_playing)
}

#' Compute goals per match for each tournament
#'
#' Raw goal totals aren't comparable across tournaments because the number
#' of matches has grown as the tournament expanded. Goals per match gives
#' us a fairer comparison between tournaments.
#'
#' @param cups worldcups tibble
#' @return Tibble with goals_per_match column added
compute_goals_per_match <- function(cups) {
  cups %>%
    mutate(goals_per_match = goals_scored / games)
}

#' Plot goals per match over time
#'
#' A line chart shows how scoring has changed across World Cup tournaments.
#' The 1990 World Cup is highlighted because its low scoring contributed
#' to later rule changes intended to encourage attacking play.
#'
#' @param df Output of compute_goals_per_match()
#' @return ggplot object
plot_goals_per_match <- function(df) {
  ggplot(df, aes(x = year, y = goals_per_match)) +
    geom_line() +
    geom_point() +
    geom_point(
      data = df %>% filter(year == 1990),
      size = 3
    ) +
    annotate(
      "text",
      x = 1990,
      y = df$goals_per_match[df$year == 1990] + 0.3,
      label = "1990 World Cup",
      hjust = 0.5
    ) +
    labs(
      title = "Goals per match at the World Cup over time",
      x = "Year",
      y = "Goals per match",
      caption = "Source: TidyTuesday 2022-11-29"
    ) +
    theme_minimal()
}

#' Create team-level match results
#'
#' Converts each match into one row per team so that a country's
#' performance can be compared when hosting and when not hosting.
#'
#' @param df Joined World Cup match and tournament data
#' @return Tibble with team, won, and is_host columns
create_team_matches <- function(df) {
  
  home <- df %>%
    transmute(
      year,
      team = home_team,
      host,
      won = outcome == "H",
      is_host = home_team == host
    )
  
  away <- df %>%
    transmute(
      year,
      team = away_team,
      host,
      won = outcome == "A",
      is_host = away_team == host
    )
  
  bind_rows(home, away)
}

#' Calculate host win rate by World Cup
#'
#' Calculates how often the host nation won its matches in each tournament
#' so we can examine whether the host advantage has changed over time.
#'
#' @param df Team-level World Cup match data
#' @return Tibble with host performance by tournament year
host_performance_by_year <- function(df) {
  df %>%
    filter(is_host) %>%
    group_by(year) %>%
    summarise(
      games = n(),
      wins = sum(won),
      win_rate = mean(won),
      .groups = "drop"
    )
}

#' Plot host win rate over time
#'
#' Shows how host nation performance has changed across World Cups.
#'
#' @param df Output of host_performance_by_year()
#' @return ggplot object
plot_host_advantage_over_time <- function(data) {
  ggplot(
    data,
    aes(x = year, y = win_rate)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1)
    ) +
    scale_x_continuous(
      breaks = seq(1930, 2022, by = 10)
    ) +
    labs(
      title = "World Cup Host Win Rate Over Time",
      subtitle = "Host performance has varied considerably across tournaments",
      x = "World Cup Year",
      y = "Host Win Rate",
      caption = "Source: TidyTuesday 2022-11-29"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = 12
      ),
      axis.title = element_text(
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(
        hjust = 1,
        size = 9
      )
    )
}

#' Plot host vs. non-host World Cup win rates
#'
#' @param host_summary Tibble containing host status and win rate
#' @return A ggplot comparing host and non-host win rates
plot_host_win_rates <- function(host_summary) {
  host_summary %>%
    mutate(
      host_status = if_else(is_host, "Host", "Non-Host")
    ) %>%
    ggplot(aes(x = host_status, y = win_rate, fill = host_status)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(
      aes(label = scales::percent(win_rate, accuracy = 0.1)),
      vjust = -0.5
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, max(host_summary$win_rate) * 1.15)
    ) +
    labs(
      title = "World Cup Win Rate: Hosts vs. Non-Hosts",
      subtitle = "Host nations have won a larger share of their matches",
      x = NULL,
      y = "Win Rate"
    ) +
    theme_minimal()
}