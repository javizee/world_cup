# Helper functions for the FIFA World Cup host-advantage analysis

normalize_country <- function(x) {
  dplyr::case_when(
    x %in% c("USA", "United States") ~ "United States",
    x %in% c("West Germany", "Germany") ~ "Germany",
    TRUE ~ x
  )
}

count_titles <- function(worldcups) {
  worldcups %>%
    dplyr::transmute(team = normalize_country(winner)) %>%
    dplyr::count(team, name = "titles", sort = TRUE)
}

plot_titles <- function(titles) {
  ggplot2::ggplot(
    titles,
    ggplot2::aes(x = stats::reorder(team, titles), y = titles)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "World Cup Titles by Country",
      x = NULL,
      y = "Titles"
    ) +
    ggplot2::theme_minimal()
}

join_match_context <- function(wcmatches, worldcups) {
  worldcups_small <- worldcups %>%
    dplyr::select(year, host)
  
  wcmatches %>%
    dplyr::left_join(worldcups_small, by = "year")
}

split_hosts <- function(host) {
  trimws(unlist(strsplit(host, ",")))
}

flag_host_matches <- function(wc_joined) {
  wc_joined %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      home_is_host = normalize_country(home_team) %in% normalize_country(split_hosts(host)),
      away_is_host = normalize_country(away_team) %in% normalize_country(split_hosts(host))
    ) %>%
    dplyr::ungroup()
}

create_team_matches <- function(wc_joined) {
  home <- wc_joined %>%
    dplyr::transmute(
      year,
      stage,
      team = normalize_country(home_team),
      opponent = normalize_country(away_team),
      is_host = vapply(
        seq_len(dplyr::n()),
        function(i) normalize_country(home_team[i]) %in% normalize_country(split_hosts(host[i])),
        logical(1)
      ),
      won = !is.na(winning_team) & normalize_country(winning_team) == normalize_country(home_team)
    )
  
  away <- wc_joined %>%
    dplyr::transmute(
      year,
      stage,
      team = normalize_country(away_team),
      opponent = normalize_country(home_team),
      is_host = vapply(
        seq_len(dplyr::n()),
        function(i) normalize_country(away_team[i]) %in% normalize_country(split_hosts(host[i])),
        logical(1)
      ),
      won = !is.na(winning_team) & normalize_country(winning_team) == normalize_country(away_team)
    )
  
  dplyr::bind_rows(home, away)
}

plot_host_win_rates <- function(host_summary) {
  host_summary %>%
    dplyr::mutate(host_status = dplyr::if_else(is_host, "Host", "Non-host")) %>%
    ggplot2::ggplot(ggplot2::aes(x = host_status, y = win_rate)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "World Cup Match Win Rate: Hosts vs. Non-hosts",
      x = NULL,
      y = "Win rate"
    ) +
    ggplot2::theme_minimal()
}

host_performance_by_year <- function(team_matches) {
  team_matches %>%
    dplyr::filter(is_host) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      games = dplyr::n(),
      wins = sum(won),
      win_rate = mean(won),
      .groups = "drop"
    )
}

plot_host_advantage_over_time <- function(host_by_year) {
  ggplot2::ggplot(host_by_year, ggplot2::aes(x = year, y = win_rate)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Host Win Rate Over Time",
      x = "World Cup year",
      y = "Host win rate"
    ) +
    ggplot2::theme_minimal()
}

# Convert match-stage labels into an ordered measure of tournament progress.
stage_level <- function(stage) {
  dplyr::case_when(
    grepl("Final", stage, ignore.case = TRUE) & !grepl("Quarter|Semi|Third", stage, ignore.case = TRUE) ~ 6,
    grepl("Third", stage, ignore.case = TRUE) ~ 5,
    grepl("Semi", stage, ignore.case = TRUE) ~ 5,
    grepl("Quarter", stage, ignore.case = TRUE) ~ 4,
    grepl("Round of 16|Round of Sixteen", stage, ignore.case = TRUE) ~ 3,
    grepl("Second group|Second Group", stage, ignore.case = TRUE) ~ 2,
    TRUE ~ 1
  )
}

stage_name <- function(level) {
  dplyr::case_when(
    level >= 6 ~ "Final",
    level == 5 ~ "Semifinal",
    level == 4 ~ "Quarterfinal",
    level == 3 ~ "Round of 16",
    level == 2 ~ "Second group stage",
    TRUE ~ "Group stage"
  )
}

summarize_tournament_progress <- function(team_matches) {
  team_matches %>%
    dplyr::mutate(stage_level = stage_level(stage)) %>%
    dplyr::group_by(year, team, is_host) %>%
    dplyr::summarise(
      furthest_stage_level = max(stage_level, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      furthest_stage = stage_name(furthest_stage_level),
      reached_quarterfinal = furthest_stage_level >= 4,
      reached_semifinal = furthest_stage_level >= 5,
      reached_final = furthest_stage_level >= 6
    )
}

summarize_host_advancement <- function(tournament_progress) {
  tournament_progress %>%
    dplyr::group_by(is_host) %>%
    dplyr::summarise(
      tournaments = dplyr::n(),
      quarterfinal_rate = mean(reached_quarterfinal),
      semifinal_rate = mean(reached_semifinal),
      final_rate = mean(reached_final),
      .groups = "drop"
    )
}

plot_host_advancement <- function(host_advancement) {
  host_advancement %>%
    dplyr::mutate(host_status = dplyr::if_else(is_host, "Host", "Non-host")) %>%
    tidyr::pivot_longer(
      cols = c(quarterfinal_rate, semifinal_rate, final_rate),
      names_to = "stage",
      values_to = "rate"
    ) %>%
    dplyr::mutate(
      stage = dplyr::recode(
        stage,
        quarterfinal_rate = "Quarterfinals",
        semifinal_rate = "Semifinals",
        final_rate = "Final"
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = stage, y = rate, fill = host_status)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "How Far Do Host Nations Advance?",
      x = NULL,
      y = "Share of tournament appearances",
      fill = NULL
    ) +
    ggplot2::theme_minimal()
}

compare_host_to_own_nonhost <- function(team_matches) {
  teams_that_hosted <- team_matches %>%
    dplyr::filter(is_host) %>%
    dplyr::distinct(team) %>%
    dplyr::pull(team)
  
  team_matches %>%
    dplyr::filter(team %in% teams_that_hosted) %>%
    dplyr::group_by(team, is_host) %>%
    dplyr::summarise(
      games = dplyr::n(),
      wins = sum(won),
      win_rate = mean(won),
      .groups = "drop"
    ) %>%
    dplyr::mutate(status = dplyr::if_else(is_host, "Hosting", "Not hosting")) %>%
    dplyr::select(team, status, games, wins, win_rate) %>%
    tidyr::pivot_wider(
      names_from = status,
      values_from = c(games, wins, win_rate),
      names_glue = "{.value}_{status}"
    ) %>%
    dplyr::filter(!is.na(win_rate_Hosting), !is.na(`win_rate_Not hosting`)) %>%
    dplyr::mutate(
      difference = win_rate_Hosting - `win_rate_Not hosting`
    ) %>%
    dplyr::arrange(dplyr::desc(difference))
}

plot_host_vs_own_nonhost <- function(host_self_comparison) {
  host_self_comparison %>%
    dplyr::select(team, win_rate_Hosting, `win_rate_Not hosting`) %>%
    tidyr::pivot_longer(
      cols = -team,
      names_to = "status",
      values_to = "win_rate"
    ) %>%
    dplyr::mutate(
      status = dplyr::recode(
        status,
        win_rate_Hosting = "Hosting",
        `win_rate_Not hosting` = "Not hosting"
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(team, win_rate),
        y = win_rate,
        group = team
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(shape = status), size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Host Countries Perform Better When They Host",
      x = NULL,
      y = "Match win rate",
      shape = NULL
    ) +
    ggplot2::theme_minimal()
}

