## PLOT

create_age_utility_plot <- function(df, color_1, color_2, j_name) {
  
  rect_df <- data.frame(
    xmin = 25, xmax = 29,
    ymin = 0, ymax = 1
  )
  max <- max(df$age, na.rm = TRUE)
  lims <- c((min(df$join_age, na.rm = TRUE) - 1), (max(df$age, na.rm = TRUE) + 1))
  breaks <- seq(from = 18, to = max, by = 2)
  
  plot <- df %>% 
    ggplot(aes(x = age_now, y = min_perc)) +
    geom_rect(data = rect_df,
              aes(x = NULL, y = NULL,
                  xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax),
              fill = color_1, alpha = 0.4) +
    geom_link(aes(x = join_age, xend = age_now,
                  y = min_perc, yend = min_perc,
                  alpha = stat(index)), 
              color = color_2, size = 1.75) + 
    geom_point(aes(x = age_now), color = color_2, size = 3.5) +
    geom_text_repel(
      aes(label = player),
      nudge_y = 0.01, fontface = "bold",
      seed = 6, color = "grey20") + 
    annotate("text", x = 27, y = 1.055,
             size = 5.5, fontface = "bold",
             family = "Roboto Condensed",
             label = "Peak Age", color = "grey20") +
    scale_y_continuous(
      expand = c(0.01, 0),
      limits = c(0, 1.08), 
      breaks = seq(0, 1, by = 0.2), 
      labels = percent_format()) +
    scale_x_continuous(
      limits = lims,
      breaks = breaks) +
    labs(
      x = "Current Age (As of March 1, 2020)", 
      y = "Minutes Played (%)",  
      title = glue::glue("<b style ='color:{color_2}'>{j_name}</b> <b style ='color:grey20'>Squad Age Profile</b>"), #{df$team_name}
      subtitle = glue::glue("<b style ='color:grey20'>La Liga 2019-2020: Matchday 25</b>")) +
    theme_bw() +
    theme(
      text = element_text(family = "Roboto Condensed"),
      panel.border = element_blank(),
      plot.title = element_markdown(size = 20, face = "bold"),
      plot.subtitle = element_markdown(size = 18),
      plot.caption = element_text(size = 14),
      panel.grid.minor = element_blank(),
      # axis.title.y = element_text(angle = 0, 
      #                             vjust= 0.5),
      axis.title = element_text(size = 16, color = "grey20"),
      axis.text = element_text(size = 14),
      axis.ticks = element_blank(),
      legend.position = "none")
  
  return(plot)
}


create_age_utility_legend <- function(df, color_1, color_2){
  
  rect_df <- data.frame(
    xmin = 25, xmax = 29,
    ymin = 0, ymax = 1
  )
  
  leg_df <- tibble(
    x = 23, xend = 28,
    y = 0.65, yend = 0.65
  )
  
  legend_plot <- df %>% 
    ggplot(aes(x = age_now, y = min_perc)) +
    geom_rect(data = rect_df,
              aes(x = NULL, y = NULL,
                  xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax),
              fill = color_1, alpha = 0.4) +
    geom_link(data = leg_df,
              aes(x = x, xend = xend,
                  y = y, yend = yend,
                  alpha = stat(index)), 
              color = color_2, size = 1.75) + 
    geom_point(x = 28, y = 0.65, color = color_2, size = 3.5) +
    ## top arrow
    geom_curve(x = 30, xend = 28.4,
               y = 0.84, yend = 0.65,
               curvature = -0.5, size = 1.2,
               arrow = arrow(length = unit(0.1, "inches")),
               lineend = "round") +
    ## bottom arrow
    geom_segment(x = 23, xend = 23,
                 y = 0.48, yend = 0.63,
                 size = 1.2,
                 arrow = arrow(length = unit(0.1, "inches")),
                 lineend = "round") +
    annotate("text", x = 30, y = 0.90, family = "Roboto Condensed",
             label = "Current Age", size = 3, color = "grey20") +
    annotate("text", x = 23, y = 0.4, family = "Roboto Condensed",
             label = "Age at Time of Joining Club", size = 3, color = "grey20") +
    scale_y_continuous(
      limits = c(0, 1.05), 
      breaks = seq(0, 1, by = 0.2),
      labels = percent_format()) +
    scale_x_continuous(
      limits = c(18, 33),
      breaks = seq(18, 34, by = 2)) +
    labs(caption = glue("
                   Data: transfermarkt.com
                   Graphic: Ryo Nakagawara (Twitter: @R_by_Ryo)")) +
    theme_bw() +
    theme(
      text = element_text(family = "Roboto Condensed"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 14, color = "grey20"),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      legend.position = "none")
  
  return(legend_plot)
}


create_summary_stats_plot <- function(df) {
  summary_df <- df %>% 
    mutate(duration = age_now - join_age) %>% 
    summarize(median_age = median(age),
              median_duration = round(median(duration, na.rm = TRUE), digits = 2),
              sum_players = n())
  
  summary_stats <- summary_df %>%
    ggplot(aes(x = age_now, y = min_perc)) +
    annotate("text", x = 25, y = 0.75, 
             size = 4, color = "grey20",
             label = glue::glue("Number of Players: {summary_df$sum_players}"),
             family = "Roboto Condensed") +
    annotate("text", x = 25, y = 0.65, 
             size = 4, color = "grey20",
             label = glue::glue("Average Age (Median): {summary_df$median_age}"),
             family = "Roboto Condensed") +
    annotate("text", x = 25, y = 0.55, 
             size = 4, color = "grey20",
             label = glue::glue("Average Duration of Stay: {summary_df$median_duration} Years"),
             family = "Roboto Condensed") +
    scale_y_continuous(limits = c(0.3, 0.9)) +
    theme_void()
  
  return(summary_stats)
}


age_utility_plotter <- function(df, team_name, color_1, color_2, j_name, img_path) {
  
  team_name <- enquo(team_name)
  
  df <- df %>% 
    filter(team_name == !!team_name)
  
  ## main age-utility plot
  age_utility_plot <- df %>% 
    create_age_utility_plot(color_1 = color_1, color_2 = color_2, j_name = j_name)
  
  ## mini legend plot
  plot_leg <- df %>% 
    create_age_utility_legend(color_1 = color_1, color_2 = color_2)
  
  ## stats summary
  plot_sum <- df %>% 
    create_summary_stats_plot()
  
  ## team logo "plot"
  img <- png::readPNG(source = img_path)
  rasterimg <- grid::rasterGrob(img, interpolate = TRUE)
  
  img_df <- data.frame(x = seq(1, 10, 1),
                       y = seq(1, 10, 1))
  
  logo_plot <- ggplot(img_df, aes(x = x, y = y)) +
    geom_point(color = "white") +
    annotation_custom(rasterimg, xmin = 2, xmax = 8, 
                      ymin = 2, ymax = 8) +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, 10)) +
    theme_void()
  
  ## set up patchwork layout
  layout <- "
AAAAD#
AAAAD#
AAAAC#
AAAA##
AAAAB#
"
  
  ## combine with patchwork syntax
  combined_plot <- age_utility_plot + plot_leg + plot_sum + logo_plot +
    plot_layout(design = layout)
  
  return(combined_plot)
}


