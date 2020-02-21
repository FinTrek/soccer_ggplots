pacman::p_load(tidyverse, polite, scales, ggimage, rvest, 
               glue, extrafont, showtext, ggrepel, magick, 
               ggforce, lubridate, cowplot, patchwork, rlang)
loadfonts(quiet = TRUE)


add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}


tategaki <- function(x){
  x <- chartr("ー", "丨", x) # 長音符の処理
  x <- strsplit(split="", x)
  sapply(x, paste, collapse="\n")
}

tategaki_alt <- function(x){
  x <- stringr::str_replace_all(x, "ー", "丨") # 長音符の処理
  stringr::str_wrap(x, width = 1)
}



create_age_utility_plot <- function(df, color_1, color_2, j_name) {
  
  rect_df <- data.frame(
    xmin = 25, xmax = 29,
    ymin = 0, ymax = 1
  )
  max <- max(df$age, na.rm = TRUE)
  lims <- c(16, (max(df$age, na.rm = TRUE) + 2))
  breaks <- seq(from = 18, to = max, by = 2)
  
  plot <- df %>% 
    ggplot(aes(x = age_now, y = min_perc)) +
    geom_vline(xintercept = 25, alpha = 0.4, color = "grey20") +
    geom_hline(yintercept = 0.5, alpha = 0.4, color = "grey20") +
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
      nudge_y = 0.01, 
      seed = 6) + 
    annotate("text", x = 27, y = 1.055,
             size = 5.5, fontface = "bold",
             family = "Yu Mincho Demibold",
             label = "ピーク年齢", color = "grey20") +
    scale_y_continuous(
      expand = c(0.01, 0),
      limits = c(0, 1.08), 
      breaks = seq(0, 1, by = 0.2), 
      labels = percent_format()) +
    scale_x_continuous(
      limits = lims,
      breaks = breaks) +
    labs(
      x = "年齢", 
      y = tategaki_alt("出場時間 (%)"),  
      title = paste(j_name, ": 年齢-出場時間"), #{df$team_name}
      subtitle = "J-League 2019 シーズン (100% = 3060分)") +
    theme_bw() +
    theme(
      text = element_text(family = "Yu Mincho Demibold"),
      panel.border = element_blank(),
      plot.title = element_text(color = color_1, size = 20, face = "bold"),
      plot.subtitle = element_text(color = color_2, size = 18),
      plot.caption = element_text(size = 14),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(angle = 0, vjust= 0.5),
      axis.title = element_text(size = 16),
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
               y = 0.77, yend = 0.65,
               curvature = -0.5, size = 1.2,
               arrow = arrow(length = unit(0.1, "inches")),
               lineend = "round") +
    ## bottom arrow
    geom_segment(x = 23, xend = 23,
                 y = 0.48, yend = 0.63,
                 size = 1.2,
                 arrow = arrow(length = unit(0.1, "inches")),
                 lineend = "round") +
    annotate("text", x = 30, y = 0.83, family = "Yu Mincho Demibold",
             label = "年齢", size = 4.5) +
    annotate("text", x = 23, y = 0.4, family = "Yu Mincho Demibold",
             label = "年齢 （入団時）", size = 4.5) +
    scale_y_continuous(
      limits = c(0, 1.05), 
      breaks = seq(0, 1, by = 0.2),
      labels = percent_format()) +
    scale_x_continuous(
      limits = c(18, 33),
      breaks = seq(18, 34, by = 2)) +
    labs(caption = glue("
                   データ: transfermarkt.com
                   作: @R_by_Ryo")) +
    theme_bw() +
    theme(
      text = element_text(family = "Yu Mincho Demibold"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 14),
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
             size = 4,
             label = glue::glue("選手: {summary_df$sum_players}人"),
             family = "Yu Mincho Demibold") +
    annotate("text", x = 25, y = 0.65, 
             size = 4,
             label = glue::glue("平均年齢: {summary_df$median_age}歳"),
             family = "Yu Mincho Demibold") +
    annotate("text", x = 25, y = 0.55, 
             size = 4,
             label = glue::glue("平均滞在期間: {summary_df$median_duration}年"),
             family = "Yu Mincho Demibold") +
    scale_y_continuous(limits = c(0.3, 0.9)) +
    theme_void()
  
  return(summary_stats)
}


age_utility_plotter <- function(df, team_name, color_1, color_2, j_name, img_path) {
  
  team_name <- enquo(team_name)
  
  df <- df %>% 
    filter(team_name == !!team_name)
  
  age_utility_plot <- df %>% 
    create_age_utility_plot(color_1 = color_1, color_2 = color_2, j_name = j_name)
  
  plot_leg <- df %>% 
    create_age_utility_legend(color_1 = color_1, color_2 = color_2)
  
  plot_sum <- df %>% 
    create_summary_stats_plot()
  
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
  
  layout <- "
AAAAD#
AAAAD#
AAAAC#
AAAA##
AAAAB#
"
  
  combined_plot <- age_utility_plot + plot_leg + plot_sum + logo_plot +
    plot_layout(design = layout)
  
  return(combined_plot)
}













