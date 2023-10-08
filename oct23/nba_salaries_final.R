library(tidyverse)
library(showtext)
# library(grid)
library(ggtext)


# read original data
nba_df <- read_csv("/Users/basch/Documents/R/R_projects/nba_salaries/nba_salaries.csv")

# data modification
nba_df_mod <- nba_df %>%
  rename(team = Team) %>%
  rename(sal = Salary) %>%
  rename(player_name = "Player Name") %>% 
  mutate(team_tidy = ifelse(
    str_detect(team, "/"),
    word(team, 1, sep = "/"),
    team
  )) %>%
  mutate(team_tidy_full = case_when(
    team_tidy == "GSW" ~ "Golden State Warriors",
    team_tidy == "LAC" ~ "LA Clippers",
    team_tidy == "LAL" ~ "Los Angeles Lakers",
    team_tidy == "BRK" ~ "Brooklyn Nets",
    team_tidy == "WAS" ~ "Washington Wizards",
    team_tidy == "POR" ~ "Portland Trail Blazers",
    team_tidy == "MIL" ~ "Milwaukee Bucks",
    team_tidy == "MIN" ~ "Minnesota Timberwolves",
    team_tidy == "MIA" ~ "Miami Heat",
    team_tidy == "PHI" ~ "Philadelphia 76ers",
    team_tidy == "DAL" ~ "Dallas Mavericks",
    team_tidy == "CHI" ~ "Chicago Bulls",
    team_tidy == "ATL" ~ "Atlanta Hawks",
    team_tidy == "TOR" ~ "Toronto Raptors",
    team_tidy == "IND" ~ "Indiana Pacers",
    team_tidy == "PHO" ~ "Phoenix Suns",
    team_tidy == "NOP" ~ "New Orleans Pelicans",
    team_tidy == "DEN" ~ "Denver Nuggets",
    team_tidy == "OKC" ~ "Oklahoma City Thunder",
    team_tidy == "CLE" ~ "Cleveland Cavaliers",
    team_tidy == "SAC" ~ "Sacramento Kings",
    team_tidy == "BOS" ~ "Boston Celtics",
    team_tidy == "CHO" ~ "Charlotte Hornets",
    team_tidy == "NYK" ~ "New York Knicks",
    team_tidy == "HOU" ~ "Houston Rockets",
    team_tidy == "MEM" ~ "Memphis Grizzlies",
    team_tidy == "ORL" ~ "Orlando Magic",
    team_tidy == "UTA" ~ "Utah Jazz",
    team_tidy == "SAS" ~ "San Antonio Spurs",
    team_tidy == "DET" ~ "Detroit Pistons"
  )) %>% 
  group_by(team_tidy_full) %>% 
  summarise(min_sal = min(sal),
            max_sal = max(sal),
            mean_sal = mean(sal))

# font settings
font_add_google("Varela Round", family = "varela")
font_add_google("Quicksand", family = "quicksand")
showtext_opts(dpi = 300)
showtext_auto()
font1 <- "varela"
font2 <- "quicksand"

# miscellaneous
caption_text <- "Chart: Algenib | Source: Hoopshype, Kaggle"

# plotting
nba_plot <- nba_df_mod %>% 
  ggplot() +
  geom_segment(data = nba_df_mod,
               mapping = aes(
                 x = min_sal,
                 xend = max_sal,
                 y = reorder(team_tidy_full, mean_sal),
                 yend = team_tidy_full),
               color = "#bfa6a2",
               alpha = 0.5,
               linewidth = 8) +
  geom_point(data = nba_df_mod,
             mapping = aes(
               x = min_sal,
               y = team_tidy_full),
               color = "#cc2229",
               shape = 16,
               size = 8) +
  geom_point(data = nba_df_mod,
             mapping = aes(
               x = mean_sal,
               y = team_tidy_full),
             color = "#57423f",
             shape = 16,
             size = 8) +
  geom_point(data = nba_df_mod,
             mapping = aes(
               x = max_sal,
               y = team_tidy_full),
             color = "#33348e",
             shape = 16,
             size = 8) +
  geom_text(data = nba_df_mod,
            mapping = aes(
              x = min_sal,
              y = team_tidy_full,
              label = ifelse(team_tidy_full == "LA Clippers", "minimum total salary", "")),
            hjust = 0,
            position = position_nudge(x = 800000),
            color = "#cc2229",
            fontface = "bold",
            family = font1,
            size = 10/.pt) +
  geom_text(data = nba_df_mod,
            mapping = aes(
              x = mean_sal,
              y = team_tidy_full,
              label = ifelse(team_tidy_full == "LA Clippers", "average total salary", "")),
            hjust = 0,
            position = position_nudge(x = 800000),
            color = "#57423f",
            fontface = "bold",
            family = font1,
            size = 10/.pt) +
  geom_text(data = nba_df_mod,
            mapping = aes(
              x = max_sal,
              y = team_tidy_full,
              label = ifelse(team_tidy_full == "LA Clippers", "maximum total salary", "")),
            hjust = 1,
            position = position_nudge(x = -800000),
            color = "#33348e",
            family = font1,
            size = 10/.pt) +
  scale_x_continuous(labels = scales::label_number(scale = 1/1000000, suffix = " million"), position = "top") +
  labs(
    title = "From (almost) Zero to Hero",
    subtitle = "NBA Player Total Salaries in US Dollars per Team (Season 2022/2023)",
    x = NULL,
    y = NULL,
    caption = caption_text) +
  theme_bw() +
  theme(
    text = element_text(family = my_font),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(60, 60, 30, 60),
    plot.title = element_text(family = font2, face="bold", size = 25, vjust = 5),
    plot.subtitle = element_text(family = font2, size = 20, vjust = 5)
    )

nba_plot


ggsave(filename = "nba_salaries.png",
       height = 10,
       width = 13.8,
       units = "in",
       dpi = 300)
