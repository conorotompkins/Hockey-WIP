library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

crosby <- read_csv("g vs xg.csv") %>%
        filter(Player == "SIDNEY.CROSBY") %>%
        select(Player, Date, TOI, ixG60, G60) %>%
        arrange(Player, Date) %>%
        mutate(game_number = dense_rank(Date), 
               Actual_Minus_Expected = G60 - ixG60)

crosby <- crosby %>%
        gather(., metric, measure, -Player, -Date, -game_number, -TOI)

crosby$metric[crosby$metric == "G60"] <- "Actual Goals"
crosby$metric[crosby$metric == "ixG60"] <- "Expected Goals"

crosby %>%
        filter(Player == "SIDNEY.CROSBY" & metric == "Actual Goals") %>%
        View()

crosby_graph1 <- crosby %>%
        filter(metric != "Actual_Minus_Expected") %>%
        ggplot(., aes(game_number, measure, color = metric, fill = metric)) +
        geom_point(size = .5, alpha = I(.2)) +
        geom_smooth(span = .2) +
        facet_wrap(~metric) +
        coord_cartesian(ylim = c(0, 7)) +
        theme_bw() +
        labs(x = "Game Number",
             y = "Per 60",
             title = "Sidney Crosby 2007-2016",
             subtitle = "5v5",
             caption = "@Null_HHockey, data from http://www.corsica.hockey") +
        theme(legend.position = "bottom",
              legend.title = element_blank())
ggsave("Crosby Actual vs Expected.png")
print(crosby_graph1)

crosby_graph2 <- crosby %>%
        filter(metric == "Actual_Minus_Expected") %>%
        ggplot(., aes(game_number, measure)) +
        geom_hline(yintercept = 0) +
        geom_point(size = .5, alpha = I(.2)) +
        geom_smooth(span = .2) +
        facet_wrap(~metric) +
        coord_cartesian(ylim = c(-5, 5)) +
        theme_bw() +
        labs(x = "Game Number",
             y = "Per 60",
             title = "Sidney Crosby 2007-2016",
             subtitle = "5v5",
             caption = "@Null_HHockey, data from http://www.corsica.hockey")
print(crosby_graph2)


ovechkin <- read_csv("g vs xg.csv") %>%
        filter(Player == "ALEX.OVECHKIN") %>%
        select(Player, Date, TOI, ixG60, G60) %>%
        arrange(Player, Date) %>%
        mutate(game_number = dense_rank(Date), 
               Actual_Minus_Expected = G60 - ixG60)

ovechkin <- ovechkin %>%
        gather(., metric, measure, -Player, -Date, -game_number, -TOI)

ovechkin$metric[ovechkin$metric == "G60"] <- "Actual Goals"
ovechkin$metric[ovechkin$metric == "ixG60"] <- "Expected Goals"
ovechkin_graph1 <- ovechkin %>%
        filter(metric != "Actual_Minus_Expected") %>%
        ggplot(., aes(game_number, measure, color = metric, fill = metric)) +
        geom_point(size = .5, alpha = I(.2)) +
        geom_smooth(span = .2) +
        facet_wrap(~metric) +
        coord_cartesian(ylim = c(0, 7)) +
        theme_bw() +
        labs(x = "Game Number",
             y = "Per 60",
             title = "Alexander Ovechkin 2007-2016",
             subtitle = "5v5",
             caption = "@Null_HHockey, data from http://www.corsica.hockey") +
        theme(legend.position = "bottom",
              legend.title = element_blank())
print(ovechkin_graph1)

ovechkin_graph2 <- ovechkin %>%
        filter(metric == "Actual_Minus_Expected") %>%
        ggplot(., aes(game_number, measure)) +
        geom_hline(yintercept = 0) +
        geom_point(size = .5, alpha = I(.2)) +
        geom_smooth(span = .2) +
        facet_wrap(~metric) +
        coord_cartesian(ylim = c(-5, 5)) +
        theme_bw() +
        labs(x = "Game Number",
             y = "Per 60",
             title = "Alex Ovechkin 2007-2016",
             subtitle = "5v5",
             caption = "@Null_HHockey, data from http://www.corsica.hockey")
print(ovechkin_graph2)

