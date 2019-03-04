#devtools::install_github('thomasp85/gganimate') 
pacman::p_load('tidyverse', 'rvest', 'pbapply', 'gganimate', 'gifski')

gameweek = 29

get_results <- function(week){
  url <- paste0('https://www.premierleague.com/matchweek/', week+3259, '/blog') #3259 for 2018/19, 1571 for last year
  page <- url %>%
    read_html()
  team1 <- page %>%
    html_nodes('.matchMinuteContainer+ .teamName abbr') %>%
    html_text()
  team2 <- page %>%
    html_nodes('.matchScoreContainer~ .teamName abbr') %>%
    html_text()
  score <- page %>%
    html_nodes('.matchScoreContainer .score') %>%
    html_text()
  score1 <- as.numeric(substr(score, 1,1))
  score2 <- as.numeric(substr(score, 3,3))
  df0 <- data.frame(week, team1, team2, score1, score2, 
             points1 = ifelse(score1 > score2, 3, ifelse(score1 < score2, 0, 1)),
             points2 = ifelse(score2 > score1, 3, ifelse(score2 < score1, 0, 1)))
  df1 <- df0 %>% select(week, 'team' = team1, 'goals.for' = score1, 'goals.against' = score2, 'points' = points1)
  df2 <- df0 %>% select(week, 'team' = team2, 'goals.for' = score2, 'goals.against' = score1, 'points' = points2)
  df <- bind_rows(df1, df2)
  df
}

results <- pblapply(FUN = get_results, X = 1:gameweek) %>% bind_rows()

#takes into account double gameweeks: adds total goals per gameweek
results.grouped <- results %>% 
  group_by(week, team) %>%
  summarise(goals.for = sum(goals.for),
            goals.against = sum(goals.against),
            points = sum(points))

#make a dataset with 1 row for each club and gw
teamweeks <- data.frame(team = rep(unique(results$team), each = gameweek),
                        week = rep(unique(results$week), 20))
results.complete <- teamweeks %>%
  left_join(results.grouped)

#replaces NAs with zeros for where a club didn't play
results.complete[is.na(results.complete)] <- 0


#finally, get the cumulative points totals
results.sum <- results.complete %>%
  mutate(cumpoints = ave(points, team, FUN = cumsum),
         cumfor = as.numeric(ave(goals.for, team, FUN = cumsum)),
         cumagainst = as.numeric(ave(goals.against, team, FUN = cumsum)),
         gd = cumfor - cumagainst) %>%
  group_by(week) %>%
  arrange(desc(cumpoints), desc(gd)) %>%
  mutate(ordering = 20:1) %>%
  ungroup() %>%
  left_join(data.frame(team =  sort(unique(results.complete$team)),
                       colour = c('#EF0107',  '#0057B8', '#B50E12', '#6C1D45', '#0070B5',
                                  '#034694', '#1B458F', '#003399', '#000000', '#0E63AD',
                                  '#003090', '#C8102E', '#6CABDD', '#DA291C', '#241F20',
                                  '#D71920', '#132257', '#FBEE23', '#7A263A', '#FDB913')
                       # colour = c('#EF0107',  '#0057B8', '#B50E12', '#6C1D45', '#034694', #the 17-18 colour schemes
                       #            '#1B458F', '#003399', '#0E63AD', '#003090', '#C8102E',
                       #            '#6CABDD', '#DA291C', '#241F20', '#D71920', '#E03A3E',
                       #            '#121212', '#132257', '#FBEE23', '#122F67', '#7A263A')
                       ))


top_teams <- results.sum %>%
  filter(ordering > 0) #used if you don't want to show all the positions at once


chart <- top_teams %>%
  ggplot(aes(x = ordering, group = team)) + 
  geom_tile(aes(y = cumpoints/2,
                height = cumpoints,
                width = 0.8,
                fill = team),
            alpha = ifelse(top_teams$ordering > 16, 1, 1), #could toggle different transparency for clubs outside of top 4
    show.legend = F) +
  geom_vline(xintercept = 16.5, linetype = 'dashed', colour = '#222222', size = .3) +
  geom_vline(xintercept = 3.5, linetype = 'dashed', colour = '#222222', size = .3) +
  scale_fill_manual(values = as.character(unique((top_teams %>% arrange(team))$colour))) +
  geom_text(aes(y = 0, label = team), hjust = 0, size = 5.7, color = 'white') +
  geom_text(aes(y = cumpoints+1, label = as.character(cumpoints)), hjust = 0, size = 5.7) +
  theme(panel.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'Gill Sans', face = 'plain', size = 26,
                                  margin = ggplot2::margin(b = 7)),
        plot.subtitle = element_text(family = 'Gill Sans', size = 18,
                                     margin = ggplot2::margin(9,0,9,0), colour = '#444444')) +
  coord_cartesian(clip = "off", expand = F) +
  coord_flip() + 
  labs(title = 'How City edged ahead',
       subtitle = 'Gameweek {closest_state}') + 
  transition_states(week, transition_length = 5, state_length = 1 )  +
  ease_aes('cubic-in-out')  


animate(chart, nframes = 174, height = 600, width = 450)
anim_save('~/Downloads/premier_league_1819.gif')
  