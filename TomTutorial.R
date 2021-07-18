install.packages("devtools")
devtools::install_github('bbplot')

library(ggplot2)
library(devtools)
library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(gt) # beautiful tables
library(DT) # beautiful interactive tables
library(ggthemes) # custom pre-built themes
library(bbplot) # more themes
library(ggtext) # custom text color
library(teamcolors) # NFL team colors and logos
library(ggforce) # better annotations
library(ggridges) # many distributions at once
library(ggrepel) # better labels
library(ggbeeswarm) # beeswarm plots
library(extrafont)
library(ggimage)


pbp <- read_csv("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.csv.gz")

early_pass <- pbp%>%
  filter( wp > .20 & wp < .80 & down <= 2 & qtr <= 2 & half_seconds_remaining > 120)%>%
  group_by(posteam)%>%
  summarize(mean_pass = mean(pass), plays = n(), mean_epa = mean(epa))%>%
  arrange(-mean_pass)
  
ggplot(early_pass , aes(x=reorder(posteam, -mean_pass), y= mean_pass))+
  geom_text_repel(aes(label = posteam))+
  theme_bw()



##########################  WR DUEL ###################################################

pbp <- pbp%>%
  mutate( receiver_player_name =case_when(
    receiver_player_name == "A.Brown" & posteam == "TEN" ~ "AJ.Brown",
    receiver_player_name == "A.Brown" & posteam == "TB"  ~ "An.Brown",
    TRUE ~ receiver_player_name
  ))

wr_duel <- pbp %>%
  filter(posteam %in% c("TEN"), receiver_player_name %in% c("AJ.Brown", "C.Davis", "J.Smith")
  )%>%
  group_by(week, receiver_player_name, posteam)%>%
  summarize(mean_epa = mean(epa, na.rm = TRUE), mean_xyac = mean(xyac_epa, na.rm = TRUE),
            mean_yac_epa = mean(yac_epa, na.rm = TRUE), air_yardss = sum(air_yards, na.rm = TRUE), targets=n())

############# SEASON TITANS WRS EPA ################################



ten_colors <-  nflfastR::teams_colors_logos %>%
  filter(team_abbr == "TEN")

ten_primary <-  pull(ten_colors, team_color)
ten_secondary <-  pull(ten_colors, team_color3)
ten_third <-  pull(ten_colors, team_color2)


wr_duel_plot <- ggplot(
  wr_duel,
  aes(
    x = week, y = mean_epa,
    color = receiver_player_name
  )
) +
  geom_line(size = 1) +
  #theme_538() +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  labs(
    x = "",
    y = "EPA (Average)",
    title = "Comparison of Titans Receivers (2020)",
    caption = "Data: @nflfastR | Plot: @_ThomasTsegaye"
  ) +
  scale_color_manual(values = c(ten_primary, ten_secondary, ten_third)) +
  scale_x_continuous(breaks = seq(1, 17, 1)) +
  scale_y_continuous(breaks = seq(-1, 2, 0.5)) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

wr_duel_plot +
  theme(legend.position = "none")+
  geom_text(data = filter(wr_duel, week ==1),
            aes(x= week, y= mean_epa, label = receiver_player_name),
            hjust = 0, nudge_x = 0.2, size = 4, fontface = "bold")+
  geom_point(data = filter(wr_duel, week >=1),
             size = 3)


##########################  Bar Charts ############################################

rb_trio <- pbp %>%
  filter(
    posteam == "BAL",
    receiver %in% c("M.Ingram", "M.Ingram II", "G.Edwards", "J.Hill") |
      rusher %in%  c("M.Ingram", "M.Ingram II", "G.Edwards", "J.Hill"),
    play_type %in% c("run", "pass")
  ) %>%
  mutate(
    # Assign a single player name for filtering regardless of play_type
    player = if_else(is.na(receiver), rusher, receiver),
    player = if_else(str_detect(player, "Ingram"), "M.Ingram", player),
    player = factor(player, levels = c("M.Ingram", "G.Edwards", "J.Hill")),
    # Add nice labels to play_type
    play_type = factor(play_type, labels = c("Reception", "Rush"))
  ) %>%
  group_by(player, play_type) %>%
  summarize(
    n = n(),
    mean_yards = sum(yards_gained, na.rm = TRUE) / n,
    mean_success = sum(success, na.rm = TRUE) / n
  )
  


Season_Titans_WR <- pbp%>%
  filter(posteam == "TEN" & play_type == "pass" & down <=4)%>%
  group_by(receiver_player_name)%>%
  summarize(sum_epa = sum(epa, na.rm = TRUE), targets = n())%>%
  filter(targets>40)
  

bar_TitanSzn <- Season_Titans_WR %>%
  pivot_longer(c("sum_epa", "targets"), names_to = "category", values_to = "stat")


wr_trio_plot <- bar_TitanSzn%>%
ggplot(aes(x=receiver_player_name, y= stat,
                       fill = receiver_player_name, position ="dodge", group = category))+
  geom_col()+
  facet_grid(~category)+
  scale_y_continuous(breaks = seq(0,60,10))+
  labs(x="Receiver",
       y="Value",
       title = "Titans Receivers EPA and Targets up to Week 11",
       caption = "Data:nflfastR Plot:@_ThomasTsegaye")


TEN_colors <- nflfastR::teams_colors_logos %>% 
  filter(team_abbr == "TEN")  

wr_trio_plot +
  geom_hline(yintercept = 0.3, color = "black", size =2)+
  scale_fill_manual(values = c(TEN_colors$team_color, TEN_colors$team_color2, TEN_colors$team_color3))+
  labs(x = "",
       y = "Value",
       title = "Titans Top Receivers EPA and Targets(2020)",
       subtitle = "Corey Davis has highest total EPA through week 11",
       caption = "Data: @nflfastR | Plot: @_ThomasTsegaye")+
  theme(legend.position = "none")  







################################ WFT RB Comparison #############################################

WFT_trio <- pbp %>%
  filter(
    posteam == "WAS",
    receiver %in% c("J.McKissic", "A.Gibson", "P.Barber") |
      rusher %in%  c("J.McKissic", "A.Gibson", "P.Barber"),
    play_type %in% c("run", "pass")
  ) %>%
  mutate(
    # Assign a single player name for filtering regardless of play_type
    player = if_else(is.na(receiver), rusher, receiver),
    player = factor(player, levels = c("J.McKissic", "A.Gibson", "P.Barber")),
    # Add nice labels to play_type
    play_type = factor(play_type, labels = c("Reception", "Rush"))
  ) %>%
  group_by(player, play_type) %>%
  summarize(
    n = n(),
    mean_yards = sum(yards_gained, na.rm = TRUE) / n,
    mean_epa = mean(epa, na.rm = TRUE)
  )%>%
  ungroup()


########   get colors for WFT     #########################################
WFT_colors <- nflfastR::teams_colors_logos%>%
  filter(team_abbr == "WAS")


ggplot(WFT_trio, aes(x=player, y= mean_yards, fill = player, group = play_type, position = "dodge"))+
  geom_col()+
  facet_grid(~play_type)+
  scale_fill_manual(values = c(WFT_colors$team_color, WFT_colors$team_color2, WFT_colors$team_color3))+
  labs(x="",
       y="Average Yards",
       title = "WFT Running Back Comparison by Rush and Reception Yards(2020)",
       subtitle = "P.Barber Only has 3 Receptions",
       caption = "Data:nflfastR, plot:@_ThomasTsegaye")+
  theme(legend.position = "none")

###################### WFT RB EPA ###########################################
ggplot(WFT_trio, aes(x=player, y=mean_epa, fill = player, group = play_type, position = "dodge"))+
  geom_col()+
  facet_grid(~play_type)+
  scale_fill_manual(values = c(WFT_colors$team_color, WFT_colors$team_color2, WFT_colors$team_color3))+
  labs(x="",
       y="Average EPA",
       title = "WFT Running Back Comparison by Rush and Reception EPA(2020)",
       subtitle = "P.Barber only has 3 receptions",
       caption = "Data:nflfastR, plot:@_ThomasTsegaye")+
  theme(legend.position = "none")





############################## EPA PER PASS AND RUN BAR ################################
#flippedx and y bar graph
epa_perdb <- pbp %>%
  filter(pass==1, !is.na(posteam))%>%
  group_by(posteam)%>%
  summarize(n=n(), epa_per_pass = sum(epa, na.rm = TRUE)/n)


pbp%>%
  filter(pass==1, !is.na(posteam))%>%
  group_by(posteam)%>%
  summarize(n=n(), epa_per_pass = sum(epa, na.rm = TRUE)/n)%>%
  ungroup()%>%
  ggplot(aes(x = epa_per_pass, y = reorder(posteam, epa_per_pass))) +
  geom_col(aes(fill = if_else(epa_per_pass >= 0, "#2c7bb6", "#d7181c"))) +
  geom_text(aes(
    label = posteam,
    color = if_else(epa_per_pass >= 0, "#2c7bb6", "#d7181c"),
    hjust = if_else(epa_per_pass > 0, -0.1, 1.1)
  ),
  fontface = "bold"
  ) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  #theme_538() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  labs(
    x = "",
    y = "EPA per Dropback",
    title = "The majority of teams had positive EPA/dropback",
    subtitle = "But there are some clear outliers",
    caption = "Data: @nflfastR | Plot: @thomas_mock"
  )
  

########### use images instead of bars #############
asp.ratio <- 1/asp.ratio
geom_image(aes(image = team_logo_wikipedia), size = 0.035, by = "width", asp = asp_ratio)  

epa_db_logos <- epa_perdb %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))%>%
  ggplot(aes(x= epa_per_pass, y = reorder(posteam, epa_per_pass)))+
  geom_image(aes(image = team_logo_wikipedia), size = 0.035, by ="width", asp = 18/9)+
  scale_fill_identity(aesthetics = c ("fill", "colour"))+
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(-0.15, 0.4, 0.1))+
  labs(x = "",
       y= "EPA per Pass",
       title = "2020 EPA per Pass",
       caption = "Data:nflfastR, plot: @_ThomasTsegaye")
epa_db_logos  
  


pect_pass <- pbp%>%
  filter(wp>=.30 & wp<=.60 & down <=2 & half_seconds_remaining >120 & qtr <4)%>%
  group_by(posteam)%>%
  summarize(n =n(), pct_passes = mean(pass, na.rm = TRUE), total_passes = sum(pass))%>%
  ungroup()%>%
  arrange(-pct_passes)
  

pect_pass <- pect_pass %>%
  left_join(epa_perdb, by = c("posteam" = "posteam"))
  
pect_pass <- pect_pass %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))

ggplot(pect_pass, aes(epa_per_pass, pct_passes))+
  geom_image(aes(image = team_logo_wikipedia), size =0.04, asp = 19/9)+
  geom_smooth(method = lm, se=FALSE, linetype ="dashed", color = "red", alpha=0.7)+
  labs(x= "EPA Per Pass",
       y= "Average Number of Pass Plays",
       title = "2020 EPA per Pass vs Average Number of Pass Plays",
       subtitle = "Wp bwtween 30 & 60, early downns, 1-3 quarters, and more than 2 minutes in half",
       caption = "Data:nflfastR, plot: @_ThomasTsegaye")

epa_early_pass <- pbp%>%
  filter(wp>=.30 & wp<=.60 & down <=2 & half_seconds_remaining >120 & play_type%in%c("run", "pass"))%>%
  group_by(posteam, play_type)%>%
  ungroup()%>%
  summarize(n =n(), pct_passes = mean(pass, na.rm = TRUE), total_passes = sum(pass), 
            mean_epa = mean(epa))

qbs <- pbp %>%
  filter(play_type %in% c("pass", "run"), penalty == 0, !is.na(epa))%>%
  group_by(name, posteam)%>%
  summarize(n_dropbacks = sum(pass), n_rush = sum(rush), n_plays = n(),
            epa_play = sum(epa)/ n_plays, success_rate = sum(success)/n_plays)%>%
  filter(n_dropbacks > 27)
qbs <- qbs%>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))
  

# highlight name and dot
ggplot(qbs, aes(x = success_rate, y = epa_play))+
  geom_point(aes(color = if_else(posteam == "TEN", "blue", "black"), 
                 size = n_plays/30), alpha = 0.5)+
  scale_color_identity()+
  geom_text_repel(data = filter(qbs, posteam == "TEN"),
                  aes(label = name), color = "blue",
                  force = 1, point.padding = 0.1, segment.size = 0.2)+
  geom_hline(yintercept = mean(qbs$epa_play), linetype = "dashed", color = "red")+
  geom_vline(xintercept = mean(qbs$success_rate), linetype = "dashed", color = "red")+
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black", alpha= 0.5)+
  labs(x = "Success Rate",
      y = "EPA per Play",
      title = "Success Rate vs EPA Per Play",
      caption = "Data:nflfastR, plot: @_ThomasTsegaye")+
  theme(legend.position = "none")




#############################################################################################


########################  Efficiency by Position  #######################################

rosters <- read_csv("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.csv") %>%
  filter(teamPlayers.position %in% c("QB", "WR", "RB", "FB", "TE"), team.season >= 2019) %>% 
  select(position = teamPlayers.position, receiver_jersey_number = teamPlayers.jerseyNumber, posteam = team.abbr)

data_clean <- pbp %>%
  filter(pass ==1 & sack == 0 & qb_scramble ==0, !is.na(receiver_jersey_number))%>%
  select(name, pass, desc, posteam, epa, defteam, complete_pass, incomplete_pass,
         air_yards, receiver_player_name, receiver_jersey_number, down, success, complete_pass
  ) %>%
  left_join(rosters, by = c("receiver_jersey_number", "posteam"))%>%
  filter(!is.na(position))%>%
  mutate(position = if_else(position == "FB", "RB", position))

pos <- data_clean %>%
  filter(position != "QB")%>%
  mutate(position = factor(position, levels = c("WR", "RB", "TE")))

pos %>%
  filter(posteam == "TEN" & between(air_yards, 1, 25)) %>%
  group_by(position, air_yards) %>% 
  mutate(n = n(),
         mean = mean(epa)) %>% 
  ungroup() %>% 
  ggplot(aes(x = air_yards, y = epa, fill = position)) +
  #group points by air yards and show all passes,  make dots black
  geom_point(aes(group = air_yards), shape = 21, alpha = 0.3, fill = "black") +
  #avg/regression points, size varies by n
  geom_point(aes(size = n, x = air_yards, y = mean), shape = 21, stroke = 0.5, color = "white", alpha = 0.8) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  stat_smooth(color = "white", method = "loess", alpha = 0.5) +
  facet_grid(~position) +
  coord_cartesian(ylim = c(-1.5, 5)) +
  scale_y_continuous(breaks = seq(-1.5, 4.5, by = 0.5)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) +
  scale_fill_manual(
    values = c("#00b159", "#003399", "#ff2b4f"),
    aesthetics = c("color", "fill")
  ) +
  labs(
    x = "Air Yards (Depth of Target)",
    y = "EPA\n",
    title = "Titans Pass EPA by  Depth of Throw a& Position since 2010",
    subtitle = "Titans don't throw much to Rb's, especially at longer distances",
    caption = "Data: @nflfastR | plotdesign: @thomas_mock"
  )



pass_comp <-pos%>%
  filter(between(air_yards, 1, 25))%>%
  group_by(position, air_yards)%>%
  summarize(n= n(), comp_rate = sum(complete_pass, na.rm = TRUE) / n, epa = mean(epa, na.rm = TRUE))

pass_compl_plot <- pass_comp%>%
  ggplot(aes(x= air_yards, y= comp_rate, fill = position))+
  geom_point(aes(size = n), shape = 21, stroke = 0.5)+
  geom_smooth(color = "white", method = "loess")+
  geom_hline(yintercept = .50, linetype = "dashed")+
  geom_vline(xintercept = 20, linetype = "dashed")+
  facet_grid(~position)+
  ggthemes::theme_fivethirtyeight()+
  scale_fill_manual(
    values = c("#00b159", "#003399", "#ff2b4f"),
    aesthetics = c("color", "fill")
  )+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Air Yards",
       y="Completion Rate",
       title = "Completion Rate vs Air Yards",
       caption = "Data:nflfastR, plotdesign: @thomas_mock")+
  guides(color= FALSE, fill = FALSE)+  #get rid of color & fill in legend of the wR RB TE labels
  theme(legend.direction = "vertical",
    legend.position = c(0.05, 0.2),
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"))
pass_compl_plot


#################################################################################################################

###################################  Density Plots  ##################################################

Sea_colors <-  nflfastR::teams_colors_logos%>%
  filter(team_abbr == "SEA")%>%
  pull(team_color2)

TEN_colorss <- nflfastR::teams_colors_logos %>%
  filter(team_abbr == "TEN")%>%
  pull(team_color2)

BAL_colors <- nflfastR::teams_colors_logos%>%
  filter(team_abbr == "BAL")%>%
  pull(team_color2)

KC_colors <- nflfastR::teams_colors_logos%>%
  filter(team_abbr == "KC")%>%
  pull(team_color2)


pbp %>%
  filter(play_type %in% c("run", "pass"), posteam %in% c("SEA", "TEN")) %>%
  group_by(posteam, play_type) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

pbp %>%
  filter(play_type == "pass", !is.na(air_yards)) %>%
  filter(posteam %in% c("SEA", "TEN")) %>%
  ggplot(aes(x = air_yards, fill = posteam)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c(TEN_colorss, Sea_colors)) +
  ggthemes::theme_fivethirtyeight()+
  guides(
    fill = guide_legend(
      label = TRUE, title = "", label.position = "left",
      direction = "vertical",
      label.theme = element_text(size = 20)
    )
  ) +
  theme(legend.position = c(0.5, 0.9)) +
  scale_x_continuous(breaks = seq(-10, 60, 10))


souce_url <- "https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv"

pass_map_df <- read_csv(souce_url) %>%
  na.omit() %>%
  select(x_coord)


pass_maps_df <- read.csv("https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv")
glimpse(pass_map_df)


pass_maps_df %>%
  filter(season == 2019 | season == 2020, str_detect(name, c("Tannehill")))%>%
  ggplot(aes(x= x, y = y)) +
  geom_density_2d_filled(
    aes(fill = ..level..),
    contour_var = "ndensity",
    breaks = seq(0.1, 1.0, length.out = 10)
    )+
  scale_y_continuous(breaks = seq(-10, 60, 5))+
  facet_wrap(~season)+
  geom_hline(yintercept = 5)



################################################################################################################

############################  Combining Tables WR vs TEAM PASS PCT ##############################################

aaa <- pbp%>%
  filter(season == 2020, play_type %in% c("run", "pass"), down<=4)%>%
  group_by(posteam, play_type)%>%
  summarize(n= n())%>%
  mutate(freq = n/sum(n))

  
aaa <- aaa %>%
  pivot_wider(names_from = play_type, values_from = freq)

aaa$run <- NULL
  
aab <- pbp %>%
  filter(season == 2020 & !is.na(receiver_player_name) & play_type == "pass" & down <= 4) %>%
  group_by(receiver_player_name, posteam) %>%
  summarize(targets = n(), aDOT = mean(air_yards, na.rm = TRUE), air_yards = sum(air_yards, na.rm = TRUE)) %>%
  filter(targets >= 35)


#merge tables with different number of rows by column
aac <- merge(aaa, aab, by = "posteam")

aac %>%
  filter(targets > 40 & air_yards > 100)%>%
  ggplot(aes(x = targets, y = pass))+
  geom_point(aes(size = air_yards, color = if_else(posteam == "TEN" | posteam == "SEA", "blue", "black")),alpha = 0.4)+
  scale_color_identity()+
  geom_text_repel(data =
                  filter(aac, targets > 40, posteam == "TEN" | posteam == "SEA"),
                  aes(label = receiver_player_name),color = "blue", point.padding = 0.1)+
  ggthemes::theme_fivethirtyeight()+
  guides(color= FALSE, fill= FALSE)+
  theme(legend.direction = "vertical",
        legend.position = c(0.83, 0.2),
        legend.background = element_blank(),
        legend.title = element_text(face = "bold"))+
  labs(x="Targets",
       y="Team Pass Percentage",
       title = "Comparing Pass Catchers Targets vs Team Pass Percentage",
       subtitle = "Titans have a low pass percentage, not giving their WRs a chance to shine; \n\ Targets>40 & Air Yards >50",
       caption = "Data:nflfastR, plot: @_ThomasT")







################################################################################################################

###############################  Distribution Plots: Tians vs SEA Pass per Air yards###########################

source_url <- read_csv("https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv")

pass_map_df <- read_csv(source_url)%>%
  na.omit()%>%
  select(-X1)#select all columns except X1 which is an idex

glimpse(pass_map_df)

pass_map_df %>%
  filter(str_detect(name, c("Mahomes|Tannehill")))%>%
  ggplot(aes(x = x_coord, y = y_coord))+
  geom_density_2d_filled(
    aes(fill = ..level..),
    contour_var = "ndensity",
    breaks = seq(0.1, 1.0, length.out =10)
  )+
  scale_y_continuous(breaks = seq(-10, 60, 5))+
  facet_wrap(~name)+
  geom_hline(yintercept = 5)

pass_maps_df %>%
  filter(str_detect(name, c("Deshaun Watson|Tannehill")))%>%
  ggplot(aes(x = x, y = y))+
  geom_density_2d_filled(
    aes(fill = ..level..),
    contour_var = "ndensity",
    breaks = seq(0.1, 1.0, length.out =10)
  )+
  scale_y_continuous(breaks = seq(-10, 60, 5))+
  facet_wrap(~name)+
  geom_hline(yintercept = 5)



pass_map_df %>%
  filter(str_detect(name, "Russell Wilson|Ryan Tannehill" ), season == 2019)%>%
  ggplot(aes(x = x_coord, y = y_coord))+
  geom_hex(binwidth = c(1,1))+
  scale_fill_gradient(low = "red", high = "yellow")+
  geom_hline(yintercept = c(3,7), color ="green")+
  facet_grid(~name)+
  scale_y_continuous(breaks = seq(-10,60,5))



pass_map_df%>%
  filter(season == 2019 & name == "Patrick Mahomes")%>%
  ggplot(aes(x=y_coord))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = c(12), color = "blue")+
  scale_x_continuous(breaks = seq(-8, 52, 2))


# find how many times Tanny threw by each yard bin (how many he threw 5 yds, 7yds, etc) , y_coord >=1
tpass_by_y <-  pass_map_df%>%
  filter( season == 2019)%>%
  group_by(name)%>%
  mutate(y_rnd = round(y_coord, digits = 0))%>%  #create y_rnd column that rounds each y 
  count(y_rnd)%>%  #get number of times a ycoord shows up (each time QB threw that far)
  mutate(
    total = sum(n),
    pct_total = n/total, 
    roll_total = cumsum(pct_total)
  )


tpass_by_y%>%
  filter(between(y_rnd, 12, 16))%>%
  summarize(pct = sum(pct_total))



pass_map_df%>%
  filter(str_detect(name, c("Lamar Jackson|Watson")))%>%
  ggplot(aes(x= x_coord, y = y_coord))+
  geom_density_2d_filled(
    aes(fill = ..level..),
    contour_var = "ndensity",  #normalize to each QBs total passes
    breaks = seq(0.1, 1, length.out = 10)  #drop lowest passes
  )+
  facet_wrap(~name)+
  geom_hline(yintercept = 5)
  #scale_y_continuous(breaks = seq(-10, 60, 5))

##################### Rank qbs by deep pass perct and throws ####
Tanny_Pass <- tpass_by_y%>%
  mutate(name = case_when(
    name == "John Stafford" ~ "Matthew Stafford",
    name == "Elisha Manning" ~ "Eli Manning",
    name == "Rayne Prescott" ~ "Dak Prescott",
    TRUE ~ name
  ))%>%
  filter(y_rnd >=10 & total >=100)%>%
  summarize(pct= sum(pct_total), throws = sum(n))%>%
  arrange(-pct)
  
ggplot(Tanny_Pass, aes(x = throws, y= pct))+
  geom_point()+
  geom_text_repel(aes(label = name))+
  geom_smooth(method = "lm", se= FALSE, linetype = "solid", color = "grey")+
  geom_hline(yintercept = mean(Tanny_Pass$pct), linetype = "dashed", color = "red")+
  geom_vline(xintercept = mean(Tanny_Pass$throws), linetype= "dashed", color ="red")+
  scale_x_continuous(breaks = seq(30, 300, 30))+
  scale_y_continuous(breaks =seq(.2,.5, .05))+
  labs(x="Number of Throws 10 yds or Deeper",
       y= "Pct of Passes 10 yds or Deeper",
       title = "2019 Qbs Number of Throws vs Percent of Throws 10 Yards or Deeper",
       caption = "Data:nflfastR, plot:@_ThomasT")










############################# Tanny CPOE EPA By Location #####################################################

zzzzzz <- pbp%>%
  filter(passer == "R.Tannehill", play_type == "pass")

Right_EPA <- pbp%>%
  filter(passer == "R.Tannehill", down <=4, play_type == "pass", penalty == 0, !is.na(pass_location))%>%
  group_by(passer, pass_location)%>%
  summarize( n=n(),total_pass= sum(n, na.rm = TRUE),epa = mean(epa, na.rm = TRUE), 
             cpoe= mean(cpoe, na.rm = TRUE), air_yards = mean(air_yards, na.rm = TRUE),
             cp = mean(cp, na.rm = TRUE))%>%
  pivot_longer(c("epa", "cpoe"), names_to = "category", values_to = "value")


ggplot(Right_EPA, aes(x = pass_location, y= value))+
  geom_col(aes(fill = category), position = "dodge")+
  labs(x="Pass Location",
       y="Mean Value",
       title = "Tannehill Pass EPA & CPOE by Location",
       subtitle = "Through Week 17 of 2020",
       caption = "Data:nflfastR, plot:@_ThomasT")
















########################## Pass VS RUSH EPA ###############################################################

################   BY SEASON #################
a111 <- pbp%>%
filter(posteam == "TEN", down <= 4, play_type %in% c("pass", "run"))%>%
  group_by(play_type)%>%
  summarize(epa = mean(epa, na.rm = TRUE))

ggplot(a111, aes(x= play_type, y= epa))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,0.3, 0.05))+
  labs(x= "",
       y= "EPA per Play",
       title = "Average EPA per Play by Play Type Through Week 12(2020)",
       caption = "Data:nflfastR, plot: @_ThomasT")



###########    By WEEK  ########################
a2 <- pbp%>%
  filter(posteam == "TEN", play_type %in% c("pass", "run"), down <= 4, !is.na(week), !is.na(play_type))%>%
  group_by(week, play_type)%>%
  summarize(epa = mean(epa))
  #pivot_wider(names_from = play_type, values_from =epa)%>%
  



Pass_Rush <- ggplot(a2, aes(x= week, y= epa, color = play_type))+
  geom_line()+
  geom_point(size=2)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(1,17,1))+
  scale_y_continuous(breaks = seq(-.54, .8, .1))+
  ggthemes::theme_fivethirtyeight()+
  labs(x= "Week",
       y= "Mean EPA",
       title = "Titans Pass vs Rush EPA Through Week 17(2020)",
       subtitle = "Had higher pass epa on 8 out of 11 wins",
       caption = "Data:nflfastR, plot:@_ThomasT")+
  theme(
      legend.title = element_blank(),
      legend.position = "top"
    )    
    
Pass_Rush +
  geom_text(data = filter(a2, week %in% c(1,2,3,5,6,9,11,12,14,15,17)),
            aes(x = week, y = epa, label = if_else(play_type == "pass","Win", "")),
            hjust = 0, nudge_x = 0.1, size = 4, fontface = "bold")+
  geom_text(data = filter(a2, week %in% c(7,8,10,13,16)),
            aes(x = week, y = epa, label = if_else(play_type == "pass","Loss", "")),
            hjust = 0, vjust = -1 ,nudge_x = 0.1, size = 4, fontface = "bold")






  
########################################### BUILD THE FIELD #############################################
not_div_5 <- function(x) {
  # select only elements of the vector not divisible by 5
  x[x %% 5 != 0]
}

center_df <- tibble(
  x_coord = c(rep(-3.1, 60), rep(3.1, 60)),
  y_coord = seq(-14, 59, 1) %>% rep(2) %>% not_div_5(),
  text = "--"
)

# line labels
annotate_df <- tibble(
  x_coord = c(12.88, -12.88) %>% rep(each = 5),
  y_coord = seq(10, 50, 10) %>% rep(2),
  text = seq(10, 50, 10) %>% rep(2) %>% str_replace("(.)(.)", "\\1 \\2"),
  rotation = c(90, 270) %>% rep(each = 5)
)

# yardlines
yardline_df <- tibble(
  y = seq(-15, 60, 5),
  yend = seq(-15, 60, 5),
  x = rep(-56 / 2, 16),
  xend = rep(56 / 2, 16)
)

# sidelines
sideline_df <- tibble(
  y = c(-15.15, -15.15),
  yend = c(60.15, 60.15),
  x = c(-56 / 2, 56 / 2),
  xend = c(-56 / 2, 56 / 2)
)


#####     Wrap the field in a function #################
ggplot(data = NULL, aes(x = x_coord, y = y_coord)) +
  coord_cartesian(
    xlim = c(-53.333 / 2, 53.333 / 2),
    ylim = c(-15, 60)
  ) +
  geom_text(
    data = annotate_df, aes(label = text, angle = rotation),
    color = "black", size = 8
  ) +
  geom_segment(
    data = yardline_df, color = "black", size = 1,
    aes(x = x, y = y, xend = xend, yend = yend)
  ) +
  geom_segment(
    x = -56 / 2, y = 0, xend = 56 / 2, yend = 0,
    color = "blue", size = 1, alpha = 0.5
  ) +
  geom_segment(
    data = sideline_df, color = "black", size = 2,
    aes(x = x, y = y, xend = xend, yend = yend)
  ) +
  geom_text(
    data = center_df,
    aes(label = text), color = "black", vjust = 0.32
  ) +
  theme_void()







add_field <- function() {
  list(
    coord_cartesian(
      xlim = c(-53.333 / 2, 53.333 / 2),
      ylim = c(-15, 60)
    ),
    geom_text(
      data = annotate_df, aes(label = text, angle = rotation),
      color = front_col, size = 8
    ),
    geom_segment(
      data = yardline_df, color = front_col, size = 1,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_segment(
      x = -56 / 2, y = 0, xend = 56 / 2, yend = 0,
      color = "blue", size = 1, alpha = 0.5
    ),
    geom_segment(
      data = sideline_df, color = front_col, size = 2,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_text(
      data = center_df,
      aes(label = text), color = front_col, vjust = 0.32
    ),
    theme_void(),
    theme(
      strip.text = element_text(size = 20, color = front_col),
      plot.background = element_rect(fill = back_col, color = NA),
      legend.position = "none",
      plot.margin = unit(c(2, 1, 0.5, 1), unit = "cm"),
      plot.caption = element_text(color = front_col),
      plot.title = element_text(color = front_col),
      plot.subtitle = element_text(color = front_col),
      panel.background = element_rect(fill = back_col, color = NA),
      panel.border = element_blank()
    )
  )
}





passer_graph <- pass_map_df%>%
  filter(str_detect(name, c("Mariota|Tannehill")))%>%
  select(name, x_coord, y_coord)%>%
  ggplot(aes(x = x_coord, y= y_coord))+
  geom_density_2d_filled(
    aes(fill = ..level.., color = ..level..),
    contour_var = "ndensity",  #normalize across facets
    breaks = seq(0.1, 1.0, length.out = 10)
  )+
  theme(legend.position = "none")+
facet_grid(~name)

heat_colors <- grDevices::colorRampPalette(c("#800026FF", "#FC4E2AFF", "#FEB24CFF", "#FFFFCCFF"))(10)

heat_palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1)

heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1))(10)

heat_colors %>% scales::show_col()

back_col <- "black"
front_col <- "white"

passer_graph +
  add_field()+
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color"))

  
    




###############################   QB COMPARISON ON 1 FIELD ###################################################

qb_density_compare <- function(pass_df, qb1_name, qb2_name, n = 200){
  
  # filter to qb1
  qb1 <- pass_df %>% 
    select(x, y, name) %>% 
    filter(str_detect(name, qb1_name))
  
  #filter to qb2
  qb2 <- pass_df %>% 
    select(x, y, name) %>% 
    filter(str_detect(name, qb2_name))
  
  # get x/y coords as vectors
  qb1_x <- pull(qb1, x)
  qb1_y <- pull(qb1, y)
  
  # get x/y coords as vectors
  qb2_x <- pull(qb2, x)
  qb2_y <- pull(qb2, y)
  
  # get x and y range to compute comparisons across
  x_rng = range(c(qb1_x, qb2_x))
  y_rng = range(c(qb1_y, qb2_y))
  
  # Explicitly calculate bandwidth for future use
  bandwidth_x <- MASS::bandwidth.nrd(c(qb1_x, qb2_x))
  bandwidth_y <- MASS::bandwidth.nrd(c(qb1_y, qb2_y))
  
  bandwidth_calc <- c(bandwidth_x, bandwidth_y)
  
  # Calculate the 2d density estimate over the common range
  d2_qb1 = MASS::kde2d(qb1_x, qb1_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  d2_qb2 = MASS::kde2d(qb2_x, qb2_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  
  # create diff df
  qb_diff <- d2_qb1
  
  # matrix subtraction density from qb2 from qb1
  qb_diff$z <- d2_qb1$z - d2_qb2$z
  
  # add matrix col names
  colnames(qb_diff$z) = qb_diff$y
  
  #### return tidy tibble ####
  qb_diff$z %>% 
    # each col_name is actually the y_coord from the matrix
    as_tibble() %>% 
    # add back the x_coord
    mutate(x= qb_diff$x) %>% 
    pivot_longer(-x, names_to = "y", values_to = "z") %>% 
    mutate(y_coord = as.double(y),
           bandwidth = list(bandwidth_calc),
           comparison = glue::glue("{qb1_name} (QB1) vs {qb2_name} (QB2)"))
  
}



compared_z <- qb_density_compare(pass_maps_df, "Tannehill", "Mahomes", n = 200) 

(compared_plot <- compared_z %>% 
    ggplot(aes(x, y)) +
    
    # add core heatmap - note that geom_raster or geom_tile both work
    geom_raster(aes(x, y, fill=z))  +
    
    # add contour polygon lines around the most dense points
    stat_contour(aes(color=..level.., z = z)) +
    
    # add a fill gradient from low (blue) to high (red) 
    # with white as the zero midpoint
    scale_fill_gradient2(low="blue",mid="white", high="red", midpoint=0) +
    scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0) +
    # drop the legends
    guides(color=FALSE, fill = FALSE) +
    add_moo_field() +
    labs(title = unique(compared_z$comparison),
         subtitle = "Color is more passes by <span style='color:red'>**QB1**</span> or by <span style='color:blue'>**QB2**</span>",
         caption = "Plot: @thomas_mock | Data: @ChiefsAnalytics")) +
  # add some customizations to the plot
  theme(legend.position = "top", legend.key.width = unit(2, "cm"),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5),
        plot.caption = element_text(face = "bold")) 
  



install.packages("ggExtra")
install.packages("patchwork")
install.packages("paletteer")
install.packages("hexbin")
library(hexbin)
library(ggExtra)   # marginal plots   
library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
#library(scales) 









############################## #########################################################################

home <- pbp%>%
  filter(season_type == "REG")%>%
  select(season, team, home_team, result)%>%
  rename(team=home_team)



#############    MOO FIELD    ##############################
not_div_5 <- function(x) {
  # select only elements of the vector not divisible by 5
  x[x %% 5 != 0]
}

center_df <- tibble(
  x_coord = c(rep(-3.1, 60), rep(3.1, 60)),
  y_coord = seq(-14, 59, 1) %>% rep(2) %>% not_div_5(),
  text = "--"
)

# line labels
horiz_yd_df <- tibble(
  x_coord = c(12.88, -12.88) %>% rep(each = 14),
  y_coord = seq(-10, 55, 5) %>% rep(2),
  text = seq(-10, 55, 5) %>% rep(2)
)

# yardlines
yardline_df <- tibble(
  y = seq(-15, 60, 5),
  yend = seq(-15, 60, 5),
  x = rep(-56 / 2, 16),
  xend = rep(56 / 2, 16)
)

# sidelines
sideline_df <- tibble(
  y = c(-15.15, -15.15),
  yend = c(60.15, 60.15),
  x = c(-56.5 / 2, 56.5 / 2),
  xend = c(-56.5 / 2, 56.5 / 2)
)

add_moo_field <- function() {
  list(
    coord_cartesian(
      xlim = c(-53.333/2, 53.333/2),
      ylim = c(-15, 60)
    ),
    geom_segment(
      data = yardline_df, color = front_col, size = 0.5,
      linetype = "dashed", alpha = 0.5,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_segment(
      aes(x = -56 / 2, y = 0, xend = 56 / 2, yend = 0),
      color = "blue", size = 1
    ),
    geom_segment(
      data = sideline_df, color = front_col, size = 2,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_text(
      data = center_df,
      aes(label = text), color = front_col, vjust = 0.32
    ),
    geom_text(
      data = horiz_yd_df, aes(label = text),
      color = front_col, size = 4, fontface = "bold"
    ),
    theme_void(),
    theme(
      strip.text = element_text(size = 20, color = front_col),
      plot.background = element_rect(fill = back_col, color = NA),
      legend.position = "none",
      plot.margin = unit(c(2, 1, 0.5, 1), unit = "cm"),
      plot.caption = element_text(color = front_col),
      plot.title = element_text(color = front_col),
      plot.subtitle = element_text(color = front_col),
      panel.background = element_rect(fill = back_col, color = NA),
      panel.border = element_blank()
    )
  )
}

back_col <- "white"
front_col <- "black"


ggplot(pass_map_df, aes(x = x_coord, y = y_coord)) +
  geom_density_2d_filled(
    aes(fill = ..level..),
    contour_var = "ndensity", # normalize to each QBs total passes
    breaks = seq(0.1, 1.0, length.out = 10) # drop the lowest passes
  ) +
  add_moo_field()