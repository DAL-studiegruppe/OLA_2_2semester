library(StatsBombR)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(SBpitch)

# Opgave 3.1
##################

# comb <- FreeCompetitions()
# FComp <- FreeCompetitions() %>% 
#   filter(competition_id %in% c(37), season_id %in% c(90))

# Matches <- FreeMatches(FComp)
# StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = TRUE)
# StatsBombData <- allclean(StatsBombData)
# saveRDS(StatsBombData, file = "StatsBombData.rds")
StatsBombData <- readRDS("StatsBombData.rds")

# find 100 kamper for mand premier league
# FComp_men <- FreeCompetitions() %>% 
#   filter(competition_id %in% c(2), season_id %in% c(27))  

# Matches_men <- FreeMatches(FComp_men)
# StatsBombData_men <- free_allevents(MatchesDF = Matches_men, Parallel = TRUE)
# StatsBombData_men <- allclean(StatsBombData_men)
# saveRDS(StatsBombData_men, file = "StatsBombData_men.rds")
StatsBombData_men <- readRDS("StatsBombData_men.rds")

# find 100 kamper for kvinde
kvinde100 <- StatsBombData %>%
  distinct(match_id) %>%  
  arrange(match_id) %>%
  slice(1:100) %>%
  left_join(StatsBombData, by = "match_id")

mand100 <- StatsBombData_men %>%
  distinct(match_id) %>%  
  arrange(match_id) %>%
  slice(1:100) %>%
  left_join(StatsBombData_men, by = "match_id")


# HOVEDTAL
{
# Alle
længde_K <- mean(kvinde100$pass.length, na.rm = TRUE)
længde_M <- mean(mand100$pass.length, na.rm = TRUE)

# Målmænd
mean(kvinde100$pass.length[kvinde100$position.name == "Goalkeeper"], na.rm = TRUE)
mean(mand100$pass.length[mand100$position.name == "Goalkeeper"], na.rm = TRUE)

# Uden målmænd
mean(kvinde100$pass.length[kvinde100$position.name != "Goalkeeper"], na.rm = TRUE)
mean(mand100$pass.length[mand100$position.name != "Goalkeeper"], na.rm = TRUE)

# pct. mål - KVINDER
antal_mål <- sum(kvinde100$shot.outcome.name %in% c("Goal"))
antal_skud <- sum(!is.na(kvinde100$shot.outcome.name))
pct_mål <- antal_mål / antal_skud * 100

# pct. mål - MÆND
antal_mål_M <- sum(mand100$shot.outcome.name %in% c("Goal"))
antal_skud_M <- sum(!is.na(mand100$shot.outcome.name))
pct_mål_M <- antal_mål_M / antal_skud_M * 100

# Skud længde
skud_K <- kvinde100 %>%
  filter(!is.na(shot.outcome.name)) %>% 
  select(location.x,
         location.y)

skud_K <- skud_K %>% 
  mutate(
    distance_to_goal = sqrt((location.x - 120)^2 + (location.y - 40)^2)
  )

mean(skud_K$distance_to_goal)

skud_M <- mand100 %>%
  filter(!is.na(shot.outcome.name)) %>% 
  select(location.x,
         location.y)

skud_M <- skud_M %>% 
  mutate(
    distance_to_goal = sqrt((location.x - 120)^2 + (location.y - 40)^2)
  )

mean(skud_M$distance_to_goal)
}

# Lav en optælling af type.name
type_counts_C <- table(kvinde100$type.name)
type_counts_C <- as.data.frame(type_counts_C)

type_counts_A <- table(mand100$type.name)
type_counts_A <- as.data.frame(type_counts_A)


ggplot(type_counts_C, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Fordeling af event typer i Chelsea-kampe",
       x = "Event Type",
       y = "Antal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# AFLEVERINGER OG MISSEDE SKUD
{
  
kvinde_P <- kvinde100 %>% 
  group_by(match_id) %>% 
  summarise(
    fejl_afleveringer = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside"), na.rm = TRUE) + 
      sum(pass.height.name == TRUE, na.rm = TRUE),
    misset_skud = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE),
    .groups = "drop"
  )
  
mand_P <- mand100 %>% 
  group_by(match_id) %>% 
  summarise(
    fejl_afleveringer = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside"), na.rm = TRUE) + 
      sum(pass.height.name == TRUE, na.rm = TRUE),
    misset_skud = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE),
    .groups = "drop"
   )
  
# pct for alle kamper
mand_Pp <- mand100 %>% 
    group_by(match_id) %>% 
    summarise(
    afleverings_pct = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside")) / sum(!is.na(pass.height.name)) * 100,
    misset_skud_pct = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE) / 
      sum(!is.na(shot.outcome.name)) * 100
  )

kvinde_Pp <- kvinde100 %>% 
  group_by(match_id) %>% 
  summarise(
    afleverings_pct = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside")) / sum(!is.na(pass.height.name)) * 100,
    misset_skud_pct = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE) / 
      sum(!is.na(shot.outcome.name)) * 100
  )

# pct til en samled df
mand_Pp <- mand_Pp %>%
  mutate(gender = "Mænd") %>%
  select(gender, everything(), -match_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE), gender = "Mænd")

kvinde_Pp <- kvinde_Pp %>%
  mutate(gender = "Kvinder") %>%
  select(gender, everything(), -match_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE), gender = "Kvinder")

Combined_Stats <- bind_rows(mand_Pp, kvinde_Pp)

# plot til pct
Combined_Stats_Long <- Combined_Stats %>%
  pivot_longer(cols = c(afleverings_pct, misset_skud_pct),
               names_to = "handling", values_to = "pct") %>%
  filter(!is.na(pct))

ggplot(Combined_Stats_Long, aes(x = handling, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = round(pct, 2)),  
            position = position_dodge(width = 0.9),
            vjust = -0.2,
            size = 5) +
  labs(title = "Kvinder laver flere fejl afleveringer, mens mænd derimod misser flere skud",
       x = "Handling",
       y = "Procent",
       fill = "Køn") +
  scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
  scale_x_discrete(labels = c(
    "afleverings_pct" = "Fejl aflevering",
    "misset_skud_pct" = "Misset skud"
  )) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# FRISPARK
{
  kvinde_V_foul <- kvinde100 %>% 
    group_by(match_id) %>% 
    summarise(
      foul_committed_count = sum(type.name == "Foul Committed", na.rm = TRUE),
      .groups = "drop"
    )
  
  mand_V_foul <- mand100 %>% 
    group_by(match_id) %>% 
    summarise(
      foul_committed_count = sum(type.name == "Foul Committed", na.rm = TRUE),
      .groups = "drop"
    )
  
  # pct til en samled df
  kvinde_V_foul <- kvinde_V_foul %>%
    mutate(gender = "Kvinder") %>%
    select(gender, everything())
  
  mand_V_foul <- mand_V_foul %>%
    mutate(gender = "Mænd") %>%
    select(gender, everything())  
  
  Combined_Stats_foul <- bind_rows(kvinde_V_foul, mand_V_foul)
  
  Combined_Stats_foul <- Combined_Stats_foul %>%
    group_by(gender) %>%
    summarise(foul_committed_count = sum(foul_committed_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct = round((foul_committed_count / sum(foul_committed_count)) * 100, 2))
  
  ggplot(Combined_Stats_foul, aes(x = gender, y = pct, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, show.legend = FALSE) +
    theme_minimal() +
    labs(
      title = "Mænd begår oftere frispark",
      x = "Køn",
      y = "Procent"
    ) +
    scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
    theme(
      axis.text.x = element_text(hjust = 1),
      plot.title = element_text(hjust = 0.5, lineheight = 1.2),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    ) +
    geom_text(aes(label = round(pct, 1)), 
              position = position_dodge(width = 0.7),
              vjust = -0.5)
}

# FRISPARK - HEATMAP
{
# Hvor på banen begås der frispark for mænd og kvinder
kvinde_frispark <- kvinde100 %>%
  filter(type.name == "Foul Committed") %>% 
  select(location.x,
         location.y)

mand_frispark <- mand100 %>%
  filter(type.name == "Foul Committed") %>% 
  select(location.x,
         location.y)
  
create_Pitch(
  grass_colour = "black",
  background_colour = "gray15",
  line_colour = "gray40"
) +
  # Add heatmap layer
  stat_density_2d(
    data = kvinde_frispark,
    aes(x = location.x, y = location.y, fill = ..density..),
    geom = "tile",
    contour = FALSE,
    alpha = 0.7
  ) +
  # Customize the color scheme
  scale_fill_gradient(
    low = "white",
    high = "blue"
  ) +
  # Add title
  labs(
    title = "Her begår kvinderne frispark",
    x = "Længde på banen",  # Ændrer x-akse label
    y = "Bredde på banen"   # Ændrer y-akse label
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 120), ylim = c(0, 85)) +
  theme(legend.position = "none")  # Fjerner legend

# Heatmap for mænd
create_Pitch(
  grass_colour = "black",
  background_colour = "gray15",
  line_colour = "gray40"
) +
  # Add heatmap layer
  stat_density_2d(
    data = mand_frispark,
    aes(x = location.x, y = location.y, fill = ..density..),
    geom = "tile",
    contour = FALSE,
    alpha = 0.7
  ) +
  # Customize the color scheme
  scale_fill_gradient(
    low = "white",
    high = "blue"
  ) +
  # Add title
  labs(
    title = "Her begår mændene frispark",
    x = "Længde på banen",  # Ændrer x-akse label
    y = "Bredde på banen"   # Ændrer y-akse label
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 120), ylim = c(0, 85)) +
  theme(legend.position = "none")  # Fjerner legend
}

# KORT
{
# Røde kort kontra gule kort
# kvinder
bad_behaviour_K <- kvinde100 %>% 
  group_by(match_id) %>% 
  summarise(
    red_bad = sum(bad_behaviour.card.name %in% c("Red Card"), na.rm = TRUE),
    yellow_bad = sum(bad_behaviour.card.name %in% c("Yellow Card"), na.rm = TRUE),
    second_yellow_bad = sum(bad_behaviour.card.name %in% c("Second Yellow"), na.rm = TRUE),
    red_foul = sum(foul_committed.card.name %in% c("Red Card"), na.rm = TRUE),
    yellow_foul = sum(foul_committed.card.name %in% c("Yellow Card"), na.rm = TRUE),
    second_yellow_foul = sum(foul_committed.card.name %in% c("Second Yellow"), na.rm = TRUE)
  )

# mænd
bad_behaviour_M <- mand100 %>% 
  group_by(match_id) %>% 
  summarise(
    red_bad = sum(bad_behaviour.card.name %in% c("Red Card"), na.rm = TRUE),
    yellow_bad = sum(bad_behaviour.card.name %in% c("Yellow Card"), na.rm = TRUE),
    second_yellow_bad = sum(bad_behaviour.card.name %in% c("Second Yellow"), na.rm = TRUE),      
    red_foul = sum(foul_committed.card.name %in% c("Red Card"), na.rm = TRUE),
    yellow_foul = sum(foul_committed.card.name %in% c("Yellow Card"), na.rm = TRUE),
    second_yellow_foul = sum(foul_committed.card.name %in% c("Second Yellow"), na.rm = TRUE)
  )
  
# Kvinder pct
bad_behaviour_K_pct <- kvinde100 %>% 
  group_by(match_id) %>% 
  summarise(
    red_foul_pct = sum(type.name == "Foul Committed" & foul_committed.card.name == "Red Card", na.rm = TRUE) / 
      sum(type.name == "Foul Committed", na.rm = TRUE) * 100,
    yellow_foul_pct = sum(type.name == "Foul Committed" & foul_committed.card.name == "Yellow Card", na.rm = TRUE) / 
      sum(type.name == "Foul Committed", na.rm = TRUE) * 100,
    second_yellow_foul_pct = sum(type.name == "Foul Committed" & foul_committed.card.name == "Second Yellow", na.rm = TRUE) / 
      sum(type.name == "Foul Committed", na.rm = TRUE) * 100
  )

# Mænd pct
bad_behaviour_M_pct <- mand100 %>% 
  group_by(match_id) %>% 
  summarise(
    red_foul_pct = sum(type.name == "Foul Committed" & foul_committed.card.name == "Red Card", na.rm = TRUE) / 
      sum(type.name == "Foul Committed", na.rm = TRUE) * 100,
    yellow_foul_pct = sum(type.name == "Foul Committed" & foul_committed.card.name == "Yellow Card", na.rm = TRUE) / 
      sum(type.name == "Foul Committed", na.rm = TRUE) * 100,
    second_yellow_foul_pct = sum(type.name == "Foul Committed" & foul_committed.card.name == "Second Yellow", na.rm = TRUE) / 
      sum(type.name == "Foul Committed", na.rm = TRUE) * 100
  )

# Lav samlet DF for kort
bad_behaviour_KK <- bad_behaviour_K_pct %>%
  mutate(gender = "Kvinder") %>%
  select(gender, everything(), -match_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE), gender = "Kvinder")

bad_behaviour_MM <- bad_behaviour_M_pct %>%
  mutate(gender = "Mænd") %>%
  select(gender, everything(), -match_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE), gender = "Mænd")

bad_behaviour_combined <- bind_rows(bad_behaviour_KK, bad_behaviour_MM)

bad_behaviour_combined_long <- bad_behaviour_combined %>%
  pivot_longer(cols = c(red_foul_pct, yellow_foul_pct, second_yellow_foul_pct),
               names_to = "kort_type", values_to = "pct") %>%
  filter(!is.na(pct))
  
ggplot(bad_behaviour_combined_long, aes(x = kort_type, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = round(pct, 2)),  
            position = position_dodge(width = 0.9),
            vjust = -0.2,
            size = 5) +
  labs(title = "Mænd er grundliggende hårdere i deres spil",
       subtitle = "Pr. kamp",
       x = "Korttype",
       y = "Procent",
       fill = "Køn") +
  scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
  scale_x_discrete(labels = c(
    "red_foul_pct" = "Rødt kort",
    "yellow_foul_pct" = "Gult kort",
    "second_yellow_foul_pct" = "2. gult kort"
  )) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

mean(kvinde100$pass.length, na.rm = TRUE)

#########################
# Opgave 3.2


