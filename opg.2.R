# opg.2.4
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(factoextra)
library(pls)

Obesitydf <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
BMI <- readRDS("newlife-bmi.rds")

# rense data BMI
df <- BMI %>%
  # Først konverter Year til character for sikkerhed
  mutate(Year = as.character(Year)) %>%
  # Så anvend en mere detaljeret case_when
  mutate(
    Year_clean = case_when(
      # Hvis Year er NA
      is.na(Year) ~ NA_real_,
      
      # Hvis Year matcher "YYYY-YYYY" format (fx "2017-2018")
      grepl("^\\d{4}-\\d{4}$", Year) ~ as.numeric(gsub(".*-(\\d{4}).*", "\\1", Year)),
      
      # Hvis Year matcher andet format med tal ("-" kan være andre steder)
      grepl("\\d{4}", Year) ~ as.numeric(gsub(".*?(\\d{4}).*", "\\1", Year)),
      
      # Ellers NA
      TRUE ~ NA_real_
    )
  ) %>%
  select(Country, Country.Code, Year, Year_clean, Age,
         MalesOverweight.BMI.25.29.9kg.m.., MalesObesity.BMI..30kg.m..,
         FemalesOverweight.BMI.25.29.9kg.m.., FemalesObesity.BMI..30kg.m..,
         AllOW, AllOB)


life <- BMI %>%
  select(Country, Country.Code, starts_with("X")) %>%
  # Konverter fra bredt til langt format
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "LifeExpectancy"
  ) %>%
  # Rens årstal (fjern "X" fra årstallet og konverter til numerisk)
  mutate(
    Year_clean = as.numeric(gsub("X", "", Year)),
    LifeExpectancy = as.numeric(LifeExpectancy)
  ) %>%
  # Fjern NA-værdier i levetidsdata
  filter(!is.na(LifeExpectancy))


cor_df <- df %>%
  # Konvertér til numeriske værdier
  mutate(across(c(MalesOverweight.BMI.25.29.9kg.m.., MalesObesity.BMI..30kg.m..,
                  FemalesOverweight.BMI.25.29.9kg.m.., FemalesObesity.BMI..30kg.m..,
                  AllOW, AllOB), ~as.numeric(as.character(.)))) %>%
  # Join med levetidsdata
  inner_join(
    life %>% select(Country.Code, Year_clean, LifeExpectancy),
    by = c("Country.Code", "Year_clean"),
    relationship = "many-to-many"
  ) %>%
  # Gruppér for at håndtere duplikater
  group_by(Country.Code, Year_clean) %>%
  summarise(
    Country = first(Country),
    MalesOW = mean(MalesOverweight.BMI.25.29.9kg.m.., na.rm = TRUE),
    MalesOB = mean(MalesObesity.BMI..30kg.m.., na.rm = TRUE),
    FemalesOW = mean(FemalesOverweight.BMI.25.29.9kg.m.., na.rm = TRUE),
    FemalesOB = mean(FemalesObesity.BMI..30kg.m.., na.rm = TRUE),
    AllOW = mean(AllOW, na.rm = TRUE),
    AllOB = mean(AllOB, na.rm = TRUE),
    LifeExpectancy = mean(LifeExpectancy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Fjern rækker hvor centrale værdier er NA
  filter(!is.na(AllOB) & !is.na(LifeExpectancy))

# korrelation mellem bmi og life-exp

cor_ob_life <- cor(cor_df$AllOB, cor_df$LifeExpectancy, use = "complete.obs")
cor_ow_life <- cor(cor_df$AllOW, cor_df$LifeExpectancy, use = "complete.obs")
cor_male_ob_life <- cor(cor_df$MalesOB, cor_df$LifeExpectancy, use = "complete.obs")
cor_female_ob_life <- cor(cor_df$FemalesOB, cor_df$LifeExpectancy, use = "complete.obs")
cor_male_ow_life <- cor(cor_df$MalesOW, cor_df$LifeExpectancy, use = "complete.obs")
cor_female_ow_life <- cor(cor_df$FemalesOW, cor_df$LifeExpectancy, use = "complete.obs")

# plot
ggplot(cor_df, aes(x = AllOW, y = LifeExpectancy)) +
  geom_point(aes(color = Year_clean), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Høj korrelation mellem ALLOW og forventet levetid",
       subtitle = paste("Korrelation: r =", round(cor_ow_life, 3)),
       x = "(BMI=25-30) procent af befolkning",
       y = "Forventet levetid ved fødsel (år)",
       color = "År") +
  theme_minimal()

# regression
model_who <- lm(LifeExpectancy ~ MalesOW+MalesOB+FemalesOW+FemalesOB+AllOW+AllOB, data = cor_df)
model_who <- lm(LifeExpectancy ~ MalesOW+MalesOB+AllOB, data = cor_df)

summary(model_who)

# cor matrix
cor_matrix <- cor_df %>%
  select(MalesOW, MalesOB, FemalesOW, FemalesOB, AllOW, AllOB, LifeExpectancy) %>%
  cor(use = "pairwise.complete.obs")
print(cor_matrix)

# bregne ALLOW og life-ex
AllOW <- cor_df %>% 
  filter(!is.na(AllOW)) %>%  
  group_by(AllOW_group = ifelse(AllOW > 32.25, "Over 32.25", "Under 32.25")) %>%
  summarise(
    mean_life_expectancy = mean(LifeExpectancy, na.rm = TRUE),
    count = n()
  )

# Beregne life-expectency. e(x)=T(x)/L(x)
# x = age
# T(x)= total number of person-years lived by the cohort from age x until all members of the cohort have died.
# L(x) = l(x+1) + 0.5*d(x).
# l(x+1) = l(x)*exp[-m(x)]
# m(x) = d(x)/L(x)
# d(x) = l(x) - l(x+1)
# qx=Mx/(Bx+(Mx/2)).
# Mx = the number of deaths at the age of x to under x+1 years in the reported period
# Bx = average population aged x to under x+1 in the base period
# age i obesity dataframe er fra 14-61
# vi finder andre information på internet
# vi tager år 2019 for undgår corona periode 
# ældste person i datasæt er på 61, og næste ældste er på 56, der er 5 år imellem, så fjerne vi ældste person for bedre resultater
# vi fjerne også folk på 14 år og folk der over 54 år for sikker vores resultater

# hente csv fil fra CDC. https://wonder.cdc.gov/single-race-single-year-v2019.html
#ages <- read.csv("Single-Race Population Estimates 2010-2019 by State and Single-Year Age.csv")
#ages <- ages[-c(39:55),]
#ages <- as.numeric(gsub(";", "", ages))
#age_groups <- 15:52
#agesdf <- data.frame(
  #Age = age_groups,
  #Population = ages
#)
#Obesitydf <- Obesitydf[-c(134,416),]
#Obesitydf <- Obesitydf %>% filter(Age<=54)

#death <- 29771+59178+82986+160393
#population <- (20849+21254+23277+21932+21443+19584+20345+20355)*1000


#age_rates <- data.frame(
 # age_group = c("15-24", "25-34", "35-44", "45-54"),
  #age_start = c(15, 25, 35, 45),
  #age_end = c(24, 34, 44, 54),
  #death_rate = c(69.7, 128.8, 199.2, 392.4) / 100000  
#)
#Obesitydf$life <- NA

# Vi kan bare hent life-expectency fra CDC hjemmeside isted for selv regne ude. 
# https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/NVSR/70-19/
# Vi behøver ikke fjerne alder fra dataframe

# rense data obesity

Obesitydf <- Obesitydf %>%
  mutate(across(c(FCVC, NCP, CH2O, FAF, TUE), round))

Obesitydf$BMI <- with(Obesitydf, Weight / (Height^2))

Obesitydf$BMI_Category <- cut(Obesitydf$BMI, 
                              breaks = c(0, 18.5, 25, Inf),
                              labels = c(0, 1, 2))

Obesitydf$BMI_cat <- cut(Obesitydf$BMI, 
                              breaks = c(0, 18.5, 25, 30, Inf),
                              labels = c(0, 1, 2, 3))

Obesitydf$Age <- round(Obesitydf$Age)

USA <- Obesitydf %>% 
  group_by(BMI_cat, Gender) %>%
  summarise(
    mean_value = mean(n(), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = c(Gender, BMI_cat),
    values_from = mean_value,
    names_prefix = "",
    names_glue = "{ifelse(Gender == 'Male', 'Males', 'Females')}{ifelse(BMI_cat == 0, 'UW', 
                 ifelse(BMI_cat == 1, 'N', 
                 ifelse(BMI_cat == 2, 'OW', 'OB')))}"
  ) %>%
  mutate(
    AllUW = (MalesUW + FemalesUW)/2,
    AllN = (MalesN + FemalesN)/2,
    AllOW = ((MalesOW + FemalesOW)/2111)*100,
    AllOB = ((MalesOB + FemalesOB)/2111)*100
  ) %>%
  select(MalesOW, MalesOB, FemalesOW, FemalesOB, AllOW, AllOB)

life_exp <- read_excel("Table01.xlsx", sheet = 1)
life_exp <- life_exp[-101,]

life_exp <- life_exp %>%
  mutate(
    Age = as.numeric(gsub("^(\\d+)[-–].*$", "\\1", Age))
  )

Obesitydf <- Obesitydf %>%
  left_join(
    select(life_exp, Age, ex),
    by = "Age"
  )

cor_us <- cor(Obesitydf$ex,Obesitydf$BMI,use = "complete.obs")

model <- lm(ex ~ BMI, data = Obesitydf)
summary(model)

ggplot(Obesitydf, aes(x = BMI, y = ex)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Life-exp falder når BMI stiger",
       x = "BMI (kg/m²)",
       y = "Forventet levetid (år)") +
  theme_minimal()

# Kan eventuelt lave en glm model for Obesitydf
# første skal vi erstatte yes og no til faktor
kk <- c("family_history_with_overweight", "FAVC", "SMOKE","SCC")

for (k in kk) {
  Obesitydf[[k]] <- ifelse(Obesitydf[[k]] == "yes", 1, 
                      ifelse(Obesitydf[[k]] == "no", 0, Obesitydf[[k]]))
}

kkk <- c("CAEC","CALC")
for (k in kkk) {
  Obesitydf[[k]] <- ifelse(Obesitydf[[k]] == "no", 0, 
                           ifelse(Obesitydf[[k]] == "Sometimes", 1,
                                  ifelse(Obesitydf[[k]] == "Frequently", 2,
                                         ifelse(Obesitydf[[k]] == "Always", 3,
                                  
                                  Obesitydf[[k]]))))
}

kkkk <- ("MTRANS")
for (k in kkkk) {
  Obesitydf[[k]] <- ifelse(Obesitydf[[k]] == "Walking", 0, 
                           ifelse(Obesitydf[[k]] == "Bike", 1,
                                  ifelse(Obesitydf[[k]] == "Motorbike", 2,
                                         ifelse(Obesitydf[[k]] == "Automobile", 3,
                                                ifelse(Obesitydf[[k]] == "Public_Transportation", 4,
                                                Obesitydf[[k]])))))
}

Obesitydf <- Obesitydf %>%
  mutate(across(c(family_history_with_overweight, FAVC, SMOKE, CAEC, SCC, CALC,MTRANS,FCVC,,NCP,CH2O,FAF,TUE,BMI_Category), as.factor))

# glm
glm <- glm(BMI_Category ~ family_history_with_overweight + CAEC + 
             CALC + NCP + FAF + TUE, 
           data = Obesitydf, 
           family = "binomial")

summary(glm)

# predict
Obesitydf$predict <- predict(glm, type = "response")

summary(Obesitydf$predict)

hist(Obesitydf$predict, breaks=30, main="Fordeling af predict", 
     xlab="Forudsigelsesværdi", col="lightblue")

Obesitydf$predict_bmi <- cut(Obesitydf$predict, 
                                    breaks = c(-Inf, 0.5, 0.9, Inf), 
                                    labels = c(0, 1, 2))


conf_matrix <- table(Rigtig = Obesitydf$BMI_Category, 
                     Predict = Obesitydf$predict_bmi)
conf_matrix

# pca
Obesitydf_prep <- Obesitydf %>%
  mutate(across(c(family_history_with_overweight, FAVC, SMOKE, CAEC, 
                  SCC, CALC, MTRANS, FCVC, NCP, CH2O, FAF, TUE,BMI_Category),
                ~ as.numeric(as.character(.))))

pcr.fit <- pcr(
  ex ~ 
    family_history_with_overweight + CAEC + 
    CALC + NCP + FAF + TUE + BMI_Category,
  data = Obesitydf_prep,
  scale = TRUE,
  validation = "CV"
)
summary(pcr.fit)
loading.pcr <- pcr.fit$loadings
comp <- loading.pcr[,1:4]
comp
