#Justin Moreno
#Wrangling data For college Scoreboard

library(tidyverse)

college_sc <- read.csv("./college_scorecard_data.csv")

glimpse(college_sc)

view(college_sc)

head(college_sc)

summary(college_sc)

names(college_sc)


college_sc <- college_sc %>%
  rename(state = STABBR)


names(college_sc) <- str_to_lower(names(college_sc))

names(college_sc)

summary(college_sc)


college_sc <- college_sc %>%
  mutate(adm_rate = as.numeric(str_replace(adm_rate, "NULL", ""))) 


college_sc <- college_sc %>%
  mutate(sat_avg = as.numeric(str_replace(sat_avg, "NULL", "")))
  

college_sc <- college_sc %>%
  mutate(avg_facsal_year = avgfacsal*12)

college_sc %>% 
  select(instnm, avgfacsal, avg_facsal_year)

write_csv(college_sc, "./college_sc_cleaned.csv")


ncol(mtcars)
nrow(mtcars)
cars <- (mtcars)
summary(mpg)

names(cars) <- str_to_lower(names(cars))

table(mtcars$am) 


# Step 1: Find the maximum MPG value
max_mpg <- max(mtcars$mpg)

# Step 2: Find the cars with the maximum MPG
best_mpg_cars <- mtcars %>% 
  filter(mpg == max_mpg)

# View the cars with the best MPG
best_mpg_cars

mean_disp <- mean(mtcars$disp)  # Calculate the mean of 'disp'
round(mean_disp, 4)  # Round the result to 4 decimal places


round(mean(mtcars$disp[mtcars$mpg > 20]), 4)
