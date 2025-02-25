#untitled 7 
#working with college Schoolboard


library(tidyverse)
library(leaflet)
who <- read.csv("C:/Users/justi/OneDrive/Desktop/CGit/college_sc_cleaned.csv")

summary(who$costt4_a)

sd(who$costt4_a, na.rm= TRUE)

who %>%
  ggplot(aes(x = costt4_a)) + geom_density()

who %>%
  ggplot(aes(x = costt4_a )) + geom_histogram() + coord_flip()

who %>% 
  count(state)


who %>% 
  count(state) %>%
  View()

table(who$state)


who %>% 
  count(n_distinct(state))

who %>% 
  ggplot(aes(x = state)) + geom_bar() + coord_flip()

# Create a density plot for 'costt4_a'
who %>%
  ggplot(aes(x = costt4_a)) + 
  geom_density() +
  labs(title = "Density Plot of Cost of Attendance",
       x = "Cost of Attendance",
       y = "Density")

library(ggplot2)
who %>%
  ggplot(aes(x = sat_avg, y = costt4_a)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +  # Add regression line
  labs(title = "Average cost of attendance vs SAT scores",
       x = "SAT average",
       y = "Cost of attendance")

who %>% 
  group_by(state) %>% 
  summarize(avg_cost = mean(costt4_a, na.rm = TRUE)) %>% 
  arrange(desc(avg_cost))

who%>% 
  group_by(state) %>% 
  summarise(avg_cost = mean(costt4_a, na.rm = TRUE)) %>%
  arrange(desc(avg_cost)) %>% 
  head(10) %>% 
  ggplot(aes( x = state, y = avg_cost)) + geom_col() + labs(title= "AVG per state" , 
                                                            x= "State",
                                                            y= "Cost")
table(who$womenonly, who$state)



who %>%
  group_by(state) %>%
  count(womenonly) %>%
  filter(womenonly == 1) %>%
  ggplot(aes(x = state, y = n)) + geom_col()

who %>%
  filter(menonly == 1) %>%    # Filter rows where menonly is 1
  group_by(state) %>%         # Group by state
  count() %>%                 # Count the number of occurrences of menonly == 1 in each state
  ggplot(aes(x = state, y = n)) +  # Plot states on x-axis and count on y-axis (n is the count column)
  geom_col() +                # Create a bar chart
  labs(title = "Men Only by State", x = "State", y = "Count")

who%>% 
  ggplot(aes(x = sat_avg, y = costt4_a, color = state)) + geom_point() + geom_smooth(method = "lm", color = "red") + theme(legend.position = "none")

cor.test()

