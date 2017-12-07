
library(tidyverse)
library(readr)
library(countrycode)


votes <- readRDS("votes.Rds")
resolutions <- readRDS("descriptions.rds")
write.csv(votes, file = "votes.csv",row.names=FALSE)
write.csv(resolutions, file = "resolutions.csv",row.names=FALSE)

votes <- read_csv("votes.csv")

votes_tidy <- votes %>% 
  filter(vote <= 3) %>% 
  mutate(year = session + 1945 ,
         country= countrycode(ccode,"cown", "country.name" )) %>%
  select(-c(session,ccode)) %>%
  group_by(year , country) %>% 
  summarize( total = n() , percent_yes = mean(vote ==1)) %>% 
  ungroup()


subject_country <- votes_tidy %>% 
  filter(country %in% c("United States of America" , "Japan" ,
                        "Norway", "Sweden" , "Israel" , "France"))

subject_country %>% 
  ggplot(aes( x = year , y = percent_yes)) +
  geom_line(color = "blue") +
  facet_wrap(~ country)

# Model the trend with liner regression, finding the best fit line
# Tidying model with broom package

library(broom)


# country_coef_model <- votes_tidy %>% 
#   nest(-country) %>% 
#   mutate( model = map(data, ~lm(percent_yes ~ year , .))) %>% 
#   mutate(tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
#   filter ( term == "year" , p.adjust(p.value) < 0.05) %>% 
#   arrange(desc(estimate))

resolutions <- read_csv("resolutions.csv")


votes_tidy2 <- votes %>%
  filter(vote <=3) %>% 
  mutate(year = session + 1945 ,
         country= countrycode(ccode,"cown", "country.name" )) %>% 
  inner_join(resolutions, by=c("rcid", "session")) %>% 
  gather(topic, has_topic , me:ec) %>% 
  filter(has_topic ==1) %>% 
  
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons and nuclear material",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))

country_year_topic <- votes_tidy2 %>% 
  group_by(country, year, topic) %>% 
  summarize( total = n() , percent_yes = mean(vote == 1)) %>% 
  ungroup()


country_coef_model <- country_year_topic %>% 
  nest(-country, -topic) %>% 
  mutate( model = map(data, ~lm(percent_yes ~ year , .))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>% 
  filter ( term == "year" , p.adjust(p.value) < 0.05)

country_year_topic %>% 
  filter (country == "Norway") %>% 
  ggplot(aes(year, percent_yes ))+
  geom_line()+
  facet_wrap(~ topic)


