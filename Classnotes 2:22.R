library(tidyverse)

#grab penguin data
install.packages("palmerpenguins")
library(palmerpenguins)
head(penguins)
summary(penguins)
glimpse(penguins)

#filter

ladies = filter(penguins, sex == "female")
summary(ladies)

gentoo_ladies = filter(penguins, sex=="female", species=="Gentoo")
summary(gentoo_ladies)

#the pipe
gentoo_ladies = penguins %>% filter(sex=="female",
                                    species=="Gentoo") %>%
summarize(mean_body_mass_g = mean(body_mass_g))

gentoo_ladies

#compare to base R
female_penguins = penguins[which(penguins$sex=="female"),]
gentoo_ladies = female_penguins[which(female_penguins$species=="Gentoo"),]
mean(gentoo_ladies$body_mass_g)

chinstrap_penguins = penguins %>% filter(species =="Chinstrap")
chinstrap_flippers = penguins %>% filter(species =="Chinstrap", flipper_length_mm > 200)
summary(chinstrap_penguins)
#ratio of f to m for chinstraps = 1:1

summary(chinstrap_flippers)
#f:m ratio = 1:17, so given the comparison between the two ratios I think that males tend to have much larger flipper length than females

penguins_mass = penguins %>% 
  filter(!is.na(sex)) %>% #remove NA from data
  group_by(species, sex) %>% #separate each species into a distinct group
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=TRUE))
            
penguins_mass

write.csv() # base r
write_csv(penguins_mass, '~/Documents/Whale_data/unit_3_penguins/penguins_mass.csv')

#more dplyr functions

num_species_by_island = penguins %>% 
  group_by(species, island) %>%
  summarize(n())

#mutate
penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g*0.0022) # 0.0022 lb/g

glimpse(penguins_for_america)

#distinct
penguins %>% 
  distinct(island)

#select()
penguins %>% select(species, sex)
penguins %>% select(-bill_length_mm, -bill_depth_mm)

#arrange
penguins %>% 
  arrange(rev(body_mass_g))
