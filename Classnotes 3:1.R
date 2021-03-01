library(tidyverse)
library(palmerpenguins)
library(rstatix)

glimpse(penguins)

#################################
#          t-test
#################################

# one sample t-test
summary(penguins)
#gentoo
gentoo = penguins %>%
  filter(species == "Gentoo")
ggplot(data=gentoo) +
  geom_histogram(aes(x=body_mass_g))

#t-test for Gentoo body mass
mean(gentoo$body_mass_g, na.rm=TRUE)
sd(gentoo$body_mass_g, na.rm=TRUE)

#test for outliers
gentoo %>% 
  identify_outliers(body_mass_g)

# Q-Q plot
ggplot(data=gentoo) + 
  stat_qq(aes(sample=body_mass_g))

#one sample t-test
#Gentoo body mass
t.test(gentoo$body_mass_g, mu=5500) #compare with theoretical mean
gentoo_results = gentoo %>% 
  t_test(body_mass_g ~ 1, mu=5500)

## unpaired, 2 sample t-test a.k.a. "independent sample t-test"
# Gentoo vs. Adelie body mass

data_for_t_test = penguins %>% 
  filter(species %in% c("Gentoo", "Adelie")) %>%
  !is.na(body_mass_g)) %>% #filters out NA
  select(species, body_mass_g) %>%
  droplevels()

summary(data_for_t_test)

#calculate summary statistics
data_for_t_test %>%
  group_by(species) %>%
  summarize(mean = mean(body_mass_g),
            sdd = sd(body_mass_g))
ggplot(data=data_for_t_test) + 
  geom_histogram(aes(x=body_mass_g)) +
  facet_wrap(~species, scales="free")

#identify outliers
data_for_t_test %>%
  group_by(species) %>%
  identify_outliers(body_mass_g)

ggplot(data=data_for_t_test) + 
  stat_qq(aes(sample=body_mass_g))+
  facet_wrap(~species, scales="free")

# check for equality of variance
data_for_t_test %>%
  levene_test(body_mass_g~species)

# unpaired, 2-sample t-test
t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal=TRUE)

data_for_t_test %>% 
  t_test(body_mass_g ~ species)

## class exercise: flipper lengths between males and females; it's wrong, see E's example
glimpse(penguins)

data_for_exercise = penguins %>%
  group_by(sex)
  
summarize(data_for_exercise) 

ggplot(data=data_for_exercise) + 
  geom_histogram(aes(x=flipper_length_mm)) + 
  facet_wrap(~sex, scales="free")

summary(data_for_exercise)

data_for_exercise %>%
  levene_test(flipper_length_mm~sex)

data_for_exercise %>%
  group_by(sex) %>%
  identify_outliers(flipper_length_mm)

t.test(data_for_exercise$flipper_length_mm ~ data_for_exercise$sex, var.equal=TRUE)

data_for_t_test %>% 
  t_test(flipper_length_mm ~ sex)

## Correlations are fun

ggplot(data=gentoo) + 
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm))

# correlation
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs") #only use penguins that have real numbers for length and depth, not an NA

cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs") #only use penguins that have real numbers for length and depth, not an NA

head(gentoo)
cor(gentoo[,3:6], use="complete.obs")

library(GGally)

penguins %>% 
  select(species, bill_length_mm, bill_depth_mm, body_mass_g, flipper_length_mm) %>%
  ggpairs(aes(color=species))
  
  
