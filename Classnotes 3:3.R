library(tidyverse)
library(palmerpenguins)
library(GGally)


temp = penguins %>%
  select(-island, -sex, -year) %>%
  GGally:: ggpairs(aes(color=species))

## Bill length vs bill depth
penguins %>% 
  select(bill_length_mm, bill_depth_mm) %>%
  ggpairs()

# Build linear model
lm1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
summary(lm1)

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm)) + 
  geom_point() +
  geom_smooth(method="lm")
plot(lm1)

# plot.lm function
class(lm1)
plot(lm1)

# model is bad - separate out species to build better model
gentoo = penguins %>% 
  filter(species=="Gentoo")

gentoo %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

lm2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
class(lm2)
summary(lm2)

ggplot(aes(y=bill_depth_mm, x = bill_length_mm), data=gentoo) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(aes(y=bill_depth_mm, x = bill_length_mm, color=species), data=penguins) +
  geom_point() + 
  geom_smooth(method="lm")

#simpsons paradox - look at erin's notes
ggplot(aes(y=bill_depth_mm, x = bill_length_mm, color=species)) +
  geom_smooth(method="lm")

#class exercise
lm3 = lm(flipper_length_mm ~ bill_length_mm, data=gentoo)
plot(lm3)
summary(lm3)

gentoo %>%
  select(flipper_length_mm, bill_length_mm) %>%
  ggpairs()
#corr = 0.661

ggplot(aes(y=flipper_length_mm, x = bill_length_mm, color=species), data=gentoo) +
  geom_point() + 
  geom_smooth(method="lm")

#lm2 r squared = 0.41, lm3 = 0.43, so lm3 is better b/c r squared is higher
