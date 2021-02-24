library(tidyverse)
library(ggplot2)
library(palmerpenguins)

find("filter")
head(penguins)
penguins %>% dplyr::filter(species=="Adelie")

#scatterplot

ggplot(aes(x=flipper_length_mm, y=body_mass_g), data=penguins) +
  geom_point(aes(color=species)) +
  geom_smooth(aes(color=species),method="lm") +
  ylab("Body mass (g)") +
  xlab("Flipper length (mm)") +
  ggtitle("Penguins are cute")

#class exercise
Adelie_pengins = penguins %>% filter(species == "Adelie")
ggplot(aes(x=bill_depth_mm, y=bill_length_mm), data=Adelie_pengins) +
  geom_point(aes(color=island)) +
  ylab("Bill length (mm)") +
  xlab("Bill depth (mm)") +
  ggtitle("Depth vs Length")

penguin_ts = penguins %>%
  group_by(species, year) %>%
  summarize(num=n())
ggplot(aes(x=year, y=num, color=species),data=penguin_ts) +
  geom_point() +
  geom_line()

##########################
# histograms!
##########################

ggplot(data=penguins) + 
  geom_histogram(aes(x=flipper_length_mm, fill=species), position="identity", alpha=0.5, binwidth=5) +
  scale_fill_manual(values=c("darkorange", "darkorchid", "cyan4"))

##boxplots

ggplot(data=penguins) +
  geom_boxplot(aes(y=flipper_length_mm, x=species)) +
  geom_jitter(aes(y=flipper_length_mm, x=species, color=species))

#barchart

ggplot(data=penguins) + 
  geom_bar(aes(x=sex, fill=species)) +
  coord_flip() +
  facet_wrap(~species, ncol=1) + 
  ggsave(filename = "figures/penguin_sex_bar.png", device="png", width=4, height=7, dpi=300, units="in")
  
  
