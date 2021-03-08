library(tidyverse)
library(GGally)
library(car)
library(palmerpenguins)
library(ggiraph)
library(ggiraphExtra)

# Multiple Regression

summary(penguins)
penguins_lm_3 = penguins %>%
  filter(!is.na(sex),
         !is.na(bill_depth_mm),
         !is.na(bill_length_mm))
summary(penguins_lm_3)

# build model
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
summary(lm_3)
coef(lm_3)[1]
coef(lm_3)[2]
anova(lm_3)

broom::tidy(lm_3, conf.int=TRUE) %>%
  mutate_if(is.numeric, round, 2)

ggPredict(lm_3, se=TRUE, interactive=TRUE)

#predict with predict()

lm_3_predictions = predict(lm_3, interval="confidence")
head(lm_3_predictions)
head(penguins_lm_3)
penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predict)

# plot the predictions
ggplot(data = penguins_lm_3_predict, aes(y = bill_depth_mm, x = bill_length_mm, color = species)) + 
  geom_point() + 
  geom_line(aes(y=fit)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr, fill = species), alpha = 0.5, color=NA)      #confidence interval

### generate predictions with new data

newdata_bill_length_mm = seq( min(penguins_lm_3$bill_length_mm), max(penguins_lm_3$bill_length_mm), by = 0.1)
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, species = unique(penguins_lm_3$species))  
tail(newdata)

newdata_predictions = predict(lm_3, newdata = newdata, interval = "confidence")  
head(newdata_predictions)
newdata_predict_lm_3 = cbind(newdata, newdata_predictions)

ggplot() +
  geom_point(data=penguins_lm_3, aes(y=bill_depth_mm, x=bill_length_mm, color=species)) + 
  geom_line(data=newdata_predict_lm_3, aes(y=fit, x=bill_length_mm, color=species)) +
  geom_ribbon(data = newdata_predict_lm_3, aes(ymin = lwr, ymax=upr, x = bill_length_mm, fill = species), color=NA, alpha=0.5)

# using tidyverse
tidy_predict = lm_3 %>%
  broom:: augment(penguins_lm_3 se_fit=TRUE)
  mutate(lwr = .fitted - 1.96 * .se.fit,
         upr = .fitted + 1.96 * .se.fit)

