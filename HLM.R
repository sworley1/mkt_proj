library(tidyverse)
library(lme4)
library(MCMCpack)
listings <- read.csv("~/Desktop/MKT-FinalProj/listings_cleaned.csv")

listings <- listings %>% mutate(
  
  logPrice = log(price_clean),
  daysRented_30 = 30 - availability_30 
  
) %>% drop_na()

lm.1 <- lm(daysRented_30 ~ accommodates + bedrooms + logPrice + room_type, 
           data=listings)

summary(lm.1)

dhr.mcmc <- MCMChregress(fixed=daysRented_30 ~ accommodates + bedrooms + logPrice:room_type, random=~logPrice, group="neighbourhood_cleansed",
                         mcmc=6000,
                         thin=6,
                         data=listings,
                         r=2,
                         R=diag(2))
summary(hr.mcmc$mcmc[,1:5])

plot(dhr.mcmc$mcmc[,2], main="mu 1 , History")
hist(dhr.mcmc$mcmc[,2], main="mu 1 , History")

plot(dhr.mcmc$mcmc[,5], main="gamma 2 , Promotion:Income")
hist(dhr.mcmc$mcmc[,5], main="gamma 2 , Promotion:Income")
