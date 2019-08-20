library(shinydashboard)
library(rCharts)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(leaflet)
library(ggmap)
library(shiny)


# load the seattle housing data into the data frame named house_info and select columns I want to analyze
house_info = read.csv('./listings.csv', stringsAsFactors = F) %>% 
  select(c(5, 38,46,47,49,50, 52,53,54,58,59,60,61,62,63,64,74,77:83))

ToNum = function(x){
  x = as.numeric((substr(x, 2, nchar(x))), 2)
  x[is.na(x)] = 0
  return(x)
}

# set the price dataframe and convert all price variables into same unit, which is <$ per person per day>
house_price <- house_info %>%
  transmute(name, neighbourhood = neighbourhood_group_cleansed, 
            guests_included, price = ToNum(price),
            price_ppd = price / guests_included,
            weekly_price = ToNum(weekly_price) / guests_included,
            monthly_price = ToNum(monthly_price) / guests_included,
            cleaning_fee = ToNum(cleaning_fee),
            extra_people = ToNum(extra_people),
            security_deposit = ToNum(security_deposit),
            longitude, latitude) %>%
  filter(guests_included != 0, price_ppd > 3)

# set the score dataframe and store all score related column into it
house_score <- house_info %>%
  select(contains('score'),neighbourhood = neighbourhood_group_cleansed, name, number_of_reviews)

# # clean the score dataframe and calculate the average scores for each aspect
house_score_ave <- house_score %>%
  mutate(ave_score = rowMeans(house_score[, 2:7])) %>%
  filter(is.na(ave_score) == F) %>%
  group_by(neighbourhood) %>%
  summarise(accuracy = sum(review_scores_accuracy * number_of_reviews)/sum(number_of_reviews),
            checkin = sum(review_scores_checkin * number_of_reviews)/sum(number_of_reviews),
            clean = sum(review_scores_cleanliness * number_of_reviews)/sum(number_of_reviews),
            communication = sum(review_scores_communication * number_of_reviews)/sum(number_of_reviews),
            location = sum(review_scores_communication * number_of_reviews)/sum(number_of_reviews),
            value = sum(review_scores_value * number_of_reviews)/sum(number_of_reviews))

house_score_ave_2 = data.frame()
for (n in c(2, 4, 6)){
  house_score_ave_2 = rbind(house_score_ave_2, gather(house_score_ave, colnames(house_score_ave)[n], colnames(house_score_ave)[n+1],
                                                      key = 'aspect', value = 'ave_score')[, c(1,6,7)])
}


score_price <- inner_join(house_price %>% 
                            group_by(neighbourhood) %>% 
                            summarise(ave_price = mean(price_ppd)),
                          (house_score_ave_2 %>% 
                             group_by(neighbourhood) %>% 
                             summarise(ave_value = mean(house_score_ave_2$value))))


# the function is to convert the polar coordinator into radar coordinator,
# refer to http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html
# I learned a lot from this, it is brilliant!
coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

room_choices = c('All', unique(house_info$room_type))
neibour_choices = c(house_score$neighbourhood)
name = unique(house_score$neighbourhood)
neighbour_name = name[1:17]
names(neighbour_name) <- name




























