library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(leaflet)
library(ggmap)
library(shiny)
library(grid)
library(DT)


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
            price_ppd = round(price / guests_included, 2),
            weekly_price = round(ToNum(weekly_price) / guests_included, 2),
            monthly_price = round(ToNum(monthly_price) / guests_included, 2),
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
  summarise(accuracy = mean(review_scores_accuracy),
            checkin = mean(review_scores_checkin),
            cleanliness = mean(review_scores_cleanliness),
            communication = mean(review_scores_communication),
            location = mean(review_scores_communication),
            value = mean(review_scores_value))

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
                             summarise(score_value = mean(ave_score))))


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
#
# draw the score evaluation radar figures to compare the scores of different neibourhood


room_labels <- lapply(seq(nrow(house_info)), function(i) {
  paste0( '<p>', house_info[i, "name"], '<p></p>', 
          'Property: ', house_info[i, "property_type"], '<p></p>',
          'Included Guest: ', house_info[i, "guests_included"], '<p></p>',
          'Bathrooms: ', house_info[i, "bathrooms"],'</p><p>', 
          'Badrooms: ', house_info[i, "bedrooms"], '</p>' )
})

price_labels <- lapply(seq(nrow(house_price)), function(i) {
  paste0( '<p>', house_price[i, "name"], '<p></p>',
          'Cleaning Fee($): ', house_price[i, "cleaning_fee"],'</p><p>', 
          'Security Deposit($): ', house_price[i, "security_deposit"],'</p><p>', 
          'Fee for Extra People($): ', house_price[i, "extra_people"], '</p>' )
})
  
function(input, output){
  observeEvent(input$room_type, {
    if(input$room_type == 'All'){
      output$map_room <-  renderLeaflet(
        house_info %>%
          leaflet() %>%
          addTiles(# Add default OpenStreetMap map tiles and set the zoom value from 11 to 16
            options = providerTileOptions(minZoom = 10, maxZoom = 16)) %>%
          # Add circle markers on the map and specified different colors
          addCircles(
            ~ longitude, ~ latitude,
            color = ~ case_when(
              room_type == 'Entire home/apt' ~ 'red',
              room_type == 'Private room' ~ 'green',
              room_type == 'Shared room' ~ 'purple'
            ),
            label = ~lapply(room_labels, htmltools::HTML)
          ) %>% 
          setView(-122.335167, 47.608013, zoom = 11)
      )
    } else {
      output$map_room <-  renderLeaflet(
        house_info %>%
          filter(room_type == input$room_type) %>%
          leaflet() %>%
          addTiles(# Add default OpenStreetMap map tiles and set the zoom value from 11 to 16
            options = providerTileOptions(minZoom = 10, maxZoom = 16)) %>%
          # Add circle markers on the map and specified different colors
          addCircles(
            ~ longitude,
            ~ latitude,
            color = ~ case_when(
              room_type == 'Entire home/apt' ~ 'red',
              room_type == 'Private room' ~ 'green',
              room_type == 'Shared room' ~ 'purple'
            ),
            label = ~lapply(room_labels, htmltools::HTML)
          ) %>% 
          setView(-122.335167, 47.608013, zoom = 11)
      )
    }
  })
  
    output$map_price <- renderLeaflet(
    house_price %>% 
      filter(house_price$price_ppd > input$price_range[1] & house_price$price_ppd <= input$price_range[2]) %>%
      leaflet() %>%
        addTiles(
          options = providerTileOptions(minZoom = 10, maxZoom = 16)) %>%
        addCircles(~longitude, ~latitude,
                   #set the pice range and set different colors for circle markers
                   color = ~case_when(price_ppd <= 50  ~ 'red',
                                      price_ppd <= 100 ~ 'yellow',
                                      price_ppd <= 150 ~ 'green',
                                      price_ppd <= 200  ~ 'aquamarine',
                                      price_ppd <= 250  ~ 'blue',
                                      price_ppd <= 300  ~ 'blueviolet',
                                      price_ppd <= 400  ~ 'black'),
                   label = ~lapply(price_labels, htmltools::HTML)) %>%
 
      setView(-122.335167, 47.608013, zoom = 11))
   
    output$score_eval <- renderPlot(
      house_score_ave_2 %>%
        # Arrange the aspect part to make sure the line connected by the right order and make sure
        # there is no intersection in the polygon.
        arrange(desc(aspect)) %>%
        ggplot(aes(x = aspect, y = ave_score)) +
        # use geom_polygon rather geom_line, because geom_line cannot connect the start points and endpoints
        geom_polygon(aes(group = neighbourhood, color = neighbourhood),
                     fill = NA,
                     size = 1) +
        # remove the labels on each facet
        theme(
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
        ) +
        xlab('') + ylab('') + 
        theme(legend.position="right", legend.text = element_text(size = 14),
              legend.key.size = unit(1.5, 'line'), legend.title = element_text(face="bold", size = 16)) +
        ylim(min(house_score_ave_2$ave_score), 10) +
        facet_wrap( ~ neighbourhood) +
        coord_radar())
    
    output$score_com <- renderPlot(
      house_score_ave_2 %>%
        filter(neighbourhood %in% input$checkGroup) %>% 
        # Arrange the aspect part to make sure the line connected by the right order and make sure
        # there is no intersection in the polygon.
        arrange(desc(aspect)) %>%
        ggplot(aes(x = aspect, y = ave_score)) +
        # use geom_polygon rather geom_line, because geom_line cannot connect the start points and endpoints
        geom_polygon(aes(group = neighbourhood, color = neighbourhood),
                     fill = NA,
                     size = 1) +
        # remove the labels on each facet
        xlab('') + ylab('') + 
        theme(
          axis.ticks.x = element_line(),
          axis.text.x = element_text(face = "bold", size = 10, angle = 30),
          axis.ticks.y = element_line(),
          axis.text.y = element_text(face = "bold", size = 10),
          legend.position = "right",
          legend.text = element_text(size = 10),
          legend.key.size = unit(1.5, 'line'),
          legend.title = element_text(face = "bold", size = 14)) +
        ylim(8.8, 10) +
        coord_radar())
    
    output$score_price_nbh <- renderPlot(
      if(input$checkbox){
        score_price %>% 
          ggplot(aes(x = score_value, y = ave_price, group = neighbourhood)) +
          geom_point(aes(
            shape = neighbourhood,
            color = neighbourhood,
            size = neighbourhood
          )) +
          scale_shape_manual(values  = c(1:17)) +
          scale_size_manual(values = rep(5, 17)) +
          scale_x_continuous(limits = c(9.1, 9.9)) +
          xlab('Average Score') +
          ylab('Average Price($ppd)') +
          theme(
            axis.title = element_text(face = "bold", size = 12),
            axis.ticks.x = element_line(),
            axis.text.x = element_text(face = "bold", size = 10),
            axis.ticks.y = element_line(),
            axis.text.y = element_text(face = "bold", size = 10),
            legend.position = "right",
            legend.text = element_text(face = "bold", size = 10),
            legend.key.size = unit(1.5, 'line'),
            legend.title = element_text(face = "bold", size = 12)) +
          geom_hline(
            yintercept = 90 ,
            linetype = "dashed",
            color = "red",
            size = 1
          ) +
          geom_vline(
            xintercept = 9.7 ,
            linetype = "dashed",
            color = "red",
            size = 1
          ) +
          geom_segment(aes(
            x = 9.31,
            y = 85,
            xend = 9.2,
            yend = 77
          ),
          arrow = arrow(length = unit(0.5, "cm")))
      } else{
        score_price %>% filter(neighbourhood != 'University District') %>% 
          ggplot(aes(x = score_value, y = ave_price, group = neighbourhood)) +
          geom_point(aes(
            shape = neighbourhood,
            color = neighbourhood,
            size = neighbourhood
          )) +
          scale_shape_manual(values  = c(1:16)) +
          scale_size_manual(values = rep(5, 16)) +
          scale_x_continuous(limits = c(9.5, 9.9)) +
          xlab('Average Score') +
          ylab('Average Price($ppd)') +
          theme(
            axis.title = element_text(face = "bold", size = 12),
            axis.ticks.x = element_line(),
            axis.text.x = element_text(face = "bold", size = 10),
            axis.ticks.y = element_line(),
            axis.text.y = element_text(face = "bold", size = 10),
            legend.position = "right",
            legend.text = element_text(face = "bold", size = 10),
            legend.key.size = unit(1.5, 'line'),
            legend.title = element_text(face = "bold", size = 12)) +
          geom_hline(
            yintercept = 90 ,
            linetype = "dashed",
            color = "red",
            size = 1
          ) +
          geom_vline(
            xintercept = 9.7 ,
            linetype = "dashed",
            color = "red",
            size = 1
          )
      }
    
    )
    
    
    output$tb_price = DT::renderDataTable({
      DT::datatable(house_price[, 1:10], options = list(lengthMenu = c(10,30, 50), 
                                                pageLength = 10,
                                                scrollX = TRUE,
                                                filter = 'top', autoWidth = TRUE,
                                                columnDefs = list(list(width = '200px', targets = 1))))
      })
    
    output$tb_score = DT::renderDataTable({
      DT::datatable((house_score %>%
                      mutate(ave_score = round(rowMeans(house_score[, 2:7]), 2)) %>%
                      filter(is.na(ave_score) == F))[, -1], options = list(lengthMenu = c(10,30, 50), 
                                                        pageLength = 10,
                                                        scrollX = TRUE,
                                                        filter = 'top', autoWidth = TRUE,
                                                        columnDefs = list(list(width = '200px', targets = 8))))
    })
    
    


  
         
}

