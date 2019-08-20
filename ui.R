
controls <-
  list(h2("Neibourhood"), 
       tags$div(align = 'right', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'checkGroup', 
                                   label    = "Select the neibourhoods:", 
                                   choices  = neighbour_name,
                                   selected = name[1],
                                   inline   = FALSE))) 
shinyUI(
  dashboardPage(skin = 'blue',
    dashboardHeader(title = p(strong(span("Seattle Housing", style="color:black"), style = "font-family:'time'")), titleWidth = 200),
    dashboardSidebar(width = 200,
      sidebarUserPanel(p(strong(span("Haoyun Zhang", style="color:black"), style = "font-family:'time'")),
                       image = 'Haoyun.png',
                       tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      ')))),
      #'https://img-s-msn-com.akamaized.net/tenant/amp/entityid/BBL1H4v.img?h=468&w=624&m=6&q=60&o=f&l=f&x=560&y=349'),
      sidebarMenu(
        menuItem("Introduction", tabName = "introduction", icon = icon("book")),
        
        menuItem('Room Map',  icon = icon('map'),
                 menuSubItem(tabName = 'RoomMap',icon = icon('bed'),
                             selectizeInput(inputId = "room_type", 
                                            label = "Room Type",
                                            choices = room_choices))),
        
        menuItem('Price Map',  icon = icon('map-signs'),
                 menuSubItem(tabName = 'PriceMap', icon = icon('money'),
                             sliderInput("price_range", "Price Range(ppd)",
                                         min = 0, max = 400,
                                         value = c(0, 50), step = 50,
                                         pre = '$', sep = ',',
                                         animate = T))),
        
        menuItem("Score Radar", icon = icon("medal"),
                 menuSubItem('Overall Score', tabName = 'score', icon = icon('globe')),
                 menuSubItem('Compared Score', tabName = 'score_compare', icon = icon('dice'))),
                
        
        menuItem("Score-Price Plot", tabName = "score_price", icon = icon("balance-scale")),
        
        menuItem("Price Data", tabName = "price_data", icon = icon("money")),
        
        menuItem("Score Data", tabName = "score_data", icon = icon("fire")))
        ),

    
    dashboardBody(
      tabItems(
        tabItem(tabName = 'introduction', 
                fluidRow(
                  box(status = "warning", solidHeader = F, 
                      collapsible = F, width = 6,
                
                  fluidRow(column(12, p(strong(h4("This project aims to: ", style = "font-family:'time'"))
                                        ), 
                                      p(h4("I: visualize the", strong(span("distribution of houses type", style="color:blue")),
                                           "on the map of seattle;", br(),
                                         "II: visualize the", strong(span("distribution of price range", style="color:red")), "on the map of seattle;", br(),
                                         "III:", strong(span("score evaluation system", style="color:green")), "for every neighbourhood of seattle;",br(),
                                         "IV:", strong(span("price-score relationship analysis",  style="color:orange")), "of every neighbourhood.", style = "font-family:'time'")
                                        ))),
                  fluidRow(column(12, align = 'center', img(src='best_neighbour.png', height = '80%', width = '90%')))),
                  
                  box(status = "danger", solidHeader = F, 
                      collapsible = F, width = 6,
                      fluidRow(column(12, align = 'center', img(src='neighbourhood.png',width = '65%')))))),
        
        tabItem(tabName = 'RoomMap',
                fluidRow(
                  # selectizeInput(inputId = "room_type", 
                  #                label = "Room Type", 
                  #                choices = c('All', unique(house_info$room_type))),
                      column( 8, align="center", 
                      leafletOutput("map_room",height  = 550,  width = 900)))
                ),
        
        tabItem(tabName = 'PriceMap',
                fluidRow(
                      column( 12, align="center" ,
                              leafletOutput("map_price", height = 550, width = 900))
                  
                )
                
        ),
        tabItem(tabName = 'score',
                fluidRow(
                  # box(status = "warning", solidHeader = F, 
                  #     collapsible = F, width = 12,
                      column( 12, align="center" ,
                              plotOutput("score_eval", height = 500, width = 900))
                      # column( 4, align="center" ,
                      #         plotOutput("score_eg", height = 500, width = 280)))
                )
        ),
        
        tabItem(tabName = 'score_compare',
                fluidRow(
                  column( 3, align="center", controls),
                  column( 9, align="center",
                              plotOutput("score_com", height = 500, width = 600))
                 
                      
                )
              ),
        
        tabItem(tabName = 'score_price',
                fluidRow(
                  column(8, align = "right",
                                plotOutput("score_price_nbh", height = 500, width = 768)),
                  column(4, align = "left", checkboxInput("checkbox", label = "University District", value = TRUE)))
                ),
        
        tabItem(tabName = 'price_data',
                  fluidRow(
                    column(12, align = 'center', 
                           DT::dataTableOutput('tb_price', height = 350, width = 860)))
                ),
        
        tabItem(tabName = 'score_data',
                fluidRow(
                  column(12, align = 'center', 
                         DT::dataTableOutput('tb_score', height = 350, width = 860)))
                  )
        
      )
    )
  )
)





