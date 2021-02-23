library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(reshape2)

ui <- dashboardPage(
  dashboardHeader(title="2019 Squirrel Census",
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Gray Squirrel",
                   message = "You guys are nuts (acorns, specifically).",
                   icon = icon("paw", class = NULL, lib = "font-awesome")
                 ),
                 messageItem(
                   from = "Mr. Hawke",
                   message = "Is data real-time? I'm looking for lunch.",
                   icon = icon("question"),
                   time = "13:45"
                 ),
                 messageItem(
                   from = "Support",
                   message = "The new server is ready.",
                   icon = icon("life-ring"),
                   time = "2014-12-01"
                 )
    ),
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "6 New Squirrel Users",
                   icon("grin-stars"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "12 New Preditor Users",
                   icon("grimace"),
                   status = "danger"
                 )
    ),
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Add more squirrel photos"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Add 2020 data"
                 ),
                 taskItem(value = 75, color = "yellow",
                          "Measure squirrel internet traffic"
                 ),
                 taskItem(value = 5, color = "red",
                          "Attend Conference: Computing for Rodents"
                 )
    )
  ), # EO dashboard header
  dashboardSidebar(
    sidebarMenu(
      menuItem("Communication", tabName = "squirrelcoms", icon = icon("dashboard")),
      menuItem("Activity", tabName = "activity", icon = icon("th")),
      menuItem("Physical Attributes", tabName = 'physical', icon=icon("search")),
      menuItem("Map", tabName = 'location', icon = icon("map")),
      menuItem("More Info", tabName = 'moreinfo', icon=icon("check"))
    )
  ), # dashboard Sidebar
  dashboardBody(    
    # First tab content
    tabItems(
      tabItem(tabName = "squirrelcoms",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  radioButtons("TailBehav", "Tail Behavior:",
                               c("Flag" = "Flags",
                                 "Twitch" = "Twitches")
                  )
                )
              ), # end of row
              fluidRow(
                box(plotOutput("plot2", height = 250)),
                
                box(
                  radioButtons("Vocals", "Vocal Behavior:",
                               c("Lead Singer" = "kuk",
                                 "Backup" = "quaa",
                                 "Chorus" = "moan"),
                  ),
                )
              ), # end of row
              fluidRow(
                textOutput("txt"),
                tags$head(tags$style("#txt{color: red;
                                 font-size: 25px;
                                 }"
                                )
                )
                
              ),
              fluidRow(
                tags$h2("Descriptions taken from ",
                        tags$a(href = "https://www.wired.com/2014/06/squirrel-alarm-calls-are-surprisingly-complex/", "WIRED"))              
              )
      ), # end of first Tab
      tabItem(tabName = "activity",
              fluidRow(
                box(plotOutput("plot3", height=250)),
                
                box(
                  checkboxGroupInput("actions", label = h3("Actions:"), 
                                     choices = list("Running" = 1, "Chasing" = 2, "Climbing" = 3,
                                                    "Eating" = 4, "Foraging" = 5),
                                     selected = 1)                  
                )
                
                
              ),
              fluidRow(
                box(plotOutput("plot4", height=250)),
                
                box(
                  radioButtons("interactions", "Interactions:",
                               c("Approaches Human" = "approaches",
                                 "Ignores Human" = "ignores",
                                 "Runs from Human" = "flees")
                  )
                  
                )
                
              )
      ), # end of second tab
      tabItem(tabName = "location",
              fluidRow(
                h2('Squirrel Locations'),
                h3('(Click on Squirrel for Squirrel ID)'),
                leafletOutput("plot5", height=250),
                p()
              ),
              fluidRow(
                checkboxGroupInput("shift", label = h3("Time of Day:"), 
                                   choices = list("AM"=1,"PM"=2),
                                   selected = 1)
              )
      ), # End of third tab
      tabItem(tabName="physical",
              fluidRow(
                box(plotOutput("plot6", height=300)),
                box(plotOutput("plot7", height=300))
              ),
              fluidRow(
                tags$img(src = "baby1.png",height=300, width=375),
                tags$img(src = "baby2.png",height=300, width=375)
              ),
              fluidRow(
                tags$img(src="squirrelsx2.png",height=300, width=750)
              )
      ),
      tabItem(tabName="moreinfo",
              fluidRow(
                h1("Think I Made This Data Up?!?")
              ),
              fluidRow(
                tags$h3("If so, ...",
                tags$a(href = "https://www.thesquirrelcensus.com/", "Click here!"))
              ),
              fluidRow(
                tags$h1('Fundamentals of Shiny Dashboard')
              ),
              fluidRow(tags$h3(tags$a(href="https://rstudio.github.io/shinydashboard/get_started.html",
                              "Get Started Creating a Shiny Dashboard")))
      )
    ) # tab items
  ) # dashboard body
) # dashboard page



server <- function(input, output) {
  url <-  'data/nyc_squirrels.csv'
  squirrels <- read.csv(url)
  squirrels <- select(squirrels,
                      Unique.Squirrel.ID,
                      X, Y,
                      Shift, Date, Age,
                      Primary.Fur.Color,
                      Highlight.Fur.Color,
                      Location, 
                      Running, Chasing, Climbing, Eating, Foraging,
                      Kuks, Quaas, Moans,
                      Tail.flags, Tail.twitches,
                      Approaches, Indifferent, Runs.from)

    output$plot1 <- renderPlot({
    xcol <- switch(input$TailBehav,
                     Flags = squirrels$Tail.flags,
                     squirrels$Tail.twitches)
    ggplot(data=squirrels, aes(x=xcol)) +
      geom_bar(fill="olivedrab1") +
      ggtitle("Observed Squirrel Tail Behavior") +
      xlab(input$TailBehav) + ylab("Count") +
      theme_minimal()
  })
  output$plot2 <- renderPlot({
    xcol <- switch(input$Vocals,
                   kuk = squirrels$Kuks,
                   quaa = squirrels$Quaas,
                   squirrels$Moans)
    ggplot(data=squirrels, aes(x=xcol)) +
      geom_bar(fill="olivedrab1") +
      ggtitle("Squirrel Vocalizations") +
      xlab(input$Vocals) + ylab("Count") +
      theme_minimal()
  })
  output$txt <- renderText({
    kukdesc <- "A kuk is a short, sharp, broad-frequency alarm sound."
    quaadesc <- "A quaa is a longer kuk with varying lengths."
    moandesc <- "A moan is tonal, like a whistle"
    vocdesc <- switch(input$Vocals,
                      kuk = kukdesc,
                      quaa = quaadesc,
                      moandesc)
    vocdesc
  })
  
  output$plot3 <- renderPlot({
    acts.of.int <-  c('Running','Chasing','Climbing','Eating','Foraging')
    sqsub <- squirrels %>%
      select(all_of(acts.of.int)) %>%
      melt(measure.vars=c('Running','Chasing',
                                           'Climbing','Eating','Foraging'))
    #filter data
    acts.of.int = acts.of.int[as.numeric(input$actions)]
    sqsub <- sqsub %>%
      filter(is.element(variable, acts.of.int)) %>%
      rename(Activity = variable)
    #plot datas
    ggplot(data=sqsub, aes(x=value, fill=Activity)) + 
      geom_bar(stat="count",position=position_dodge()) +
      xlab("Activities") +
      ylab("Occurrences") + 
      ggtitle('Squirrel Shenanigans')
  })
  
  output$plot4 <- renderPlot({
    xcol <- switch(input$interactions,
                   approaches = squirrels$Approaches,
                   indiff = squirrels$Indifferent,
                   squirrels$Runs.from)
    ggplot(data=squirrels, aes(x=xcol)) +
      geom_bar(fill="darkorchid1") +
      ggtitle("Squirrel Interactions") +
      xlab(input$interactions) + ylab("Count") +
      theme_minimal()
  })
  output$plot5 <- renderLeaflet({
    shift.of.int = c('AM','PM')
    shift.of.int = shift.of.int[as.numeric(input$shift)]
    sqsub <- squirrels %>%
      filter(is.element(Shift,shift.of.int))
    squico <- icons(
      iconUrl = ifelse(sqsub$Shift=='AM',
                       'figs/sq_am.png',
                       'figs/sq_pm.png'),
      iconWidth = 25,
      iconHeight = 25
    )

    leaflet(options = leafletOptions(minZoom = 16, maxZoom = 20)) %>%
      addTiles() %>%
      addMarkers(lng=sqsub$X, lat=sqsub$Y, popup=squirrels$Unique.Squirrel.ID,
                 icon = squico)
  })
  
  output$plot6 <- renderPlot({
    ggplot(data=squirrels, aes(x=Primary.Fur.Color)) + 
      geom_bar(stat="count",fill="chocolate4") +
      xlab("Color") +
      ylab("Occurrences") + 
      ggtitle('Squirrel Primary Fur Colors')
    
  })
  output$plot7 <- renderPlot({
    ggplot(data=squirrels, aes(x=Highlight.Fur.Color)) + 
      geom_bar(stat="count",fill="darkorange3") +
      xlab("Color") +
      ylab("Occurrences") + 
      ggtitle('Squirrel Highlight Fur Colors') + 
      theme(axis.text.x = element_text(angle = 90))
  })
  
}


shinyApp(ui, server)
