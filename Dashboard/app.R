library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
bigfoot <- read.csv("bigfoot.csv", check.names=FALSE)
# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Bigfoot Sightings",
                          dropdownMenu(type = "messages",
                                       notificationItem(text = "New Sasquatch sighting!", 
                                                        icon = icon("users"))
                          )
                          
                          
)

    # Sidebar with a slider input for number of bins 
    # Dashboard Sidebar ----------------------------------------------
    sidebar <- dashboardSidebar(
        useShinyalert(),
        sidebarMenu(
            id = "tabs",
            
            # # Menu Items ----------------------------------------------
            menuItem("Intro", icon = icon("tree"), tabName = "intro"),
            menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
            menuItem("Table", icon = icon("table"), tabName = "table"),
            # 
            # # Inputs: select variables to plot ----------------------------------------------
            # selectInput("worldSelect",
            #             "Homeworld:",
            #             choices = sort(unique(starwars.load$homeworld)),
            #             multiple = TRUE,
            #             selectize = TRUE,
            #             selected = c("Naboo", "Tatooine")),
            # 
            # # Birth year Selection ----------------------------------------------
            chooseSliderSkin("HTML5", color="SaddleBrown"),
            sliderInput("slider2", label = h4("Sighting Years"), min = 1920, 
                        max = 2018, value = c(1920, 2018), sep = ''),
            checkboxGroupButtons(
                inputId = "class_button", label = "Make a choice :", 
                choices = c("Class A", "Class B"),
                selected = c("Class A", "Class B"),
                justified = TRUE, status = "primary",
                color = 'green',
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                 no = icon("remove", lib = "glyphicon"))
            ),
            bsTooltip(id = "class_button", title = "<strong>Class A</strong><br> <i>Reports of clear sightings.</i><br><br> <strong>Class B</strong><br> <i>Observations without a clear view.</i>",
                      placement = "bottom", trigger = "hover")
        )
    )



# Dashboard body ----------------------------------------------
body <- dashboardBody(
    tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css?family=Mansalva&display=swap');
      .main-header .logo {
        font-family: 'Mansalva', cursive;
        font-weight: bold;
        font-size: 24px;
      }
    ")))
    ,
    tabItems(
    tabItem("intro",
            fluidPage(
                fluidRow(
                imageOutput("image")
                    # box(
                    #     title = "In the Forest", status = "primary", solidHeader = TRUE,
                    #     imageOutput("image"), width = 4
                    # )
                ),
            
            fluidRow(
                infoBoxOutput("count"),
                infoBoxOutput("month"),
                valueBoxOutput("state")
            )
            )
    )
            ,
    # Plot page ----------------------------------------------
    tabItem("plot",
            
            # Input and Value Boxes ----------------------------------------------
            fluidRow(
                infoBoxOutput("mass"),
                valueBoxOutput("height")
            ),
            
            # Plot ----------------------------------------------
            fluidRow(
                tabBox(title = "Plot",
                       width = 8,
                       tabPanel("State", plotlyOutput("plot_state")),
                       tabPanel("Height", plotlyOutput("plot_height")))
            )
    ),
    
    # Data Table Page ----------------------------------------------
    tabItem("table",
            fluidPage(
                box(title = "Reported Bigfoot Sightings", DT::dataTableOutput("table"), width = 12))
    )
)
)

ui <- dashboardPage(header, sidebar, body, skin='green')

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    

        observeEvent(input$class_button, ignoreNULL = FALSE,{
            if (length(input$class_button)==0) {
                cat('something is happening')
            shinyalert("Oops!", "You won't have any sightings if you don't pick at least one class.", type = "error")
            }

        }
        )

    output$image <- renderImage({
            return(list(
                src = "sasquatch.jpg",
                contentType = 'image/jpeg',
                alt = "Is this Bigfoot?"
            ))
        }, deleteFile = FALSE)

    
    data <- reactive({
        validate(
            need(input$class_button, "Select at least one class")
        )
        sub <- subset(bigfoot, select = c('state','season','date','title','classification', 'year')
    ) 
        colnames(sub) <- c('State', 'Season', 'Date', 'Description', 'Class', 'Year')
        sub <- sub[(sub['Year'] >= input$slider2[1]) & (sub['Year'] <= input$slider2[2]),]
        sub <- subset(sub, Class %in% input$class_button)
        sub
    
    }
    )
    
    # Aggregating from to State Level
    state_count <- reactive({count <- aggregate(x = data()[c('State','Year')],
                               by = list(states = data()$State),FUN = length)
                            count <- count[order(count$Year),]
                            count

    })
     
    # Data table of characters ----------------------------------------------
    output$table <- DT::renderDataTable({
        data()
    })

    # A plot showing the mass of characters -----------------------------
    # output$plot_state <- renderPlotly({
    #     p <- plot_ly(
    #         x = state_count()$states,
    #         y = state_count()$Year,
    #         name = "SF Zoo",
    #         type = "bar"
    #     )
    #     
    # 
    # })
    
    output$plot_state <- renderPlotly({
      ggplotly(  
        p1 <- ggplot(state_count(), aes(x = reorder(states, -Year), y = Year)) + geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      )
    })



    
    
    # Mass mean info box ----------------------------------------------
    output$count <- renderInfoBox({
        sw <- swInput()
        num <- round(mean(sw$mass, na.rm = T), 2)
        

        
        infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
    })
    
    output$state <- renderInfoBox({
        sw <- swInput()
        num <- round(mean(sw$mass, na.rm = T), 2)
        
        
        
        infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
    })
    
    # Height mean value box ----------------------------------------------
    output$height <- renderValueBox({
        sw <- swInput()
        num <- round(mean(sw$height, na.rm = T), 2)
        
        valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
    })



    
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)