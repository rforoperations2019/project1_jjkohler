library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
bigfoot <- read.csv("bigfoot.csv", check.names=FALSE)
# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Bigfoot Sightings"
                          
                          
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
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            )
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
              
                    box(
                        title = "Alignment", status = "primary", solidHeader = TRUE,
                        imageOutput("image")
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
                       width = 12,
                       tabPanel("Mass", plotlyOutput("plot_mass")),
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
    
    # observeEvent(input$class_button, {
    #     if (isFALSE(input$class_button)) {
    #                 # Show a modal when the button is pressed
    #     shinyalert("Oops!", "Something went wrong.", type = "error")
    #     }
        
        observeEvent(input$class_button, {
            if (length(input$class_button)==0) {
            shinyalert("Oops!", "Something went wrong.", type = "error")
            }
        }
        )
    # })
    output$image <- renderImage({
            return(list(
                src = "sasquatch.jpg",
                filetype = "image/jpeg",
                alt = "Is this Bigfoot?"
            ))
        },deleteFile = FALSE)
        
    # output$picture <- renderImage({return(list(src = "sasquatch.jpg",contentType = "image/jpg",
    #                                            alt = "Alignment"))}, deleteFile = FALSE)
    
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
     
    # # Reactive data function -------------------------------------------
    # swInput <- reactive({
    #     starwars <- starwars.load %>%
    #         
    #         # Slider Filter ----------------------------------------------
    #     filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    #     
    #     # Homeworld Filter ----------------------------------------------
    #     if (length(input$worldSelect) > 0 ) {
    #         starwars <- subset(starwars, homeworld %in% input$worldSelect)
    #     }
    #     
    #     # Return dataframe ----------------------------------------------
    #     return(starwars)
    # })
    # 
    # # Reactive melted data ----------------------------------------------
    # mwInput <- reactive({
    #     swInput() %>%
    #         melt(id = "name")
    # })
    
    # A plot showing the mass of characters -----------------------------
    output$plot_mass <- renderPlotly({
        dat <- subset(mwInput(), variable == "mass")
        
        # Generate Plot ----------------------------------------------
        ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
    })
    
    # A plot showing the height of characters -----------------------------------
    output$plot_height <- renderPlotly({
        dat <- subset(mwInput(),  variable == "height")
        ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
    })
    
    # Data table of characters ----------------------------------------------
    output$table <- DT::renderDataTable({
        data()
    })
    
    # Mass mean info box ----------------------------------------------
    output$mass <- renderInfoBox({
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