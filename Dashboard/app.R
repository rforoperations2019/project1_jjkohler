library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
bigfoot <- read.csv("bigfoot.csv", check.names=FALSE)
# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Bigfoot Sightings"
                          
                          
)

    # Sidebar with a slider input for number of bins 
    # Dashboard Sidebar ----------------------------------------------
    sidebar <- dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            
            # # Menu Items ----------------------------------------------
            # menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
            menuItem("Table", icon = icon("table"), tabName = "table")
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
            # sliderInput("birthSelect",
            #             "Birth Year:",
            #             min = min(starwars.load$birth_year, na.rm = T),
            #             max = max(starwars.load$birth_year, na.rm = T),
            #             value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)),
            #             step = 1)
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
    data <- reactive({sub <- subset(bigfoot, select = c('state','season','date','title','classification','number')
    ) 
    colnames(sub) <- c('State', 'Season', 'Date', 'Description', 'Class','Number')
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