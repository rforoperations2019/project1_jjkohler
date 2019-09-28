library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(RColorBrewer)
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
            menuItem("Bigfoot at a Glance", icon = icon("tree"), tabName = "intro"),
            menuItem("Bigfeet Plots", icon = icon("bar-chart"), tabName = "plot"),
            menuItem("Sasquatch Table", icon = icon("table"), tabName = "table"),
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
            
            prettyCheckboxGroup(inputId='season', label= h4("Season of Sighting"), choices = c('Spring','Summer','Fall','Winter'), 
                                selected = c('Spring','Summer','Fall','Winter'),
                                status = "default", shape = "curve",
                                outline = FALSE, fill = FALSE, thick = TRUE, animation = 'pulse',
                                icon = NULL, plain = FALSE, bigger = FALSE, inline = FALSE,
                                width = NULL, choiceNames = NULL, choiceValues = NULL),
            checkboxGroupButtons(
                inputId = "class_button", label = h4("Choose Sighting Class :"), 
                choices = c("Class A", "Class B"),
                selected = c("Class A", "Class B"),
                justified = TRUE, status = "primary",
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
            
            # # Input and Value Boxes ----------------------------------------------
            # fluidRow(
            #     infoBoxOutput("mass"),
            #     valueBoxOutput("height")
            # ),
            # 
            # Plot ----------------------------------------------
            fluidRow(
                box(plotlyOutput("plot_state"), width = 8)),
            fluidRow(
                box(plotlyOutput("plot_year"), width = '7'),
                box(plotlyOutput("plot_month"), width = '4'))
                # tabBox(title = "Plot",
                #        width = 8,
                #        tabPanel("State", plotlyOutput("plot_state")),
                #        tabPanel("Height", plotlyOutput("plot_year")))
            
    ),
    
    # Data Table Page ----------------------------------------------
    tabItem("table",
            fluidPage(
                box(title = "Reported Bigfoot Sightings", DT::dataTableOutput("table"), width = 12))
    )
)
)

ui <- dashboardPage(header, sidebar, body, skin='green')

# Define server function -----
server <- function(input, output) {

#Error if no Season is chosen
        observeEvent(input$season, ignoreNULL = FALSE,{
            if (length(input$season)==0) {
            shinyalert("Oh No!", "The Sasquatch may just hibernate or go south if you don't choose a season.", type = "warning")
        }
      }
    )
    
#Error if no Class is chosen 
        observeEvent(input$class_button, ignoreNULL = FALSE,{
            if (length(input$class_button)==0) {
            shinyalert("Oops!", "You won't have any sightings if you don't pick at least one class.", type = "error")
        }
      }
    )
#Render Sasquatch image for landing page
        
    output$image <- renderImage({
            return(list(
                src = "sasquatch.jpg",
                contentType = 'image/jpeg',
                alt = "Is this Bigfoot?"
            ))
        }, deleteFile = FALSE)

#Filter data table from inputs--------------------------
    
    data <- reactive({
        #Check for existence of inputs
        validate(
            need(input$class_button, "Select at least one Class"),
            need(input$season, "Select at least one Season")
        )
        sub <- subset(bigfoot, select = c('state','season','date','title','classification','month', 'year')) 
        colnames(sub) <- c('State', 'Season', 'Date', 'Description', 'Class', 'Month','Year')
        sub <- sub[(sub['Year'] >= input$slider2[1]) & (sub['Year'] <= input$slider2[2]),]
        sub <- subset(sub, Class %in% input$class_button)
        sub <- subset(sub, Season %in% input$season)
        sub
    
       }
    )
    
# Aggregating sightings by state for plotting
    state_count <- reactive({count <- aggregate(x = data()[c('State','Year')],
                               by = list(states = data()$State),FUN = length)
                            colnames(count) <- c('State', 'Count','Year')
                            count <- count[order(count$Year),]
                            count

    })
    
# Aggregating sightings by year for plotting
    year_count <- reactive({count <- aggregate(x = data()[c('State','Year')],
                                                by = list(years = data()$Year),FUN = length)
    colnames(count) <- c('Year', 'Count','State')
    count <- count[order(count$Year),]
    count
    
    })
    
# Aggregating sightings by year for plotting
    month_count <- reactive({count <- aggregate(x = data()[c('State','Month')],
                                               by = list(months = data()$Month),FUN = length)
    colnames(count) <- c('Month1','Count','State')
    count <- count[order(count$Month1),]
    count$Month <- month.abb[count$Month1]
    count

    })
     
    # Data table of Bigfoot Sightings ----------------------------------------------
    output$table <- DT::renderDataTable({
        data()
    })



# A plot showing sightings by state -----------------------------    
    output$plot_state <- renderPlotly({
      ggplotly(  
        p1 <- ggplot(state_count(), aes(x = reorder(State, -Count), y = Count, label=State)) +
            geom_bar(stat = "identity", width = 0.8, color='darkgreen', fill='forestgreen')+
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            ggtitle('Bigfoot Sightings By State')+
            xlab("State") + 
            ylab("Count of Sightings"), tooltip=c('label','y')
      )
    })

# A plot showing sightings by year -----------------------------    
    output$plot_year <- renderPlotly({
        ggplotly(  
            p2 <- ggplot(year_count(), aes(x = Year, y = Count, label=Year)) +
                geom_line(color='forestgreen') + geom_point(color='saddlebrown', stroke='forestgreen') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle('Yearly Sasquatches')+
                xlab("Year") + 
                ylab("Count of Sightings"), tooltip=c('label','y')
        )
    })

    # A plot showing sightings by month -----------------------------    
    output$plot_month<- renderPlotly({
        ggplotly(  
            p3 <- ggplot(month_count(), aes(x = reorder(Month, -Month1), y = Count, label=Month)) +
                geom_bar(stat = "identity", width = 0.8, color='darkgreen', fill='saddlebrown')+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle("Bigfoot's Favorite Month")+
                xlab("Month") + 
                ylab("Count of Sightings") +
                coord_flip(), tooltip=c('label','y') 
        )
    })
    
    
    
    # output$plot_month<- renderPlot({
    #     theme_set(theme_classic())
    # p <- ggplot(month_count(), aes(x= Month, y= Count)) +
    #     geom_point(col="tomato2", size=3)   # Draw points
    # # geom_segment(aes(x=Month,
    # #                  xend=Month,
    # #                  y=min(Count),
    # #                  yend=max(Count)),
    # #              linetype="dashed",
    # #              size=0.1) +   # Draw dashed lines
    # # labs(title="Dot Plot",
    # #      subtitle="Make Vs Avg. Mileage",
    # #      caption="") +
    # # coord_flip()
    # })
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