#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(purrr)
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(shinyWidgets)
library(scales)
library(tidyverse)

library(shiny)
library(tidyr)


PARS <- list(
    debug = FALSE,
    classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
    sparkline_color = "#333333",
    font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

options(
    highcharter.google_fonts = FALSE,
    highcharter.debug = PARS$debug,
    # shiny.launch.browser = PARS$debug,
    highcharter.theme = 
        hc_theme_smpl(
            title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
            subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
            chart = list(
                backgroundColor = "transparent",
                style = list(fontFamily = PARS$font, fontSize = "1.0em")
            ),
            plotOptions = list(
                series = list(
                    dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
                    animation = list(duration = 3000)
                )
            ),
            legend = list(
                itemStyle =  list(
                    fontWeight = "normal"
                )
            )
        )
)

dropdownButtonp <- purrr::partial(
    dropdownButton,
    status = "customstatus",
    size = "sm",
    right = TRUE,
    status = "info",
    width = "400px",
    inline = TRUE,
)
#spark


#theme
hc_theme_sparkline_vb <- function(...) {
    
    theme <- list(
        chart = list(
            backgroundColor = NULL,
            margins = c(0, 0, 0, 0),
            spacingTop = 0,
            spacingRight = 0,
            spacingBottom = 0,
            spacingLeft = 0,
            plotBorderWidth = 0,
            borderWidth = 0,
            style = list(overflow = "visible")
        ),
        xAxis = list(
            visible = F, 
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        yAxis = list(
            visible = F,
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        tooltip = list(
            outside = FALSE,
            shadow = FALSE,
            borderColor = "transparent",
            botderWidth = 0,
            backgroundColor = "transparent",
            style = list(textOutline = "5px white")
        ),
        plotOptions = list(
            series = list(
                marker = list(enabled = FALSE),
                lineWidth = 2,
                shadow = FALSE,
                fillOpacity = 0.0,
                color = "#FFFFFFBF",
                fillColor = list(
                    linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
                    stops = list(
                        list(0.00, "#FFFFFF00"),
                        list(0.50, "#FFFFFF7F"),
                        list(1.00, "#FFFFFFFF")
                    )
                )
            )
        ),
        credits = list(
            enabled = FALSE,
            text = ""
        )
    )
    theme <- structure(theme, class = "hc_theme")
    
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(
            theme,
            hc_theme(...)
        )
    }
    
    theme
}

valueBoxSpark <- function(value, subtitle, icon = NULL, color = "aqua", 
                          width = 4, href = NULL, spark = NULL, height_spark = "150px",minititle = NULL) {
    
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon)) 
        shinydashboard:::tagAssert(icon, type = "i")
    
    boxContent <- div(
        class = paste0("small-box bg-", color),
        div(
            class = "inner",
            if(!is.null(minititle)) tags$small(minititle),
            h3(value),
            # tags$span(style = paste0("height:", height_spark), hc_size(spark, height = "100vh")),
            tags$span(hc_size(spark, height = height_spark)),
            if (!is.null(subtitle)) p(subtitle)
        ),
        if (!is.null(icon)) div(class = "icon-large", icon)
    )
    
    if (!is.null(href)) 
        boxContent <- a(href = href, boxContent)
    
    div(class = if (!is.null(width)) 
        paste0("col-sm-", width), boxContent)
}
#health data
health=read.csv("health.csv")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin="green",
    dashboardHeader(
        title = "HEALTH SECTOR DASHBOARD", titleWidth = 500,
        #subtitle = "GOVERNMENT OF KENYA", subtitleWidth = 500,
        tags$li(actionLink("LinkedIn", 
                           label = "", 
                           icon = icon("twitter"),
                           onclick = "window.open('https://twitter.com/dankilemi')"),
                class = "dropdown"),
        tags$li(actionLink("Facebook", 
                           label = "", 
                           icon = icon("linkedin"),
                           onclick = "window.open('https://www.linkedin.com/in/kilemi-dan-a7b259201/')"),
                class = "dropdown")
        
    ), #disable = TRUE
    dashboardSidebar(
        disable = TRUE
        
    ),
    
    
    dashboardBody(
        #tags$head(
        #    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
        #),
        #tags$div(
        #    style = "position: absolute; width: 100%; left: 0; z-index: 0; height: 90vh; position:fixed",
        #    mapwrdl
        #),
        
        #fluidRow(
        #   column(
        #       width = 12,
        #        class = PARS$classcol,
        #        tags$h2(class = "specialfont", "THE GOVERNMENT OF KENYA"),
        #        tags$em("KENYA ECONOMIC DASHBOARD")
        #    ),
        #),
        
        #       tabItem(
        #           tabName = "Overview",
        
        
        fluidRow(
            tabBox(
                title = NULL, width = 12,
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("Health",
                         fluidRow(
                             column(width = 10,
                                    class = "col-lg-offset-4 col-lg-4 col-md-offset-2 col-md-8 col-sm-offset-1 col-sm-10",
                                    selectInput(
                                        inputId = "Item",
                                        label = "Select Health Indicator",
                                        choices =c(
                                            "Health Facilities"="Facilities",
                                            "Level 2 Hospitals"="Level 2",
                                            "Level 3 Hospitals"="Level 3",
                                            "Level 4 Hospitals"="Level 4",
                                            "Level 5 Hospitals"="Level 5",
                                            "Leve 6 Hospitals"="Level 6",
                                            "Hospitals Beds"="Beds",
                                            "Normal Beds"="Normal Beds",
                                            "ICU Beds"="ICU Beds",
                                            "Dialysis Machines"="Dialysis",
                                            "MRI Machines"="MRI",
                                            "X RAY Machines"="Xray",
                                            "CT SCANS"="CT",
                                            "Ultrasound Machines"="Ultrasound",
                                            "Theatres"="Theatres",
                                            "Health Personnel"="Personnel",
                                            "Doctors"="Doctors",
                                            "Murses"="Mursing",
                                            "Clinical Officers"="Clinical Officer",
                                            "Community Health Workers"="CHW",
                                            "Dentists"="Dental",
                                            "Annual Health Institutions Graduates"="Graduates",
                                            "Health Campuses"="Campuses",
                                            "Counties with Health Facilities"="Counties",
                                            "CT Scan Charges"="CT Charges",
                                            "XRAY Charges"="Xray Charges",
                                            "MRI Charges"="MRI Charges",
                                            "Dialysis Charges"="Dialysis Charges",
                                            "Ultrasound Charges"="Ultrasound Charges",
                                            "Delivery Charges"="Delivery Charges",
                                            "Intensice Care Unit Charges"="ICU Charges"
                                            
                                        ),selected = "Facilities"
                                        
                                    ) 
                                    
                                    
                             ),
                             column(
                                 width = 2,
                                 dropdownButtonp(
                                     tags$h4("About this app"),
                                     "This is an app developed by Kilemi Daniel",
                                     icon = icon("info"),
                                     tooltip = tooltipOptions(title = "About this app")
                                 )
                             )
                             
                         ), #end fluid row
                         fluidRow(
                             column(width = 12,
                                    
                                    valueBoxOutput(
                                        "facility"
                                        
                                    ),
                                    valueBoxOutput(
                                        "facility_p"
                                        
                                    ),
                                    valueBoxOutput(
                                        "facility_g"
                                        
                                    )
                             )
                         ),
                         fluidRow(
                             #class = "top-buffer",
                             column(
                                 # offset = 2,
                                 width = 12,
                                 #class = PARS$classcol,
                                 valueBoxOutput("expe", 4),
                                 valueBoxOutput("maternal", 4),
                                 valueBoxOutput("child", 4),
                                 #imageOutput("img1", 3)
                             )
                             
                         )
                         
                         
                         
                )
            )
        )
        #fluidRow(infoBoxOutput("tabset1Selected"))
        
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$tabset1Selected <- renderInfoBox({
        infoBox("Selected Tab", input$tabset1, icon = icon("info-circle"))
    })
    
    #health
    #2013 figures    
    output$facility<-renderValueBox({
        valueBox(health%>%
                     filter(Item==input$Item) %>%
                     summarise("Mean" = format(round(mean(Y2013),digits=0),big.mark=",",
                                               scientific=FALSE)),
                 "2013",icon=icon('info'),color="purple"
        )
    })
    #2021 figures
    output$facility_p<-renderValueBox({
        valueBox(health%>%
                     filter(Item==input$Item) %>%
                     summarise("Mean" = format(round(mean(Y2021),digits=0),big.mark=",",
                                               scientific=FALSE)),
                 "2021",icon=icon('info'),color="blue"
        )
    })
    #increase/change
    output$facility_g<-renderValueBox({
        valueBox(health%>%
                     filter(Item==input$Item) %>%
                     summarise("Mean" = paste0(format(round(mean(Percent),digits=0),big.mark=",",
                                                      scientific=FALSE),"%")),
                 "Change in Percentage",icon=icon('info'),color="green"
        )
    })
    
    output$expe <- renderValueBox({
        expectancy=read.csv("expectancy.csv")
        d <- expectancy %>%
            
            select(Year, Life.Expectancy) %>% 
            #mutate(gdp_value = round(gdp_contribution/1e0, 2)) %>% 
            select(x = Year, y = Life.Expectancy)
        
        lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0(.,"")
        
        hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color 
            hc_add_theme(hc_theme_sparkline_vb()) %>% 
            hc_tooltip(valuePrefix = " ", valueSuffix = "")
        
        valueBoxSpark(
            value = lbl,
            subtitle = "Kenya's Life Expectancy 1960-2020",
            color = "olive",
            spark = hc,
            minititle = "Life Expectancy 2022"
        )
        
    })
    
    output$maternal <- renderValueBox({
        maternal=read.csv("maternal.csv")
        d <- maternal %>%
            
            select(Year, Maternal) %>% 
            #mutate(gdp_value = round(gdp_contribution/1e0, 2)) %>% 
            select(x = Year, y =Maternal)
        
        lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0(.,"")
        
        hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color 
            hc_add_theme(hc_theme_sparkline_vb()) %>% 
            hc_tooltip(valuePrefix = " ", valueSuffix = "")
        
        valueBoxSpark(
            value = lbl,
            subtitle = "Maternal Mortality Rates 2000-2017",
            color = "blue",
            spark = hc,
            minititle = "Maternal Mortality 2022"
        )
        
    })
    output$child <- renderValueBox({
        expectancy2=read.csv("expectancy.csv")
        d <- expectancy2 %>%
            
            select(Year, Under.Five.Mortality) %>% 
            #mutate(gdp_value = round(gdp_contribution/1e0, 2)) %>% 
            select(x = Year, y = Under.Five.Mortality)
        
        lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0(.,"")
        
        hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color 
            hc_add_theme(hc_theme_sparkline_vb()) %>% 
            hc_tooltip(valuePrefix = " ", valueSuffix = "")
        
        valueBoxSpark(
            value = lbl,
            subtitle = "Kenya's Child Mortality 1960-2020",
            color = "light-blue",
            spark = hc,
            minititle = "Child Mortality 2022"
        )
        
    })
    
}

#expectancy charts

# Run the application 
shinyApp(ui = ui, server = server)
