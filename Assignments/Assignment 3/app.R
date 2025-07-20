library(shiny)
library(DT)
library(ggplot2)
library(data.table)
library(CountryHealthAnalysis)

options(shiny.maxRequestSize = 30 * 1024^2)

# Preload 5 datasets manually
builtInData <- list(
    afg = readIndicators("data/indicators_afg.csv"),
    alb = readIndicators("data/indicators_alb.csv"),
    arm = readIndicators("data/indicators_arm.csv"),
    irl = readIndicators("data/indicators_irl.csv"),
    mlt = readIndicators("data/indicators_mlt.csv")
)

ui <- fluidPage(
    titlePanel("Assignment 3: Country Indicator Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("selectedCountry", "Select Country/Countries:",
                        choices = names(builtInData),
                        selected = names(builtInData)[1],
                        multiple = TRUE),
            
            fileInput("uploadFile", "Upload new indicators_COUNTRYCODE.csv", accept = ".csv"),
            
            numericInput("numRows", "Rows to display:", 10, min = 1),
            uiOutput("colSelector"),
            
            hr(),
            selectizeInput("plot1Indicator", "Plot 1: Indicator", choices = NULL),
            selectInput("plot1Agg", "Aggregation Function", choices = c("mean", "median", "sum")),
            sliderInput("plot1Year", "Year Range", min = 2000, max = 2024, value = c(2010, 2020)),
            
            hr(),
            selectizeInput("plot2Indicator", "Plot 2: Indicator", choices = NULL),
            checkboxInput("showTrend", "Show Trend Line", TRUE),
            sliderInput("plot2Year", "Year Range", min = 2000, max = 2024, value = c(2010, 2020))
        ),
        
        mainPanel(
            h4(textOutput("titleText")),
            DTOutput("dataTable"),
            tabsetPanel(
                tabPanel("Plot 1", plotOutput("plot1")),
                tabPanel("Plot 2", plotOutput("plot2"))
            )
        )
    )
)

server <- function(input, output, session) {
    dataStore <- reactiveVal(builtInData)
    
    observeEvent(input$uploadFile, {
        file <- input$uploadFile
        if (is.null(file)) return()
        
        try({
            newData <- readIndicators(file$datapath)
            iso <- tolower(unique(newData$`Country ISO3`))
            if (!is.na(iso) && nzchar(iso)) {
                updated <- dataStore()
                updated[[iso]] <- newData
                dataStore(updated)
                updateSelectInput(session, "selectedCountry", choices = names(updated))
                showNotification(paste("Loaded:", iso), type = "message")
            } else {
                showNotification("Upload failed: Missing ISO3 code in file", type = "error")
            }
        }, silent = TRUE)
    })
    
    selectedData <- reactive({
        req(input$selectedCountry)
        selected <- lapply(input$selectedCountry, function(cn) dataStore()[[cn]])
        out <- rbindlist(selected, fill = TRUE)
        out[, Value := as.numeric(Value)]
        out
    })
    
    output$titleText <- renderText({
        paste("Data for:", paste(input$selectedCountry, collapse = ", "))
    })
    
    output$colSelector <- renderUI({
        dat <- selectedData()
        if (is.null(dat) || nrow(dat) == 0) return(NULL)
        checkboxGroupInput("cols", "Columns to show:",
                           choices = names(dat),
                           selected = names(dat))
    })
    
    output$dataTable <- renderDT({
        dat <- selectedData()
        if (is.null(input$cols) || length(input$cols) == 0 || !all(input$cols %in% names(dat))) return(NULL)
        filtered <- dat[, ..input$cols]
        if (!is.data.frame(filtered)) filtered <- as.data.frame(filtered)
        datatable(head(filtered, input$numRows))
    })
    
    observe({
        inds <- unique(selectedData()$`Indicator Name`)
        updateSelectizeInput(session, "plot1Indicator", choices = inds, server = TRUE)
        updateSelectizeInput(session, "plot2Indicator", choices = inds, server = TRUE)
    })
    
    output$plot1 <- renderPlot({
        req(input$plot1Indicator)
        dat <- selectedData()
        sub <- dat[`Indicator Name` == input$plot1Indicator &
                       Year >= input$plot1Year[1] & Year <= input$plot1Year[2]]
        sub <- sub[!is.na(Value)]
        if (nrow(sub) == 0) return(NULL)
        agg <- aggregate(Value ~ Year, data = sub, FUN = match.fun(input$plot1Agg), na.rm = TRUE)
        plot(agg$Year, agg$Value, type = "b", col = "blue",
             main = paste(toupper(input$plot1Agg), "of", input$plot1Indicator),
             xlab = "Year", ylab = "Value")
    })
    
    output$plot2 <- renderPlot({
        req(input$plot2Indicator)
        dat <- selectedData()
        sub <- dat[`Indicator Name` == input$plot2Indicator &
                       Year >= input$plot2Year[1] & Year <= input$plot2Year[2]]
        sub <- sub[!is.na(Value)]
        if (nrow(sub) == 0) return(NULL)
        p <- ggplot(sub, aes(x = Year, y = Value, color = `Country Name`)) +
            geom_point(alpha = 0.6) +
            labs(title = input$plot2Indicator, y = "Value", x = "Year") +
            theme_minimal()
        if (input$showTrend) {
            p <- p + geom_smooth(method = "lm", se = FALSE)
        }
        print(p)
    })
}

shinyApp(ui, server)
