################################################################################
# Main Server Function
################################################################################

server <- function(input, output, session) {
    # Initialize reactive values
    dataStore <- reactiveVal(builtInData)
    
    # Color palette helper function to chsnge plotting colors for the plots
    getColors <- function(palette, n = 1) {
        switch(palette,
               "set3" = RColorBrewer::brewer.pal(n = max(3, min(12, n)), name = "Set3")[1:n],
               "blues" = RColorBrewer::brewer.pal(n = max(3, n), name = "Blues")[1:n],
               "default" = if(n == 1) "#2c3e50" else c("#2c3e50", "#3498db"))
    }
    
    # retrieve the selected data
    selectedData <- reactive({
        req(input$selectedCountry)
        dataStore()[[input$selectedCountry]]
    })
    
    # Update indicator choices
    observe({
        inds <- unique(selectedData()$`Indicator Name`)
        updateSelectizeInput(session, "plot1Indicator", choices = inds, server = TRUE)
        updateSelectizeInput(session, "plot2Indicator", choices = inds, server = TRUE)
        updateSelectizeInput(session, "plot3Indicator", choices = inds, server = TRUE)
    })
    
    # Handle file uploads
    observeEvent(input$uploadFile, {
        file <- input$uploadFile
        if (is.null(file)) return()
        
        # must add try catching blocks to prevent error cascading through.
        tryCatch({
            newData <- readIndicators(file$datapath)
            countryName <- unique(newData$`Country Name`)[1]
            
            if (!is.na(countryName) && nzchar(countryName)) {
                updated <- dataStore()
                updated[[countryName]] <- newData
                dataStore(updated)
                updateSelectInput(session, "selectedCountry", choices = names(updated))
                showNotification(paste("Loaded:", countryName), type = "message")
            } else {
                showNotification("Upload failed: Missing country name in file", type = "error")
            }
        }, error = function(e) {
            showNotification(paste("Error loading file:", e$message), type = "error")
        })
    })
    
    # Updating title according to requirements
    output$titleText <- renderText({
        paste("Data for:", input$selectedCountry)
    })
    
    # Generate column selector
    output$colSelector <- renderUI({
        dat <- selectedData()
        if (is.null(dat) || nrow(dat) == 0) return(NULL)
        checkboxGroupInput("cols", "Columns to show:",
                         choices = names(dat),
                         selected = names(dat))
    })
    
    # Render data table post selection
    output$dataTable <- renderDT({
        dat <- selectedData()
        req(input$cols)
        req(all(input$cols %in% names(dat)))
        
        filtered <- dat[, .SD, .SDcols = input$cols]
        datatable(
            head(filtered, input$numRows),
            options = list(
                pageLength = input$numRows,
                scrollX = TRUE,
                dom = 'rtip',
                language = list(
                    info = "Showing _START_ to _END_ of _TOTAL_ entries"
                )
            )
        )
    })
    
    
    
    
    
    
    ############################################################################
    # Render Time Series plot for the first plot
    ############################################################################
    
    output$plot1 <- renderPlot({
        req(input$plot1Indicator)
        dat <- selectedData()
        
        # Filter data
        sub <- dat[`Indicator Name` == input$plot1Indicator &
                   Year >= input$plot1Year[1] & Year <= input$plot1Year[2]]
        
        if (nrow(sub) == 0) {
            return(ggplot() + 
                   annotate("text", x = 1, y = 1, label = "No data available for selected range") +
                   theme_void())
        }
        
        # Get colors from the helper function
        colors <- getColors(input$plot1ColorPalette, 1)
        
        # Create base plot with no selections
        if (input$plot1Agg == "none") {
            # Raw values
            p <- ggplot(sub, aes(x = Year, y = Value)) +
                geom_line(color = colors[1], linewidth = 1) +
                geom_point(color = colors[1], size = 3) +
                labs(title = paste("Values for", input$plot1Indicator),
                     x = "Year", y = "Value")
            
        } else if (input$plot1Agg == "running_mean") {
            # Calculating the running mean
            setorder(sub, Year)
            sub[, RunningMean := frollmean(Value, n = 5, align = "right")]
            p <- ggplot(sub, aes(x = Year, y = RunningMean)) +
                geom_line(color = colors[1], linewidth = 1) +
                geom_point(color = colors[1], size = 3) +
                labs(title = paste("5-Year Running Mean of", input$plot1Indicator),
                     x = "Year", y = "5-Year Running Mean")
            
        } else if (input$plot1Agg == "cumsum") {
            # Calculate cumulative sum
            setorder(sub, Year)
            sub[, CumulativeSum := cumsum(Value)]
            p <- ggplot(sub, aes(x = Year, y = CumulativeSum)) +
                geom_line(color = colors[1], linewidth = 1) +
                geom_point(color = colors[1], size = 3) +
                labs(title = paste("Cumulative Sum of", input$plot1Indicator),
                     x = "Year", y = "Cumulative Sum")
            
        } else if (input$plot1Agg == "yoy") {
            # Calculate year-over-year change
            setorder(sub, Year)
            sub[, YoYChange := (Value - shift(Value)) / shift(Value) * 100]
            p <- ggplot(sub, aes(x = Year, y = YoYChange)) +
                geom_line(color = colors[1], linewidth = 1) +
                geom_point(color = colors[1], size = 3) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                labs(title = paste("Year-over-Year % Change in", input$plot1Indicator),
                     x = "Year", y = "Change (%)")
        }
        
        # Add common theme elements
        p + theme_minimal() +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                panel.grid.major = element_line(color = "#e9ecef"),
                panel.grid.minor = element_blank()
            )
    })
    
    
    
    

    
    
    
    ############################################################################
    # Render Top/Bottom Values plot
    ############################################################################
    
    output$plot2 <- renderPlot({
        req(input$plot2Indicator)
        dat <- selectedData()
        
        sub <- dat[`Indicator Name` == input$plot2Indicator &
                  Year >= input$plot2Year[1] & Year <= input$plot2Year[2]]
        
        if (nrow(sub) == 0) {
            return(ggplot() + 
                   annotate("text", x = 1, y = 1, label = "No data available for selected range") +
                   theme_void())
        }
        
        # Get top/bottom values
        plotData <- if(input$plot2Order == "top") {
            sub[order(-Value)][1:min(10, .N)]
        } else {
            sub[order(Value)][1:min(10, .N)]
        }
        
        # Create plot
        colors <- getColors(input$plot2ColorPalette, 1)
        
        ggplot(plotData, aes(x = reorder(as.character(Year), Value), y = Value)) +
            geom_col(fill = colors[1], alpha = 0.7) +
            geom_text(aes(label = round(Value, 2)), 
                     vjust = ifelse(plotData$Value >= 0, -0.5, 1.5),
                     color = "#2c3e50", size = 3.5) +
            labs(
                title = paste(if(input$plot2Order == "top") "Highest" else "Lowest",
                            "Values for", input$plot2Indicator),
                x = "Year",
                y = "Value"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major = element_line(color = "#e9ecef"),
                panel.grid.minor = element_blank()
            )
    })
    
    
    
    
    
    
    
    
    
    ############################################################################
    # Render Rolling Average plot
    ############################################################################
    
    output$plot3 <- renderPlot({
        req(input$plot3Indicator)
        dat <- selectedData()
        
        sub <- dat[`Indicator Name` == input$plot3Indicator &
                  Year >= input$plot3Year[1] & Year <= input$plot3Year[2]]
        
        if (nrow(sub) == 0) {
            return(ggplot() + 
                   annotate("text", x = 1, y = 1, label = "No data available for selected range") +
                   theme_void())
        }
        
        # Calculate rolling mean
        windowSize <- input$plot3Window
        setorder(sub, Year)
        sub[, RollingMean := frollmean(Value, n = windowSize, align = "right")]
        
        # Get colors
        colors <- getColors(input$plot3ColorPalette, 2)
        
        # Create plot
        ggplot(sub, aes(x = Year)) +
            geom_line(aes(y = Value, color = "Actual"), linewidth = 1, alpha = 0.7) +
            geom_point(aes(y = Value, color = "Actual"), size = 2) +
            geom_line(aes(y = RollingMean, color = "Rolling Average"), 
                     linewidth = 1.2, linetype = "dashed") +
            scale_color_manual(
                name = "Series",
                values = c("Actual" = colors[1], "Rolling Average" = colors[2])
            ) +
            labs(
                title = paste(windowSize, "Year Rolling Average for", input$plot3Indicator),
                x = "Year",
                y = "Value"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                legend.position = "top",
                panel.grid.major = element_line(color = "#e9ecef"),
                panel.grid.minor = element_blank()
            )
    })
} 