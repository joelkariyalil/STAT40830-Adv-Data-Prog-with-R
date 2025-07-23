################################################################################
# Main Server Function
################################################################################

server <- function(input, output, session) {
    
    ############################################################################
    # Initialize Reactive Values and Data Store
    ############################################################################
    
    dataStore <- reactiveVal(builtInData)
    
    ############################################################################
    # Helper Functions
    ############################################################################
    
    # Color palette helper function to change plotting colors for the plots
    getColors <- function(palette, n = 1) {
        switch(palette,
               "set3" = RColorBrewer::brewer.pal(n = max(3, min(12, n)), name = "Set3")[1:n],
               "blues" = RColorBrewer::brewer.pal(n = max(3, min(9, n)), name = "Blues")[1:n],
               "default" = {
                   if(n == 1) "#2c3e50" 
                   else if(n <= 8) RColorBrewer::brewer.pal(n = max(3, n), name = "Set2")[1:n]
                   else rainbow(n)
               })
    }
    
    ############################################################################
    # Data Reactive Functions
    ############################################################################
    
    # retrieve the selected data for multiple countries
    selectedData <- reactive({
        req(input$selectedCountry)
        req(length(input$selectedCountry) > 0)
        
        # Combine data from all selected countries
        allData <- rbindlist(lapply(input$selectedCountry, function(country) {
            dataStore()[[country]]
        }), fill = TRUE)
        
        return(allData)
    })
    
    ############################################################################
    # Update Indicator Choices for Plot Controls
    ############################################################################
    
    # Update indicator choices
    observe({
        inds <- unique(selectedData()$`Indicator Name`)
        
        # Set the default to the indicator with least missing values
        default_indicator <- "Fertilizer consumption (% of fertilizer production)"
        
        # Use the best indicator if it exists in the data, otherwise use the first one
        selected_indicator <- if(default_indicator %in% inds) default_indicator else inds[1]
        
        updateSelectizeInput(session, "plot1Indicator", choices = inds, 
                           selected = selected_indicator, server = TRUE)
        updateSelectizeInput(session, "plot2Indicator", choices = inds, 
                           selected = selected_indicator, server = TRUE)
        updateSelectizeInput(session, "plot3Indicator", choices = inds, 
                           selected = selected_indicator, server = TRUE)
    })
    
    ############################################################################
    # File Upload Handler
    ############################################################################
    
    # Handle file uploads
    observeEvent(input$uploadFile, {
        file <- input$uploadFile
        if (is.null(file)) return()
        
        # must add try catching blocks to prevent error cascading through.
        tryCatch({
            newData <- readIndicators(file$datapath)
            countryName <- unique(newData$`Country Name`)[1]
            
            if (!is.na(countryName) && nzchar(countryName)) {
                # Preserve current state
                currentSelection <- input$selectedCountry
                currentCols <- input$cols
                currentRows <- input$numRows
                
                # Update data store
                updated <- dataStore()
                updated[[countryName]] <- newData
                dataStore(updated)
                
                # Update choices while preserving current selection
                newChoices <- names(updated)
                preservedSelection <- if(is.null(currentSelection)) newChoices[1] else currentSelection
                
                # Add the new country to selection if not already present
                if(!countryName %in% preservedSelection) {
                    preservedSelection <- c(preservedSelection, countryName)
                }
                
                updateSelectInput(session, "selectedCountry", 
                                choices = newChoices,
                                selected = preservedSelection)
                
                showNotification(paste("Successfully loaded:", countryName), type = "message")
            } else {
                showNotification("Upload failed: Missing country name in file", type = "error")
            }
        }, error = function(e) {
            showNotification(paste("Error loading file:", e$message), type = "error")
        })
    })
    
    ############################################################################
    # Dynamic Title Generation
    ############################################################################
    
    # Updating title according to requirements for multiple countries
    output$titleText <- renderText({
        paste("Data for:", paste(input$selectedCountry, collapse = ", "))
    })
    
    ############################################################################
    # Dynamic Column Selector Generation
    ############################################################################
    
    # Generate column selector
    output$colSelector <- renderUI({
        dat <- selectedData()
        if (is.null(dat) || nrow(dat) == 0) return(NULL)
        
        # Preserve current column selection if it exists
        currentCols <- input$cols
        defaultCols <- names(dat)
        
        # Use current selection if valid, otherwise use all columns
        selectedCols <- if(!is.null(currentCols) && all(currentCols %in% defaultCols)) {
            currentCols
        } else {
            defaultCols
        }
        
        checkboxGroupInput("cols", "Columns to show:",
                         choices = defaultCols,
                         selected = selectedCols)
    })
    
    ############################################################################
    # Dynamic Data Tables - UI Generation
    ############################################################################
    
    # Generate dynamic data tables - one for each country
    output$dynamicDataTables <- renderUI({
        req(input$selectedCountry)
        req(length(input$selectedCountry) > 0)
        
        countries <- input$selectedCountry
        
        country_blocks <- lapply(seq_along(countries), function(i) {
            country_name <- countries[i]
            
            div(
                style = "margin-bottom: 30px; padding: 20px; border-radius: 10px; border: 2px solid #DDDDDD; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                
                h3(country_name, 
                   style = "color: #2c3e50; margin-bottom: 15px; text-align: center; 
                           background-color: white; padding: 10px; border-radius: 5px; 
                           box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
                
                DTOutput(paste0("dataTable_", gsub("[^A-Za-z0-9]", "_", country_name)))
            )
        })
        
        do.call(tagList, country_blocks)
    })
    
    ############################################################################
    # Dynamic Data Tables - Server Side Rendering
    ############################################################################
    
    # Create individual data tables for each country
    observe({
        req(input$selectedCountry)
        
        # Create new data table for each selected country
        lapply(input$selectedCountry, function(country_name) {
            table_id <- paste0("dataTable_", gsub("[^A-Za-z0-9]", "_", country_name))
            
            # Create output for this country
            local({
                country <- country_name
                output[[table_id]] <- renderDT({
                    req(input$cols)
                    
                    # Get data for this specific country
                    country_data <- dataStore()[[country]]
                    if(is.null(country_data)) return(NULL)
                    
                    req(all(input$cols %in% names(country_data)))
                    
                    # Sort and filter
                    setorder(country_data, Year, `Indicator Name`)
                    filtered <- country_data[, .SD, .SDcols = input$cols]
                    
                    # Ensure numRows is valid
                    num_rows <- if(is.null(input$numRows) || is.na(input$numRows) || input$numRows < 1) 10 else input$numRows
                    
                    # Create the data table
                    datatable(
                        head(filtered, num_rows),
                        options = list(
                            pageLength = min(num_rows, 15),
                            scrollX = TRUE,
                            dom = 'rtip',
                            language = list(
                                info = paste("Showing _START_ to _END_ of _TOTAL_", country, "entries")
                            ),
                            columnDefs = list(
                                list(className = 'dt-center', targets = "_all")
                            )
                        ),
                        class = 'cell-border stripe'
                    )
                })
            })
        })
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
        countries <- unique(sub$`Country Name`)
        colors <- getColors(input$plot1ColorPalette, length(countries))
        
        # Create base plot with multiple countries
        if (input$plot1Agg == "none") {
            # Raw values
            p <- ggplot(sub, aes(x = Year, y = Value, color = `Country Name`)) +
                geom_line(linewidth = 1) +
                geom_point(size = 2) +
                scale_color_manual(values = setNames(colors, countries)) +
                labs(title = paste("Values for", input$plot1Indicator),
                     x = "Year", y = "Value", color = "Country")
            
        } else if (input$plot1Agg == "running_mean") {
            # Calculating the running mean for each country
            sub[, RunningMean := frollmean(Value, n = 5, align = "right"), by = `Country Name`]
            p <- ggplot(sub, aes(x = Year, y = RunningMean, color = `Country Name`)) +
                geom_line(linewidth = 1) +
                geom_point(size = 2) +
                scale_color_manual(values = setNames(colors, countries)) +
                labs(title = paste("5-Year Running Mean of", input$plot1Indicator),
                     x = "Year", y = "5-Year Running Mean", color = "Country")
            
        } else if (input$plot1Agg == "cumsum") {
            # Calculate cumulative sum for each country
            sub[, CumulativeSum := cumsum(Value), by = `Country Name`]
            p <- ggplot(sub, aes(x = Year, y = CumulativeSum, color = `Country Name`)) +
                geom_line(linewidth = 1) +
                geom_point(size = 2) +
                scale_color_manual(values = setNames(colors, countries)) +
                labs(title = paste("Cumulative Sum of", input$plot1Indicator),
                     x = "Year", y = "Cumulative Sum", color = "Country")
            
        } else if (input$plot1Agg == "yoy") {
            # Calculate year-over-year change for each country
            sub[, YoYChange := (Value - shift(Value)) / shift(Value) * 100, by = `Country Name`]
            p <- ggplot(sub, aes(x = Year, y = YoYChange, color = `Country Name`)) +
                geom_line(linewidth = 1) +
                geom_point(size = 2) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                scale_color_manual(values = setNames(colors, countries)) +
                labs(title = paste("Year-over-Year % Change in", input$plot1Indicator),
                     x = "Year", y = "Change (%)", color = "Country")
        }
        
        # Add common theme elements
        p + theme_minimal() +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                panel.grid.major = element_line(color = "#e9ecef"),
                panel.grid.minor = element_blank(),
                legend.position = "top"
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
        
        # Get top/bottom values across all countries
        plotData <- if(input$plot2Order == "top") {
            sub[order(-Value)][1:min(10, .N)]
        } else {
            sub[order(Value)][1:min(10, .N)]
        }
        
        # Create combined year-country labels for x-axis
        plotData[, YearCountry := paste(Year, "-", substr(`Country Name`, 1, 3))]
        
        # Create plot
        countries <- unique(plotData$`Country Name`)
        colors <- getColors(input$plot2ColorPalette, length(countries))
        
        ggplot(plotData, aes(x = reorder(YearCountry, Value), y = Value, fill = `Country Name`)) +
            geom_col(alpha = 0.7) +
            scale_fill_manual(values = setNames(colors, countries)) +
            geom_text(aes(label = round(Value, 2)), 
                     vjust = ifelse(plotData$Value >= 0, -0.5, 1.5),
                     color = "#2c3e50", size = 3) +
            labs(
                title = paste(if(input$plot2Order == "top") "Highest" else "Lowest",
                            "Values for", input$plot2Indicator),
                x = "Year - Country",
                y = "Value",
                fill = "Country"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major = element_line(color = "#e9ecef"),
                panel.grid.minor = element_blank(),
                legend.position = "top"
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
        
        # Calculate rolling mean for each country
        windowSize <- input$plot3Window
        sub[, RollingMean := frollmean(Value, n = windowSize, align = "right"), by = `Country Name`]
        
        # Get colors
        countries <- unique(sub$`Country Name`)
        colors <- getColors(input$plot3ColorPalette, length(countries))
        
        # Create plot
        ggplot(sub, aes(x = Year, color = `Country Name`)) +
            geom_line(aes(y = Value, linetype = "Actual"), linewidth = 0.8, alpha = 0.7) +
            geom_line(aes(y = RollingMean, linetype = "Rolling Average"), linewidth = 1.2) +
            scale_color_manual(values = setNames(colors, countries)) +
            scale_linetype_manual(
                name = "Series",
                values = c("Actual" = "solid", "Rolling Average" = "dashed")
            ) +
            labs(
                title = paste(windowSize, "Year Rolling Average for", input$plot3Indicator),
                x = "Year",
                y = "Value",
                color = "Country",
                linetype = "Series"
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