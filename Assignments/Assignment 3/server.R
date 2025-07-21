server <- function(input, output, session) {
    dataStore <- reactiveVal(builtInData)
    
    selectedData <- reactive({
        req(input$selectedCountry)
        dataStore()[[input$selectedCountry]]
    })
    
    observe({
        inds <- unique(selectedData()$`Indicator Name`)
        updateSelectizeInput(session, "plot1Indicator", choices = inds, server = TRUE)
        updateSelectizeInput(session, "plot2Indicator", choices = inds, server = TRUE)
        updateSelectizeInput(session, "plot3Indicator", choices = inds, server = TRUE)
    })
    
    observeEvent(input$uploadFile, {
        file <- input$uploadFile
        if (is.null(file)) return()
        
        tryCatch({
            newData <- readIndicators(file$datapath)
            countryName <- unique(newData$`Country Name`)[1]
            if (!is.na(countryName) && nzchar(countryName)) {
                updated <- dataStore()
                updated[[countryName]] <- newData
                dataStore(updated)
                updateSelectInput(session, "selectedCountry", 
                                choices = names(updated))
                showNotification(paste("Loaded:", countryName), 
                               type = "message")
            } else {
                showNotification("Upload failed: Missing country name in file", type = "error")
            }
        }, error = function(e) {
            showNotification(paste("Error loading file:", e$message), type = "error")
        })
    })
    
    output$titleText <- renderText({
        paste("Data for:", input$selectedCountry)
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
        req(input$cols)
        req(all(input$cols %in% names(dat)))
        
        # Ensure we get a data.frame by using .SD
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
    
    output$plot1 <- renderPlot({
        req(input$plot1Indicator)
        dat <- selectedData()
        sub <- dat[`Indicator Name` == input$plot1Indicator &
                   Year >= input$plot1Year[1] & Year <= input$plot1Year[2]]
        
        if (nrow(sub) == 0) {
            return(ggplot() + 
                   annotate("text", x = 1, y = 1, label = "No data available for selected range") +
                   theme_void())
        }
        
        agg_fun <- match.fun(input$plot1Agg)
        agg <- sub[, .(Value = agg_fun(Value, na.rm = TRUE)), by = Year]
        
        ggplot(agg, aes(x = Year, y = Value)) +
            geom_line(linewidth = 1, color = "#2c3e50") +
            geom_point(size = 3, color = "#2c3e50") +
            labs(title = paste(toupper(input$plot1Agg), "of", input$plot1Indicator),
                 x = "Year", y = "Value") +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                panel.grid.major = element_line(color = "#e9ecef"),
                panel.grid.minor = element_blank()
            )
    })
    
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
        
        if (input$plot2Order == "top") {
            plot_data <- sub[order(-Value)][1:min(10, .N)]
        } else {
            plot_data <- sub[order(Value)][1:min(10, .N)]
        }
        
        ggplot(plot_data, aes(x = reorder(paste(Year), Value), y = Value)) +
            geom_col(fill = "#3498db", color = "#2980b9", alpha = 0.7) +
            geom_text(aes(label = round(Value, 2)), 
                     vjust = ifelse(plot_data$Value >= 0, -0.5, 1.5),
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
        
        sub <- sub[order(Year)]
        window_size <- input$plot3Window
        
        sub[, RollingMean := frollmean(Value, n = window_size, align = "right")]
        
        rolling_label <- paste(window_size, "Year Rolling Average")
        
        ggplot(sub, aes(x = Year)) +
            geom_line(aes(y = Value, color = "Actual Value"), alpha = 0.7, linewidth = 1) +
            geom_point(aes(y = Value, color = "Actual Value"), size = 2, alpha = 0.7) +
            geom_line(aes(y = RollingMean, color = rolling_label),
                     linewidth = 1.5) +
            scale_color_manual(
                name = "Series",
                values = c(
                    "Actual Value" = "#95a5a6",
                    rolling_label = "#2980b9"
                )
            ) +
            labs(
                title = paste("Time Series with Rolling Average for", input$plot3Indicator),
                x = "Year",
                y = "Value"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                legend.position = "top",
                legend.box = "horizontal",
                panel.grid.major = element_line(color = "#e9ecef"),
                panel.grid.minor = element_blank()
            ) +
            guides(color = guide_legend(nrow = 1))
    })
} 