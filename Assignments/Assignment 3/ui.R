ui <- fluidPage(
    # Custom CSS
    tags$head(
        tags$style(HTML("
            .sidebar { background-color: #f8f9fa; padding: 15px; border-radius: 10px; }
            .nav-tabs > li > a { color: #495057; }
            .nav-tabs > li.active > a { background-color: #e9ecef !important; }
            h4 { color: #2c3e50; margin-top: 20px; }
            .form-group { margin-bottom: 20px; }
            .well { background-color: #ffffff; border: none; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
            .selectize-input { border-radius: 5px; }
            hr { border-top: 2px solid #e9ecef; }
            .plot-container { background-color: #ffffff; padding: 15px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
        "))
    ),
    
    # Title with custom style
    div(
        style = "background-color: #2c3e50; color: white; padding: 15px; margin-bottom: 20px; border-radius: 10px;",
        titlePanel("Country Health Analysis Dashboard")
    ),
    
    sidebarLayout(
        sidebarPanel(
            class = "sidebar",
            selectInput("selectedCountry", "Select Country:",
                        choices = names(builtInData),
                        selected = names(builtInData)[1],
                        multiple = FALSE),
            
            div(
                style = "background-color: #e9ecef; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                fileInput("uploadFile", "Upload new indicators_COUNTRYCODE.csv", accept = ".csv")
            ),
            
            # Data table controls
            conditionalPanel(
                condition = "input.mainTabs == 'Data'",
                numericInput("numRows", "Rows to display:", 10, min = 1),
                uiOutput("colSelector")
            ),
            
            # Plot controls
            conditionalPanel(
                condition = "input.mainTabs == 'Plots'",
                # Plot 1 Settings - only show when Time Series tab is active
                conditionalPanel(
                    condition = "input.plotTab == 'Time Series'",
                    h4("Time Series Settings:", style = "color: #2c3e50;"),
                    selectizeInput("plot1Indicator", "Indicator", choices = NULL),
                    selectInput("plot1Agg", "Aggregation Function", 
                              choices = c("Mean" = "mean", "Median" = "median", "Sum" = "sum")),
                    sliderInput("plot1Year", "Year Range", min = 1960, max = 2024, 
                              value = c(2010, 2020), step = 1),
                    selectInput("plot1ColorPalette", "Color Palette",
                              choices = c("Default" = "default",
                                        "Set3" = "set3",
                                        "Blues" = "blues"))
                ),
                
                # Plot 2 Settings - only show when Top/Bottom Values tab is active
                conditionalPanel(
                    condition = "input.plotTab == 'Top/Bottom Values'",
                    h4("Top/Bottom Values Settings:", style = "color: #2c3e50;"),
                    selectizeInput("plot2Indicator", "Indicator", choices = NULL),
                    selectInput("plot2Order", "Show",
                              choices = c(
                                  "Top 10 Highest Values" = "top",
                                  "Top 10 Lowest Values" = "bottom"
                              )),
                    sliderInput("plot2Year", "Year Range", min = 1960, max = 2024, 
                              value = c(2010, 2020), step = 1),
                    selectInput("plot2ColorPalette", "Color Palette",
                              choices = c("Default" = "default",
                                        "Set3" = "set3",
                                        "Blues" = "blues"))
                ),
                
                # Plot 3 Settings - only show when Rolling Average tab is active
                conditionalPanel(
                    condition = "input.plotTab == 'Rolling Average'",
                    h4("Rolling Average Settings:", style = "color: #2c3e50;"),
                    selectizeInput("plot3Indicator", "Select Indicator", choices = NULL),
                    numericInput("plot3Window", "Rolling Average Window Size (Years):", 5, min = 2, max = 10),
                    sliderInput("plot3Year", "Year Range", min = 1960, max = 2024, 
                              value = c(2010, 2020), step = 1),
                    selectInput("plot3ColorPalette", "Color Palette",
                              choices = c("Default" = "default",
                                        "Set3" = "set3",
                                        "Blues" = "blues"))
                )
            )
        ),
        
        mainPanel(
            h4(textOutput("titleText"), style = "color: #2c3e50;"),
            div(
                class = "plot-container",
                tabsetPanel(id = "mainTabs",
                    tabPanel("Data", 
                        DTOutput("dataTable")
                    ),
                    tabPanel("Plots",
                        tabsetPanel(
                            id = "plotTab",
                            tabPanel("Time Series", 
                                div(
                                    style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                                    plotOutput("plot1", height = "500px")
                                )
                            ),
                            tabPanel("Top/Bottom Values", 
                                div(
                                    style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                                    plotOutput("plot2", height = "500px")
                                )
                            ),
                            tabPanel("Rolling Average", 
                                div(
                                    style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                                    plotOutput("plot3", height = "500px")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
) 