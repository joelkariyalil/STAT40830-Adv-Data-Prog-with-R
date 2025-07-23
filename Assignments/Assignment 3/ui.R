################################################################################
# UI Definition - Country Health Analysis Dashboard
################################################################################

ui <- fluidPage(
    
    ############################################################################
    # CSS Styling and Theme
    ############################################################################
    
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
    
    ############################################################################
    # Dashboard Header
    ############################################################################
    
    div(
        style = "background-color: #0000FF; color: white; padding: 15px; margin-bottom: 20px; border-radius: 10px;",
        titlePanel("Country Health Analysis Dashboard")
    ),
    
    ############################################################################
    # Main Layout Structure
    ############################################################################
    
    sidebarLayout(
        
        ########################################################################
        # Sidebar Panel - Controls and Settings
        ########################################################################
        
        sidebarPanel(
            class = "sidebar",
            
            ### Country Selection Controls
            selectInput("selectedCountry", "Select Countries:",
                        choices = names(builtInData),
                        selected = names(builtInData)[1:2],
                        multiple = TRUE),
            
            ### File Upload Section
            div(
                style = "background-color: #e9ecef; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                fileInput("uploadFile", "Upload new indicators_COUNTRYCODE.csv", accept = ".csv")
            ),
            
            ### Data Table Controls
            conditionalPanel(
                condition = "input.mainTabs == 'Data'",
                numericInput("numRows", "Rows to display:", 10, min = 1),
                uiOutput("colSelector")
            ),
            
            ############################################################
            # Plot Controls and Settings
            ############################################################
            
            conditionalPanel(
                condition = "input.mainTabs == 'Plots'",
                
                ### Time Series Settings
                conditionalPanel(
                    condition = "input.plotTab == 'Time Series'",
                    h4("Time Series Settings:", style = "color: #2c3e50;"),
                    selectizeInput("plot1Indicator", "Indicator", choices = NULL),
                    selectInput("plot1Agg", "View Type", 
                              choices = c(
                                "Raw Values" = "none",
                                "Running Mean (5-year)" = "running_mean",
                                "Cumulative Sum" = "cumsum",
                                "Year-over-Year Change %" = "yoy"
                              )),
                    sliderInput("plot1Year", "Year Range", min = 1960, max = 2024, 
                              value = c(2010, 2020), step = 1),
                    selectInput("plot1ColorPalette", "Color Palette",
                              choices = c("Default" = "default",
                                        "Set3" = "set3",
                                        "Blues" = "blues"))
                ),
                
                ### Top/Bottom Values Settings
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
                
                ### Rolling Average Settings
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
        
        ########################################################################
        # Main Panel - Display Area
        ########################################################################
        
        mainPanel(
            h4(textOutput("titleText"), style = "color: #2c3e50;"),
            
            ########################################################
            # Tab Panel Container - Main Content Area
            ########################################################
            
            div(
                class = "plot-container",
                tabsetPanel(id = "mainTabs",
                    
                    ### Data View Tab
                    tabPanel("Data", 
                        uiOutput("dynamicDataTables")
                    ),
                    
                    ### Plot View Tab
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