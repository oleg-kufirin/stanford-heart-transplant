library(shiny)

# Using a fluid Bootstrap layout
fluidPage(
    tags$style(type='text/css', 
               ".help-block { font-size: 12px; line-height: 14px;}"),
    
    titlePanel("Survival analysis for the Stanford Heart Transplant Data"),
    
    sidebarLayout(

        sidebarPanel(
            img(src='Stanford.png', align = "left", 
                width = "220", height = "50"),
            br(),
            br(),
            br(),
            h4("Kaplan-Meier Survival"),
            selectInput("factor", "Stratification factor:", 
                        c("None" = "1", "Age Group" = "age.group", 
                          "Transplant" = "transplant", "Surgery" = "surgery",
                          "Year Group" = "year.group")),
            hr(),
            h4("Cox Proportional-Hazards"),
            checkboxGroupInput("inCheckboxGroup",
                               "Regressors:",
                               c("Age Group" = "age.group", 
                                 "Transplant" = "transplant", 
                                 "Surgery" = "surgery",
                                 "Year Group" = "year.group")),
            h4("Prediction"),
            selectInput("radioAgeGroup", "Age Group", 
                        c("before 40" = "[ 8.79,40.00)", 
                          "40 to 50" = "[40.00,50.00)",
                          "over 50" = "[50.00,64.41]")),
            radioButtons("radioTransplant", "Transplant",
                         choices = c("No" = "0", "Yes" = "1"), inline = TRUE),
            radioButtons("radioSurgery", "Surgery",
                         choices = c("No" = "0", "Yes" = "1"), inline = TRUE),
            selectInput("radioYearGroup", "Year Group", 
                        c("less than 2" = "[-0.0493, 2.0000)", 
                          "2 to 5" = "[ 2.0000, 5.0000)", 
                          "over 5" = "[ 5.0000, 6.4723]")),
            hr(),
            h4("Data Dictionary"),
            helpText("Time - time to death event, days"),
            helpText("Transplant - transplant indicator [0=No; 1=Yes]"),
            helpText("Surgery - prior bypass surgery [0=No; 1=Yes]"),
            helpText("Year Group - year of acceptance in the program;
                     in years after 1 Nov 1967 [<2; 2-5; >5]"),
            helpText("Age Group - [<40; 40-50; >50]")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Kaplan-Meier Survival ", br(), 
                                    plotOutput("survivalCurve"),
                                    plotOutput("cumHazardCurve")),
                        tabPanel("Cox Proportional-Hazards", br(),
                                    plotOutput("coxPHMCurve"),
                                    br(),
                                    div(style="width:600px;",
                                        fluidRow(textOutput("noteOutput"))),
                                    br(),
                                    h4("Model Summary"),
                                    div(style="width:600px;",
                                        fluidRow(verbatimTextOutput
                                                ("modelOutput",
                                                    placeholder = TRUE))),
                                    br(),
                                    plotOutput("predCoxPHMCurve")
                                 ),
                        tabPanel("Dataset", br(),
                                 DT::dataTableOutput("dataTable")
                        ),
                        tabPanel("Info", br(),
                                 tags$h4("Dataset"),
                                 textOutput("datasetOutput"),
                                 tags$h4("Kaplan-Meier Survival"),
                                 textOutput("kmOutput"),
                                 # br(),
                                 tags$a(href="http://www.sthda.com/english/wiki/survival-analysis-basics", 
                                        "Reference material"),
                                 tags$h4("Cox Proportional-Hazards"),
                                 textOutput("cphOutput"),
                                 # br(),
                                 tags$a(href="http://www.sthda.com/english/wiki/cox-proportional-hazards-model", 
                                        "Reference material"),
                                 br(), br(),
                                 tags$h5("Made by Oleg Kufirin, 2021"),
                                 tags$a(href="https://github.com/oleg-kufirin", 
                                        "Github"), br(),
                                 tags$a(href="https://www.linkedin.com/in/oleg-kufirin/", 
                                        "LinkedIn")
                        )
            )
        )
    )
)

