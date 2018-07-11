#Author: Giuseppe Angele'

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    #TITLE ROW
        fluidRow(
            #left col spacer
            #column(width=1),
        # App title ----
    column(width=4, img(src="Logos.png", height = 55)),
    column(width=3, titlePanel("MVP Pepsico Simulator"))
        ),
    
    br(),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        sidebarPanel(width = 2,
                    tags$em(h3("Setting Panel")),
                    br(), 
                    #Section 1
                    h4("Spend and Audience"),
                    sliderInput("PersPercentSplit",
                                label = div(style='width:250px;', 
                                            div(style='float:left;', 'Mass'), 
                                            div(style='float:right;', 'Personalised')), 
                                min = 0, max = 100, post  = "%", value = 40, step = 1, ticks = F),
                    sliderInput("Spend",label="Total Spend", min = 0, max = 10000000,
                                value = 1000000, step = 500000),
                    sliderInput("AudienceSize",label="Audience size", min = 0, max = 20000000,
                                value = 3000000, step = 500000),
                    br(), br(),
                    #Section 2
                    h4("Exposure"),
                    tags$b("Mass"),
                    numericInput("ReachMassA", "Reach Mass A:", value = 0.8, step = 0.1),
                    numericInput("ReachMassB", "Reach Mass B:", value = 0.002, step = 0.001),
                    numericInput("ReachMassC", "Reach Mass C:", value = -1, step = 0.1),
                    numericInput("CPMMass", "CPM Mass:", value = 20, step = 1),
                    br(),
                    tags$b("Personalised"),
                    numericInput("ReachPersA", "Reach Pers A:", value = 0.2, step = 0.1),
                    numericInput("ReachPersB", "Reach Pers B:", value = 0.002, step = 0.001),
                    numericInput("ReachPersC", "Reach Pers C:", value = -1, step = 0.1),
                    numericInput("CPMPers", "CPM Pers:", value = 200, step = 1),
                    br(), br(),
                    #Section 3
                    h4("Convert"),
                    tags$b("Probability  to Convert"),
                    numericInput("ProbConvMass", "Prob Conv Mass", value = 0.5, step = 0.1),
                    numericInput("ProbConvPers", "Prob Conv Pers", value = 0.8, step = 0.1),
                    numericInput("ProbConvBoth", "Prob Conv Both", value = 0.85, step = 0.1),
                    numericInput("ProbConvNone", "Prob Conv None", value = 0, step = 0.1),
                    br(), br(),
                    #Section 4
                    h4("Value"),
                    numericInput("Price", "Price:", value = 0.5, step = 0.1),
                    tags$b("Purchase Frequency Proabability:"),
                    rHandsontableOutput("edit_freq_dist"),
                    br(), br(), br(),
                    #Section 5
                    h4("Adjustments"),
                    numericInput("exc_var_mass", "Excl Var Mass", value = 0.1, step = 0.1, width = "200px"),
                    numericInput("exc_var_pers", "Excl Var Pers", value = 0.1, step = 0.1, width = "200px"),
                    numericInput("exc_var_both", "Excl Var Both", value = 0.1, step = 0.1, width = "200px"),
                    numericInput("exc_var_none", "Excl Var None", value = 0.1, step = 0.1, width = "200px"),
                    numericInput("calibration", "Calibration", value = 1, step = 1, width = "200px")
         
        ),
    
        mainPanel(
            tabsetPanel(type = "tabs"
                        ,
                        tabPanel("Single Run", 
                                 br(),
                                 column(4,
                                        rHandsontableOutput("Input1")),
                                 column(2,
                                        rHandsontableOutput("Input2")),
                                 column(3,
                                        rHandsontableOutput("Input3")),
                                 br(), br(), br(), br(), br(), br(), 
                                 br(), br(), br(), br(), br(), br(), 
                                 column(7,
                                        rHandsontableOutput("Results")),
                                 column(3,
                                        rHandsontableOutput("TotalRevenue"),
                                        plotlyOutput("BarChart", height ="400px", width ="500px")),
                                 tags$head(
                                     tags$style(HTML("
      .handsontable {
        overflow: visible;
      }
    ")))
                        ),
                        tabPanel("Simulation", 
                                 br(), br(),
                                 h4("In order to run the simulation of all the scenario please click on the button below.",
                                    br(),br(),
                                    "Atfer clicking it may take a few minutes. Please wait!"),
                                 br(),
                                 actionButton("runsim", "Run Simulation"),
                                 br(), br(),
                                 column(8,
                                        rHandsontableOutput("ResultsSimulation"))
                        )
                                
                        # ,
                        # tabPanel()                         
            )
        )
    )
)
