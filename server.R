#Author: Giuseppe Angele'
server <- function(input, output) {
    
    
    
    # # When Split change water change, update air
    # observeEvent(input$MassPercent,  {
    #     updateSliderInput(inputId = "PersPercent", value = 1 - input$MassPercent)
    # })
    # 
    # # when water change, update air
    # observeEvent(input$PersPercentSplit,  {
    #     updateSliderInput(inputId = "MassPercent", value = 1 - input$PersPercentSplit)
    # })
    
    
    ### Calculations
    
    DF <- reactive({
        #Debugging Bit Print Input
        print(paste0("Split  ", input$PersPercentSplit))
        print(paste0("Audience Size  ", input$AudienceSize))
        print(paste0("Spend  ", input$Spend))
        print(paste0("Reach Mass A ", input$ReachMassA))
        print(paste0("Reach Mass B ", input$ReachMassB))
        print(paste0("Reach Mass C ", input$ReachMassC))
        print(paste0("CPM Mass ", input$CPMMass))
        print(paste0("Reach Pers A ", input$ReachPersA))
        print(paste0("Reach Pers B ", input$ReachPersB))
        print(paste0("Reach Pers C ", input$ReachPersC))
        print(paste0("CPM Pers ", input$CPMPers))
        print(paste0("Prob Conv Mass ", input$ProbConvMass))
        print(paste0("Prob Conv Pers ", input$ProbConvPers))
        print(paste0("Prob Conv Both ", input$ProbConvBoth))
        print(paste0("Prob Conv None ", input$ProbConvNone))
        print(paste0("Price ", input$Price))
        
        
        ad_labels <- c("Mass", "Personalised", "Both", "None")
        
        #Reach
        cpp_mass <- input$CPMMass * input$AudienceSize * 10^-5
        reach_mass_B_spend <- input$ReachMassB/(1000*cpp_mass)^input$ReachMassC
        
        cpp_pers <-  input$CPMPers * input$AudienceSize * 10^-5
        reach_pers_B_spend <- input$ReachPersB/(1000*cpp_pers)^input$ReachPersC
        
        reach_mass <- input$ReachMassA/(1+reach_mass_B_spend*(input$Spend*(1-input$PersPercentSplit))^input$ReachMassC)
        reach_pers <- input$ReachPersA/(1+reach_pers_B_spend*(input$Spend*input$PersPercentSplit)^input$ReachPersC)
        reach_both <- 0.9*reach_mass*reach_pers
        
        # exc_reach_mass <- reach_mass - reach_both
        # exc_reach_pers <- reach_pers - reach_both
        # exc_reach_both <- reach_both
        # exc_reach_none <- 1 - exc_reach_mass - exc_reach_pers - exc_reach_both
        
        #Prob (Route)
        prob_reach_mass <- reach_mass - reach_both
        prob_reach_pers <- reach_pers - reach_both
        prob_reach_both <- reach_both
        prob_reach_none <- 1 - prob_reach_mass - prob_reach_pers - prob_reach_both
        
        prob_reach_cdf <- c(prob_reach_mass, prob_reach_mass+prob_reach_pers, 1-prob_reach_none, 1)
        prob_reach_cdf_df <- data.frame(ad_labels,prob_reach_cdf)
        
        
        #DataFrame Input
        DFInput <- data.frame(ad_labels = c("Mass", "Personal", "Both", "None"),
                              Split   = c(input$PersPercentSplit, 100-input$PersPercentSplit, 0, 0),
                              Reach_A = c(input$ReachMassA, input$ReachPersA, 0, 0),
                              Reach_B = c(input$ReachMassB, input$ReachPersB, 0, 0),
                              Reach_C = c(input$ReachMassC, input$ReachPersC, 0, 0),
                              Reach   = c(reach_pers, reach_mass, reach_both, 0),
                              Prob_Reach    = c(prob_reach_mass, prob_reach_pers, prob_reach_both, prob_reach_none),
                              Cum_Prob_Reach= prob_reach_cdf,
                              Prob_Conv     = c(input$ProbConvMass,input$ProbConvPers, input$ProbConvBoth, input$ProbConvNone),
                              Var_Prob_Conv = c(input$exc_var_mass, input$exc_var_pers, input$exc_var_both, input$exc_var_none),
                              freq_dist
        )
        
        
        #DataFrame Calculations
        DFCalc <- data.frame(Person = 1:500000,#500000
                             Rand_Prob_Reach   = runif(500000, 0, 1),
                             Rand_Prob_Conv    = runif(500000, 0, 1),
                             Rand_freq_cum_exc = runif(500000, 0, 1)
        )
        DFCalc$Route <- ifelse(DFCalc$Rand_Prob_Reach > prob_reach_cdf[3],"None",
                               ifelse(DFCalc$Rand_Prob_Reach > prob_reach_cdf[2], "Both",
                                      ifelse(DFCalc$Rand_Prob_Reach >prob_reach_cdf[1], "Personal", "Mass"
                                      )))
        
        
        DFCalc$ProbConvHat <- qnorm(DFCalc$Rand_Prob_Conv, 
                                    mean = ifelse(DFCalc$Route== "None", DFInput[DFInput$ad_labels =="None","Prob_Conv"],
                                                  ifelse(DFCalc$Route== "Both", DFInput[DFInput$ad_labels =="Both","Prob_Conv"],
                                                         ifelse(DFCalc$Route== "Personal", DFInput[DFInput$ad_labels =="Personal","Prob_Conv"],
                                                                ifelse(DFCalc$Route== "Mass", DFInput[DFInput$ad_labels =="Mass","Prob_Conv"], NA))))
                                    ,
                                    sd = ifelse(DFCalc$Route== "None", DFInput[DFInput$ad_labels =="None","Var_Prob_Conv"],
                                                ifelse(DFCalc$Route== "Both", DFInput[DFInput$ad_labels =="Both","Var_Prob_Conv"],
                                                       ifelse(DFCalc$Route== "Personal", DFInput[DFInput$ad_labels =="Personal","Var_Prob_Conv"],
                                                              ifelse(DFCalc$Route== "Mass", DFInput[DFInput$ad_labels =="Mass","Var_Prob_Conv"], NA))))
        )
        DFCalc$ProbConvHat <- ifelse(DFCalc$ProbConvHat < 0, 0, DFCalc$ProbConvHat)
        
        DFCalc$Freq <- ifelse(DFCalc$Rand_freq_cum_exc > prob_reach_cdf[3], 4,
                              ifelse(DFCalc$Rand_freq_cum_exc > prob_reach_cdf[2], 3,
                                     ifelse(DFCalc$Rand_freq_cum_exc >prob_reach_cdf[1], 2, 1
                                     )))
        DFCalc$Price <- input$Price
        DFCalc$Revenue <- DFCalc$ProbConvHat * DFCalc$Freq * DFCalc$Price
        print(DFCalc[1:10,])
        DFCalc <- rbind(DFCalc, DFCalc, DFCalc, DFCalc, DFCalc, DFCalc)
        list(DFCalc, DFInput)
    }
    )
    
    DFSim <-  eventReactive(input$runsim, {
        ad_labels <- c("Mass", "Personalised", "Both", "None")
        PersPercentSplit <- c(0:100)

        Result <- lapply(PersPercentSplit, function(x) {
        #Reach
        cpp_mass <- input$CPMMass * input$AudienceSize * 10^-5
        reach_mass_B_spend <- input$ReachMassB/(1000*cpp_mass)^input$ReachMassC

        cpp_pers <-  input$CPMPers * input$AudienceSize * 10^-5
        reach_pers_B_spend <- input$ReachPersB/(1000*cpp_pers)^input$ReachPersC

        reach_mass <- input$ReachMassA/(1+reach_mass_B_spend*(input$Spend*(100-x))^input$ReachMassC)
        reach_pers <- input$ReachPersA/(1+reach_pers_B_spend*(input$Spend*x)^input$ReachPersC)
        reach_both <- 0.9*reach_mass*reach_pers

        # exc_reach_mass <- reach_mass - reach_both
        # exc_reach_pers <- reach_pers - reach_both
        # exc_reach_both <- reach_both
        # exc_reach_none <- 1 - exc_reach_mass - exc_reach_pers - exc_reach_both

        #Prob (Route)
        prob_reach_mass <- reach_mass - reach_both
        prob_reach_pers <- reach_pers - reach_both
        prob_reach_both <- reach_both
        prob_reach_none <- 1 - prob_reach_mass - prob_reach_pers - prob_reach_both

        prob_reach_cdf <- c(prob_reach_mass, prob_reach_mass+prob_reach_pers, 1-prob_reach_none, 1)
        prob_reach_cdf_df <- data.frame(ad_labels,prob_reach_cdf)


        #DataFrame Input
        DFInput <- data.frame(ad_labels = c("Mass", "Personal", "Both", "None"),
                              Split   = c(x, 100-x, 0, 0),
                              Reach_A = c(input$ReachMassA, input$ReachPersA, 0, 0),
                              Reach_B = c(input$ReachMassB, input$ReachPersB, 0, 0),
                              Reach_C = c(input$ReachMassC, input$ReachPersC, 0, 0),
                              Reach   = c(reach_pers, reach_mass, reach_both, 0),
                              Prob_Reach    = c(prob_reach_mass, prob_reach_pers, prob_reach_both, prob_reach_none),
                              Cum_Prob_Reach= prob_reach_cdf,
                              Prob_Conv     = c(input$ProbConvMass,input$ProbConvPers, input$ProbConvBoth, input$ProbConvNone),
                              Var_Prob_Conv = c(input$exc_var_mass, input$exc_var_pers, input$exc_var_both, input$exc_var_none),
                              freq_dist
        )


        #DataFrame Calculations
        DFCalc <- data.frame(Person = 1:500000,#500000
                             Rand_Prob_Reach   = runif(500000, 0, 1),
                             Rand_Prob_Conv    = runif(500000, 0, 1),
                             Rand_freq_cum_exc = runif(500000, 0, 1)
        )
        DFCalc$Route <- ifelse(DFCalc$Rand_Prob_Reach > prob_reach_cdf[3],"None",
                               ifelse(DFCalc$Rand_Prob_Reach > prob_reach_cdf[2], "Both",
                                      ifelse(DFCalc$Rand_Prob_Reach >prob_reach_cdf[1], "Personal", "Mass"
                                      )))


        DFCalc$ProbConvHat <- qnorm(DFCalc$Rand_Prob_Conv,
                                    mean = ifelse(DFCalc$Route== "None", DFInput[DFInput$ad_labels =="None","Prob_Conv"],
                                                  ifelse(DFCalc$Route== "Both", DFInput[DFInput$ad_labels =="Both","Prob_Conv"],
                                                         ifelse(DFCalc$Route== "Personal", DFInput[DFInput$ad_labels =="Personal","Prob_Conv"],
                                                                ifelse(DFCalc$Route== "Mass", DFInput[DFInput$ad_labels =="Mass","Prob_Conv"], NA))))
                                    ,
                                    sd = ifelse(DFCalc$Route== "None", DFInput[DFInput$ad_labels =="None","Var_Prob_Conv"],
                                                ifelse(DFCalc$Route== "Both", DFInput[DFInput$ad_labels =="Both","Var_Prob_Conv"],
                                                       ifelse(DFCalc$Route== "Personal", DFInput[DFInput$ad_labels =="Personal","Var_Prob_Conv"],
                                                              ifelse(DFCalc$Route== "Mass", DFInput[DFInput$ad_labels =="Mass","Var_Prob_Conv"], NA))))
        )
        DFCalc$ProbConvHat <- ifelse(DFCalc$ProbConvHat < 0, 0, DFCalc$ProbConvHat)

        DFCalc$Freq <- ifelse(DFCalc$Rand_freq_cum_exc > prob_reach_cdf[3], 4,
                              ifelse(DFCalc$Rand_freq_cum_exc > prob_reach_cdf[2], 3,
                                     ifelse(DFCalc$Rand_freq_cum_exc >prob_reach_cdf[1], 2, 1
                                     )))
        DFCalc$Price <- input$Price
        DFCalc$Revenue <- DFCalc$ProbConvHat * DFCalc$Freq * DFCalc$Price
        print(DFCalc[1:10,])
        DFCalc <- rbind(DFCalc, DFCalc, DFCalc, DFCalc, DFCalc, DFCalc)
        ResultTemp <- data.frame(SplitPers = x,
                             SplitMass = 100-x,
                             Total_Revenue =sum(DFCalc$Revenue))
        })
        Result2 <- rbindlist(Result)
        }
    )
    

    DFInput <- reactive({
        DF <- DF(); DF <- DF[[2]]
        DF$Price <- input$Price
        DF <- DF[, c("ad_labels", "Split", "Prob_Reach", "Prob_Conv", "freq_num",  "freq_cum_exc", "Price")]
        names(DF) <- c("Exposure Type",  "Split" , "Probabilty Reached", "Probabilty of Conversion", 
                       "Frequency", "Probability of Frequency", "Price") 
        DF
    })
    
    Tot_Rev <- reactive({
        DF <- DF(); DF <- DF[[1]]
        DFGroup <- DF %>% group_by(Route) %>% summarise(Revenue = sum(Revenue))
        Total_Revenue <- data.frame(Route = "Total", Revenue = sum(DF$Revenue))
        Total_Revenue <- rbind(Total_Revenue, as.data.frame(DFGroup))
        #Total_Revenue <- format(round(Total_Revenue, 0), nsmall = 0)
        Total_Revenue <- Total_Revenue[order(Total_Revenue$Revenue,  decreasing = T),]
        Total_Revenue <- cbind(Total_Revenue, Currency = "EUR")
    })
    
    
    #Table For Frequency of View
    output$edit_freq_dist <- renderRHandsontable({
        rhandsontable(freq_dist, rowHeaders=F, readOnly=T)
    })
    
    #Table Output Input 1 Panel 1
    output$Input1 <- renderRHandsontable({
        DFInput1 <- DFInput()
        DFInput1 <- DFInput1[, c("Exposure Type", "Split", "Probabilty Reached")]
        rhandsontable(DFInput1, rowHeaders=F, readOnly=T) %>%
            hot_cols(colWidths = 100)
    })
    
    #Table Output Input 2 Panel 1
    output$Input2 <- renderRHandsontable({
        DFInput2 <- DFInput()
        DFInput2 <- as.data.frame(DFInput2[, "Probabilty of Conversion"])
        names(DFInput2) <- "Probabilty of Conversion"
        rhandsontable(DFInput2, rowHeaders=F, readOnly=T) %>%
            hot_cols(colWidths = 100)
    })
    
    #Table Output Input 2 Panel 1
    output$Input3 <- renderRHandsontable({
        DFInput3 <- DFInput()
        DFInput3 <- DFInput3[, c("Frequency", "Probability of Frequency", "Price")]
        rhandsontable(DFInput3, rowHeaders=F, readOnly=T) %>%
            hot_cols(colWidths = 100)
    })
    
    #Table Output Result Panel 1
    output$Results <- renderRHandsontable({
        DF <- DF(); DF <- DF[[1]]
        DF <- DF[1:100,]
        DF <- DF[,c("Person", "Route" , "Rand_Prob_Conv", "Freq", "Price", "Revenue")]
        names(DF) <- c("Person",  "Exposure Type" , "Probabilty of Conversion", "Frequency", "Price", "Expected Individual Revenue")
        rhandsontable(DF, rowHeaders=F, readOnly=T) %>%
            hot_cols(colWidths = 100)
    })
    
    #Table for Total
    output$TotalRevenue <- renderRHandsontable({
        Tot_Rev <- Tot_Rev()
        names(Tot_Rev) <- strwrap(names(Tot_Rev)) 
        rhandsontable(Tot_Rev, rowHeaders=F, readOnly=T) %>%
            hot_col("Revenue", format = "0,0")
    })
    
    #Table for Simualation
    
    output$ResultsSimulation <- renderRHandsontable({
        DFSimul <- DFSim()
        names(DFSimul) <- c("Pers %Spend", "Mass %Spend", "Revenue")
        DFSimul$Currency <- "EUR"
        #DFSimul <- DFSimul[1:10,]
        rhandsontable(DFSimul, rowHeaders=F, readOnly=T) %>%
            hot_col("Pers %Spend", format = "0") %>%
            hot_col("Mass %Spend", format = "0") %>%
            hot_col("Revenue", format = "0,0")  %>%
            hot_cols(colWidths = 100)
    })
    
    
    
    
    # #Table for Total Simulation
    # output$TotalRevenue <- renderRHandsontable({
    #     DFTotal <- DFTotal()
    #     rhandsontable(DFTotal, rowHeaders=F, readOnly=T) %>%
    #         hot_col("Revenue", format = "0,0")
    # })
    
    #Bar Chart
    output$BarChart <- renderPlotly({
        StackBar <- plot_ly(Tot_Rev(), x = ~Route, y = ~Revenue, type ='bar', marker = list(color ='#1d4983'), name ='Direct') %>%
            layout(yaxis = list(title = 'Revenue'))
    })
    
}