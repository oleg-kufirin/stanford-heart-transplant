library(shiny)
library(survival)
library(survminer)
library(p3state.msm)
library(Hmisc)

# Covariate name constants
AG <- "age.group"
TR <- "transplant"
YG <- "year.group"
SR <- "surgery"

# Loading the dataset
data("heart2")
# Original age column was centred at 48, go back to the original scale
heart2$age <- heart2$age + 48
# Add new column for age groups
heart2$age.group <- cut2(heart2$age, c(40,50))
# Add new column for age year of inclusion into WL groups
heart2$year.group <- cut2(heart2$year, c(2,5))

# Formatting the column name and data types
colnames(heart2)[2] <- "transplant"
heart2$transplant <- as.factor(heart2$transplant)
heart2$surgery <- as.factor(heart2$surgery)



function(input, output, session) {
    
    # Fit KM and output survival and hazard plots
    # factor: covariate of interest
    KaplanMeier <- function(factor) {
        expr = paste("survfit(Surv(time, status) ~ ", factor, ", data = heart2)")
        fit <- eval(parse(text = expr))
        
        # To avoid bug in the plotting library
        if (factor == "1")
            censoring_plot <- FALSE
        else
            censoring_plot <- TRUE
        
        # Survival curve
        surv_curve <- ggsurvplot(
            fit,
            data = heart2,
            title = "Kaplan-Meier Survival Curve",
            font.title = c(16, "bold", "darkblue"),
            pval = TRUE,
            conf.int = TRUE,
            xlab = "Time in days",
            break.time.by = 200,
            ggtheme = theme_light(),
            risk.table = "abs_pct",
            risk.table.y.text.col = T,
            risk.table.y.text = FALSE,
            ncensor.plot = censoring_plot,
            surv.median.line = "hv",
            xlim = c(0, 1800),
            fontsize = 3
        ) 

        # Cumulative hazard curve
        cumul_haz <- ggsurvplot(
            fit,
            data = heart2,
            title = "Cumulative Hazard Curve",
            font.title = c(16, "bold", "darkblue"),
            conf.int = TRUE,
            risk.table.col = "strata",
            ggtheme = theme_bw(),
            fun = "cumhaz",
            break.time.by = 200,
            xlim = c(0, 1800)
        )
        
        return(list(survival = surv_curve, hazard = cumul_haz))
    }
    
    # Evaluate Cox PH model
    coxModel <- reactive({
        if (is.null(input$inCheckboxGroup))
            factor = '1'
        else
            factor = paste(unlist(input$inCheckboxGroup), collapse=' + ')
        
        expr = paste("coxph(Surv(time, status) ~ ", factor, ", data = heart2)")
        eval(parse(text = expr))
    })
    
    # Plot for KM survival curve
    output$survivalCurve <- renderPlot({
        KaplanMeier(input$factor)$survival
    }, height = 400, width = 500)

    # Plot for KM cumulative hazard curve
    output$cumHazardCurve <- renderPlot({
        KaplanMeier(input$factor)$hazard
    }, height = 400, width = 500)
    
    # Plot for Cox PH model
    output$coxPHMCurve <- renderPlot({
        
        in_boxes <- input$inCheckboxGroup
        legend_labs <- NULL
        
        if (!is.null(in_boxes) & length(in_boxes) == 1) {
            
            if (prod(in_boxes == list(AG))){
                unique_factor <- 
                    unique(heart2$age.group)[order(unique(heart2$age.group))]
                strat <- with(heart2, data.frame(age.group = unique_factor))
                legend_labs <- c("Age<40", "Age 40-50", "Age>50")
            } else if (prod(in_boxes == list(TR))){
                unique_factor <- 
                    unique(heart2$transplant)[order(unique(heart2$transplant))]
                strat <- with(heart2, data.frame(transplant = unique_factor))
                legend_labs <- c("Transplant=No", "Transplant=Yes")
            } else if (prod(in_boxes == list(YG))){
                unique_factor <- 
                    unique(heart2$year.group)[order(unique(heart2$year.group))]
                strat <- with(heart2, data.frame(year.group = unique_factor))
                legend_labs <- c("Year<2", "Year 2-5", "Year>5")
            } else if (prod(in_boxes == list(SR))){
                unique_factor <- 
                    unique(heart2$surgery)[order(unique(heart2$surgery))]
                strat <- with(heart2, data.frame(surgery =  unique_factor))
                legend_labs <- c("Surgery=No", "Surgery=Yes")
            }
            fit_survival_fun <- survfit(coxModel(), newdata = strat)
        } else
            fit_survival_fun <- survfit(coxModel())
            
        ggsurvplot(
            fit_survival_fun,
            data = heart2,
            title = "Cox Proportional Hazard Model",
            font.title = c(16, "bold", "darkblue"),
            conf.int = TRUE,
            risk.table.col = "strata",
            ggtheme = theme_bw(),
            break.time.by = 200,
            legend.labs = legend_labs,
            xlim = c(0, 1800)
        )
    }, height = 400, width = 500)
    
    # Plot text note
    output$noteOutput <- renderText({
        "For univariate models the curve is split based on the
        values of a covariate of interest. For multivariate models the
        curve is estimated based on the mean values of covariates."
    })
    
    # Plot model summary
    output$modelOutput <- renderPrint({
        summary(coxModel())
    })
    
    # Plot predicted survival curve
    output$predCoxPHMCurve <- renderPlot({

        in_boxes <- input$inCheckboxGroup

        if (is.null(in_boxes))
            return(NULL)
        
        if (length(setdiff(in_boxes, list(AG))) == 0)
            nd = with(heart2, data.frame(age.group = input$radioAgeGroup))
        else if (length(setdiff(in_boxes, list(TR))) == 0)
            nd = with(heart2, data.frame(transplant = input$radioTransplant))
        else if (length(setdiff(in_boxes, list(YG))) == 0)
            nd = with(heart2, data.frame(year.group = input$radioYearGroup))
        else if (length(setdiff(in_boxes, list(SR))) == 0)
            nd = with(heart2, data.frame(surgery = input$radioSurgery))
        else if (length(setdiff(in_boxes, list(AG, TR))) == 0)
            nd = with(heart2, data.frame(age.group = input$radioAgeGroup,
                                         transplant = input$radioTransplant))
        else if (length(setdiff(in_boxes, list(AG, YG))) == 0)
            nd = with(heart2, data.frame(age.group = input$radioAgeGroup,
                                         year.group = input$radioYearGroup))
        else if (length(setdiff(in_boxes, list(AG, SR))) == 0)
            nd = with(heart2, data.frame(age.group = input$radioAgeGroup,
                                         surgery = input$radioSurgery))
        else if (length(setdiff(in_boxes, list(TR, YG))) == 0)
            nd = with(heart2, data.frame(transplant = input$radioTransplant,
                                         year.group = input$radioYearGroup))
        else if (length(setdiff(in_boxes, list(TR, SR))) == 0)
            nd = with(heart2, data.frame(transplant = input$radioTransplant,
                                         surgery = input$radioSurgery))
        else if (length(setdiff(in_boxes, list(YG, SR))) == 0)
            nd = with(heart2, data.frame(year.group = input$radioYearGroup,
                                         surgery = input$radioSurgery))
        else if (length(setdiff(in_boxes, list(AG, TR, YG))) == 0)
            nd = with(heart2, data.frame(age.group = input$radioAgeGroup,
                                         transplant = input$radioTransplant,
                                         year.group = input$radioYearGroup))
        else if (length(setdiff(in_boxes, list(AG, TR, SR))) == 0)
            nd = with(heart2, data.frame(age.group = input$radioAgeGroup,
                                         transplant = input$radioTransplant,
                                         surgery = input$radioSurgery))
        else if (length(setdiff(in_boxes, list(TR, YG, SR))) == 0)
            nd = with(heart2, data.frame(year.group = input$radioYearGroup,
                                         transplant = input$radioTransplant,
                                         surgery = input$radioSurgery))
        else if (length(setdiff(in_boxes, list(AG, TR, YG, SR))) == 0)
            nd = with(heart2, data.frame(age.group = input$radioAgeGroup,
                                         transplant = input$radioTransplant,
                                         year.group = input$radioYearGroup,
                                         surgery = input$radioSurgery))  
        
        ggsurvplot(
            survfit(coxModel(), newdata = nd),
            data = heart2,
            title = "Predicted Survival Curve",
            font.title = c(16, "bold", "darkblue"),
            conf.int = TRUE,
            risk.table.col = "strata",
            ggtheme = theme_bw(),
            break.time.by = 200,
            # legend.labs = legend_labs,
            xlim = c(0, 1800)
        )
    }, height = 400, width = 500)
    
    # Plot the dataset
    output$dataTable <- DT::renderDataTable({
        DT::datatable(heart2[c("time", "status", "transplant", "surgery", 
                               "age.group", "year.group")])
    })
    
    output$datasetOutput <- renderText({
        "The dataset contains information about survival of patients on the 
        waiting list for the Stanford heart transplant program from 1967 
        to 1974. The dataset is taken from 'p3state.msm' package in R."
    })
    
    output$kmOutput <- renderText({
        "On the first tab a user can see the Kaplan-Meier Survival Curve and  
        the Cumulative Hazard Curve for chosen variable of interest. 
        All varaibles are categorical and described in the data dictonary
        at the bottom of the left panel. A user can choose factor of interest
        from Stratification Factor drop-down list."
    })
    
    output$cphOutput <- renderText({
        "On the second tab a user can see a survival curve built with Cox
        Proportional-Hazards Model, model summary and the predicted curve based
        on the user's input. Regressors that a user wants to include in the 
        model can be selected with the tick-boxes on the left panel.
        Model summary is given below the curve and contains useful information
        such as coefficient values and results of statistical tests.
        Survival curve for a new patient will be drawn at the bottom where
        input values are specified on the left panel under 'Prediction' section.
        Only regressors included in the model will have an effect on prediction 
        despite other existing input values."
    })
}
