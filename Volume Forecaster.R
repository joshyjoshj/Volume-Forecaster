#Packages
#####
library(vip)
library(janitor)
library(ranger)
library(tidymodels)
library(tidyverse)
library(glmnet)
library(readxl)
library(shiny)
#Data Import
#####
main <- read_excel("C:/Users/JB/OneDrive/Documents/Gappers Volume.xlsx",na = "NA") #Data import
mainvol <- filter(main, Fade == "Yes") #Selecting faders only
mainvol <- clean_names(mainvol) #Tidying names
mainvol <- mainvol[c(12,13,14,16,17,18,19,22,24,27,31,36,42)]#Selection of VARS  
#####
set.seed(123)
mainvol_split <- initial_split(mainvol, prop = 3/4)
mainvol_train <- training(mainvol_split)
mainvol_test <- testing(mainvol_split)
#####
vol_folds <- vfold_cv(mainvol_train)
#####
v1_tune_spec <-
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
  set_engine("glmnet")



#Grid for hyperparameters


v1_glmnet_grid <- grid_regular(mixture(),
                               penalty(),
                               levels = 10)



#Creating model workflow 


v1_vol_wf <- workflow() %>%
  add_model(v1_tune_spec) %>%
  add_formula(v1 ~ vol0930 * pmve1 * pmve2 * pmve4 * x0pmve * pm_jerk * avg5min)


#Fitting grid to resamples


v1_vol_res <-
  v1_vol_wf %>%
  tune_grid(
    resamples = vol_folds,
    grid = v1_glmnet_grid
  )


#Selecting best model

v1_best_mod <- v1_vol_res %>%
  select_best("rsq")


#Updating values of workflow from the best fitting model 


v1_final_wf <-
  v1_vol_wf %>%
  finalize_workflow(v1_best_mod)


#Fitting to the training data 


v1_final_fit <-
  v1_final_wf %>%
  fit(data  = mainvol_train)




#####
v2_tune_spec <-
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
  set_engine("glmnet")



#Grid for hyperparameters


v2_glmnet_grid <- grid_regular(mixture(),
                               penalty(),
                               levels = 10)



#Creating model workflow 


v2_vol_wf <- workflow() %>%
  add_model(v2_tune_spec) %>%
  add_formula(v2_2 ~ vol0930 * pmve1 * pmve2 * pmve4 * x0pmve * pm_jerk * avg5min)


#Fitting grid to resamples


v2_vol_res <-
  v2_vol_wf %>%
  tune_grid(
    resamples = vol_folds,
    grid = v2_glmnet_grid
  )


#Selecting best model

v2_best_mod <- v2_vol_res %>%
  select_best("rsq")


#Updating values of workflow from the best fitting model 


v2_final_wf <-
  v2_vol_wf %>%
  finalize_workflow(v2_best_mod)


#Fitting to the training data 


v2_final_fit <-
  v2_final_wf %>%
  fit(data  = mainvol_train)




#####
v3_tune_spec <-
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
  set_engine("glmnet")



#Grid for hyperparameters


v3_glmnet_grid <- grid_regular(mixture(),
                               penalty(),
                               levels = 10)



#Creating model workflow 


v3_vol_wf <- workflow() %>%
  add_model(v3_tune_spec) %>%
  add_formula(v3_3 ~ vol0930 * pmve1 * pmve2 * pmve4 * x0pmve * pm_jerk * avg5min)


#Fitting grid to resamples


v3_vol_res <-
  v3_vol_wf %>%
  tune_grid(
    resamples = vol_folds,
    grid = v3_glmnet_grid
  )


#Selecting best model

v3_best_mod <- v3_vol_res %>%
  select_best("rsq")


#Updating values of workflow from the best fitting model 


v3_final_wf <-
  v3_vol_wf %>%
  finalize_workflow(v3_best_mod)


#Fitting to the training data 


v3_final_fit <-
  v3_final_wf %>%
  fit(data  = mainvol_train)





#####
v4_tune_spec <-
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
  set_engine("glmnet")



#Grid for hyperparameters


v4_glmnet_grid <- grid_regular(mixture(),
                               penalty(),
                               levels = 10)



#Creating model workflow 


v4_vol_wf <- workflow() %>%
  add_model(v4_tune_spec) %>%
  add_formula(v4_4 ~ vol0930 * pmve1 * pmve2 * pmve4 * x0pmve * pm_jerk * avg5min)


#Fitting grid to resamples


v4_vol_res <-
  v4_vol_wf %>%
  tune_grid(
    resamples = vol_folds,
    grid = v4_glmnet_grid
  )


#Selecting best model

v4_best_mod <- v4_vol_res %>%
  select_best("rsq")


#Updating values of workflow from the best fitting model 


v4_final_wf <-
  v4_vol_wf %>%
  finalize_workflow(v4_best_mod)


#Fitting to the training data 


v4_final_fit <-
  v4_final_wf %>%
  fit(data  = mainvol_train)


v4_pred <-
  v4_final_fit %>%
  predict(ticker_data)


#####
v5_tune_spec <-
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
  set_engine("glmnet")



#Grid for hyperparameters


v5_glmnet_grid <- grid_regular(mixture(),
                               penalty(),
                               levels = 10)



#Creating model workflow 


v5_vol_wf <- workflow() %>%
  add_model(v5_tune_spec) %>%
  add_formula(v5_5 ~ vol0930 * pmve1 * pmve2 * pmve4 * x0pmve * pm_jerk * avg5min)


#Fitting grid to resamples


v5_vol_res <-
  v5_vol_wf %>%
  tune_grid(
    resamples = vol_folds,
    grid = v5_glmnet_grid
  )


#Selecting best model

v5_best_mod <- v5_vol_res %>%
  select_best("rsq")


#Updating values of workflow from the best fitting model 


v5_final_wf <-
  v5_vol_wf %>%
  finalize_workflow(v5_best_mod)
a

#Fitting to the training data 


v5_final_fit <-
  v5_final_wf %>%
  fit(data  = mainvol_train)


v5_pred <-
  v5_final_fit %>%
  predict(ticker_data)






#####



ui <- fluidPage(
  tabsetPanel(
  tabPanel("Volume Forecast",
  wellPanel(
    fluidRow(
      column(3,
             textInput(inputId = "Ticker", label = "Ticker", value = ""),
             numericInput(inputId = "PMV1", label = "PMV1", value = 0),
             numericInput(inputId = "PMV2", label = "PMV2", value = 0),
             numericInput(inputId = "PMV3", label = "PMV3", value = 0),
             numericInput(inputId = "PMV4", label = "PMV4", value = 0),
             numericInput(inputId = "PMV5", label = "PMV5", value = 0),
             numericInput(inputId = "vol0931", label  = "vol0931", value = 0)
      ),
      column(3,
             textInput(inputId = "Ticker_2", label = "Ticker", value = ""),
             numericInput(inputId = "PMV1_2", label = "PMV1", value = 0),
             numericInput(inputId = "PMV2_2", label = "PMV2", value = 0),
             numericInput(inputId = "PMV3_2", label = "PMV3", value = 0),
             numericInput(inputId = "PMV4_2", label = "PMV4", value = 0),
             numericInput(inputId = "PMV5_2", label = "PMV5", value = 0),
             numericInput(inputId = "vol0931_2", label  = "vol0931", value = 0)
      ),
      column(3,
             textInput(inputId = "Ticker_3", label = "Ticker", value = ""),
             numericInput(inputId = "PMV1_3", label = "PMV1", value = 0),
             numericInput(inputId = "PMV2_3", label = "PMV2", value = 0),
             numericInput(inputId = "PMV3_3", label = "PMV3", value = 0),
             numericInput(inputId = "PMV4_3", label = "PMV4", value = 0),
             numericInput(inputId = "PMV5_3", label = "PMV5", value = 0),
             numericInput(inputId = "vol0931_3", label  = "vol0931", value = 0)
      ),
      column(3,
             textInput(inputId = "Ticker_4", label = "Ticker", value = ""),
             numericInput(inputId = "PMV1_4", label = "PMV1", value = 0),
             numericInput(inputId = "PMV2_4", label = "PMV2", value = 0),
             numericInput(inputId = "PMV3_4", label = "PMV3", value = 0),
             numericInput(inputId = "PMV4_4", label = "PMV4", value = 0),
             numericInput(inputId = "PMV5_4", label = "PMV5", value = 0),
             numericInput(inputId = "vol0931_4", label  = "vol0931", value = 0))
    )),
  fluidRow(
    wellPanel(
      tableOutput("table"),
      actionButton("table_2_action", label = "Percent Calculation"),
      tableOutput("table_2")
    )
  ),
  fluidRow(
    wellPanel(
      tableOutput("table2"),
      actionButton("table2_2_action", label = "Percent Calculation"),
      tableOutput("table2_2")
    )
  ),
  fluidRow(
    wellPanel(
      tableOutput("table3"),
      actionButton("table3_2_action", label = "Percent Calculation"),
      tableOutput("table3_2")
    )
  ),
  fluidRow(
    wellPanel(
      tableOutput("table4"),
      actionButton("table4_2_action", label = "Percent Calculation"),
      tableOutput("table4_2")
    )
  )),
  tabPanel("Ticker Data",
           inputPanel(
             textInput(inputId = "TickerData", label = "Ticker"),
             dateInput(inputId = "Date", label = "Date")
             
           ),
           wellPanel(
             tableOutput(outputId = "summarytable")  
           ),
           wellPanel(
             tableOutput(outputId = "maintable")
           )
  )
  
  
  )
  
)


server <- function(input,output) {
  
  
  inputs <- reactive({
    tibble(vol0930 = input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5,
           pmve1 = (((vol0930)-(input$PMV1))/120),
           pmve2 = (((vol0930)-(input$PMV1+input$PMV2))/90),
           pmve4 = (((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4))/30),
           x0pmve = (vol0930/120),
           pm_jerk = (x0pmve/120),
           avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4+inputPMV5)))))
    )
  })
  
  
  timer <- reactiveTimer(1000)
  
  
  #Column 1 
  v1_predic <- reactive({
    v1_final_fit %>%
      predict(tibble(vol0930 = input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5,
                     pmve1 = (((vol0930)-(input$PMV1))/120),
                     pmve2 = (((vol0930)-(input$PMV1+input$PMV2))/90),
                     pmve4 = (((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5)))))
      ))
    
  })
  
  
  v2_predic <- reactive({
    v2_final_fit %>%
      predict(tibble(vol0930 = input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5,
                     pmve1 = (((vol0930)-(input$PMV1))/120),
                     pmve2 = (((vol0930)-(input$PMV1+input$PMV2))/90),
                     pmve4 = (((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5)))))
      ))
    
  })
  
  
  v3_predic <- reactive({
    v3_final_fit %>%
      predict(tibble(vol0930 = input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5,
                     pmve1 = (((vol0930)-(input$PMV1))/120),
                     pmve2 = (((vol0930)-(input$PMV1+input$PMV2))/90),
                     pmve4 = (((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5)))))
      ))
    
  })
  
  
  v4_predic <- reactive({
    v4_final_fit %>%
      predict(tibble(vol0930 = input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5,
                     pmve1 = (((vol0930)-(input$PMV1))/120),
                     pmve2 = (((vol0930)-(input$PMV1+input$PMV2))/90),
                     pmve4 = (((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5)))))
      ))
    
  })
  
  
  v5_predic <- reactive({
    v5_final_fit %>%
      predict(tibble(vol0930 = input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5,
                     pmve1 = (((vol0930)-(input$PMV1))/120),
                     pmve2 = (((vol0930)-(input$PMV1+input$PMV2))/90),
                     pmve4 = (((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1+input$PMV2+input$PMV3+input$PMV4+input$PMV5)))))
      ))
    
  })
  
  
  tibble_output <- reactive({
    tibble(
      Ticker = input$Ticker,
      vol0931 = input$vol0931
    )
  })
  
  
  output$table <- renderTable({
    format.data.frame(tibble_output() %>%
                        mutate(vol0935pred = (vol0931+(5*v1_predic()$.pred))) %>%
                        mutate(vol0945pred = (vol0935pred+(10*v2_predic()$.pred))) %>%
                        mutate(vol1000pred = (vol0945pred+(15*v3_predic()$.pred))) %>%
                        mutate(vol1100pred = (vol1000pred+(60*v4_predic()$.pred))) %>%
                        mutate(vol1600pred = (vol1100pred+(300*v5_predic()$.pred))))
    
  }) 
  
  
  
  vol_percent_table_1 <- eventReactive(input$table_2_action, {
    tibble(
      current_volume = ((((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/quote-short/%s?apikey=c34a828bdd72b3933d449ea419de9562", input$Ticker))))[c(3)])$volume),
      vol1600pred = input$vol0931+(5*v1_predic()$.pred)+(10*v2_predic()$.pred)+(15*v3_predic()$.pred)+(60*v4_predic()$.pred)+(300*v5_predic()$.pred),
      percent_of_daily_forecast = (current_volume/vol1600pred)*100
    )
  })
  
  
  
  output$table_2 <- renderTable({
    vol_percent_table_1()
  })
  
  
  
  
  
  #Column 2 
  
  v1_predic_2 <- reactive({
    v1_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2,
                     pmve1 = (((vol0930)-(input$PMV1_2))/120),
                     pmve2 = (((vol0930)-(input$PMV1_2+input$PMV2_2))/90),
                     pmve4 = (((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2)))))
      ))
    
  })
  
  
  v2_predic_2 <- reactive({
    v2_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2,
                     pmve1 = (((vol0930)-(input$PMV1_2))/120),
                     pmve2 = (((vol0930)-(input$PMV1_2+input$PMV2_2))/90),
                     pmve4 = (((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2)))))
      ))
    
  })
  
  
  v3_predic_2 <- reactive({
    v3_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2,
                     pmve1 = (((vol0930)-(input$PMV1_2))/120),
                     pmve2 = (((vol0930)-(input$PMV1_2+input$PMV2_2))/90),
                     pmve4 = (((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2)))))
      ))
    
  })
  
  
  v4_predic_2 <- reactive({
    v4_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2,
                     pmve1 = (((vol0930)-(input$PMV1_2))/120),
                     pmve2 = (((vol0930)-(input$PMV1_2+input$PMV2_2))/90),
                     pmve4 = (((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2)))))
      ))
    
  })
  
  
  v5_predic_2 <- reactive({
    v5_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2,
                     pmve1 = (((vol0930)-(input$PMV1_2))/120),
                     pmve2 = (((vol0930)-(input$PMV1_2+input$PMV2_2))/90),
                     pmve4 = (((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_2+input$PMV2_2+input$PMV3_2+input$PMV4_2+input$PMV5_2)))))
      ))
    
  })
  
  
  tibble_output_2 <- reactive({
    tibble(
      Ticker = input$Ticker_2,
      vol0931 = input$vol0931_2
    )
  })
  
  
  output$table2 <- renderTable({
    format.data.frame(tibble_output_2() %>%
                        mutate(vol0935pred = (vol0931+(5*v1_predic_2()$.pred))) %>%
                        mutate(vol0945pred = (vol0935pred+(10*v2_predic_2()$.pred))) %>%
                        mutate(vol1000pred = (vol0945pred+(15*v3_predic_2()$.pred))) %>%
                        mutate(vol1100pred = (vol1000pred+(60*v4_predic_2()$.pred))) %>%
                        mutate(vol1600pred = (vol1100pred+(300*v5_predic_2()$.pred))))
  }) 
  
  
  
  
  vol_percent_table_2 <- eventReactive(input$table2_2_action,{
    tibble(
      current_volume = ((((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/quote-short/%s?apikey=c34a828bdd72b3933d449ea419de9562", input$Ticker_2))))[c(3)])$volume),
      vol1600pred = input$vol0931_2+(5*v1_predic_2()$.pred)+(10*v2_predic_2()$.pred)+(15*v3_predic_2()$.pred)+(60*v4_predic_2()$.pred)+(300*v5_predic_2()$.pred),
      percent_of_daily_forecast = (current_volume/vol1600pred)*100
    )
  })
  
  
  
  output$table2_2 <- renderTable({
    vol_percent_table_2()
  })
  
  #Column 3 
  
  v1_predic_3 <- reactive({
    v1_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3,
                     pmve1 = (((vol0930)-(input$PMV1_3))/120),
                     pmve2 = (((vol0930)-(input$PMV1_3+input$PMV2_3))/90),
                     pmve4 = (((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3)))))
      ))
    
  })
  
  
  v2_predic_3 <- reactive({
    v2_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3,
                     pmve1 = (((vol0930)-(input$PMV1_3))/120),
                     pmve2 = (((vol0930)-(input$PMV1_3+input$PMV2_3))/90),
                     pmve4 = (((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3)))))
      ))
    
  })
  
  
  v3_predic_3 <- reactive({
    v3_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3,
                     pmve1 = (((vol0930)-(input$PMV1_3))/120),
                     pmve2 = (((vol0930)-(input$PMV1_3+input$PMV2_3))/90),
                     pmve4 = (((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3)))))
      ))
    
  })
  
  
  v4_predic_3 <- reactive({
    v4_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3,
                     pmve1 = (((vol0930)-(input$PMV1_3))/120),
                     pmve2 = (((vol0930)-(input$PMV1_3+input$PMV2_3))/90),
                     pmve4 = (((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3)))))
      ))
    
  })
  
  
  v5_predic_3 <- reactive({
    v5_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3,
                     pmve1 = (((vol0930)-(input$PMV1_3))/120),
                     pmve2 = (((vol0930)-(input$PMV1_3+input$PMV2_3))/90),
                     pmve4 = (((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_3+input$PMV2_3+input$PMV3_3+input$PMV4_3+input$PMV5_3)))))
      ))
    
  })
  
  
  tibble_output_3 <- reactive({
    tibble(
      Ticker = input$Ticker_3,
      vol0931 = input$vol0931_3
    )
  })
  
  
  output$table3 <- renderTable({
    format.data.frame(tibble_output_3() %>%
                        mutate(vol0935pred = (vol0931+(5*v1_predic_3()$.pred))) %>%
                        mutate(vol0945pred = (vol0935pred+(10*v2_predic_3()$.pred))) %>%
                        mutate(vol1000pred = (vol0945pred+(15*v3_predic_3()$.pred))) %>%
                        mutate(vol1100pred = (vol1000pred+(60*v4_predic_3()$.pred))) %>%
                        mutate(vol1600pred = (vol1100pred+(300*v5_predic_3()$.pred))))
  }) 
  
  
  
  
  
  vol_percent_table_3 <- eventReactive(input$table3_2_action, {
    tibble(
      current_volume = ((((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/quote-short/%s?apikey=c34a828bdd72b3933d449ea419de9562", input$Ticker_3))))[c(3)])$volume),
      vol1600pred = input$vol0931_3+(5*v1_predic_3()$.pred)+(10*v2_predic_3()$.pred)+(15*v3_predic_3()$.pred)+(60*v4_predic_3()$.pred)+(300*v5_predic_3()$.pred),
      percent_of_daily_forecast = (current_volume/vol1600pred)*100
    )
  })
  
  
  
  output$table3_2 <- renderTable({
    vol_percent_table_3()
  })
  
  
  #Column 4
  
  v1_predic_4 <- reactive({
    v1_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4,
                     pmve1 = (((vol0930)-(input$PMV1_4))/120),
                     pmve2 = (((vol0930)-(input$PMV1_4+input$PMV2_4))/90),
                     pmve4 = (((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4)))))
      ))
    
  })
  
  
  v2_predic_4 <- reactive({
    v2_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4,
                     pmve1 = (((vol0930)-(input$PMV1_4))/120),
                     pmve2 = (((vol0930)-(input$PMV1_4+input$PMV2_4))/90),
                     pmve4 = (((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4)))))
      ))
    
  })
  
  
  v3_predic_4 <- reactive({
    v3_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4,
                     pmve1 = (((vol0930)-(input$PMV1_4))/120),
                     pmve2 = (((vol0930)-(input$PMV1_4+input$PMV2_4))/90),
                     pmve4 = (((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4)))))
      ))
    
  })
  
  
  v4_predic_4 <- reactive({
    v4_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4,
                     pmve1 = (((vol0930)-(input$PMV1_4))/120),
                     pmve2 = (((vol0930)-(input$PMV1_4+input$PMV2_4))/90),
                     pmve4 = (((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4)))))
      ))
    
  })
  
  
  v5_predic_4 <- reactive({
    v5_final_fit %>%
      predict(tibble(vol0930 = input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4,
                     pmve1 = (((vol0930)-(input$PMV1_4))/120),
                     pmve2 = (((vol0930)-(input$PMV1_4+input$PMV2_4))/90),
                     pmve4 = (((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4))/30),
                     x0pmve = (vol0930/120),
                     pm_jerk = (x0pmve/120),
                     avg5min = (mean(x0pmve,pmve1,pmve2,pmve4,(((vol0930)-(input$PMV1_4+input$PMV2_4+input$PMV3_4+input$PMV4_4+input$PMV5_4)))))
      ))
    
  })
  
  
  tibble_output_4 <- reactive({
    tibble(
      Ticker = input$Ticker_4,
      vol0931 = input$vol0931_4
    )
  })
  
  
  output$table4 <- renderTable({
    format.data.frame(tibble_output_4() %>%
                        mutate(vol0935pred = (vol0931+(5*v1_predic_4()$.pred))) %>%
                        mutate(vol0945pred = (vol0935pred+(10*v2_predic_4()$.pred))) %>%
                        mutate(vol1000pred = (vol0945pred+(15*v3_predic_4()$.pred))) %>%
                        mutate(vol1100pred = (vol1000pred+(60*v4_predic_4()$.pred))) %>%
                        mutate(vol1600pred = (vol1100pred+(300*v5_predic_4()$.pred))))
  }) 
  
  
  
  
  
  vol_percent_table_4 <- eventReactive(input$table4_2_action, {
    timer()
    tibble(
      current_volume = ((((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/quote-short/%s?apikey=c34a828bdd72b3933d449ea419de9562", input$Ticker_4))))[c(3)])$volume),
      vol1600pred = input$vol0931_4+(5*v1_predic_4()$.pred)+(10*v2_predic_4()$.pred)+(15*v3_predic_4()$.pred)+(60*v4_predic_4()$.pred)+(300*v5_predic_4()$.pred),
      percent_of_daily_forecast = (current_volume/vol1600pred)*100
    )
  })
  
  
  
  output$table4_2 <- renderTable({
    vol_percent_table_4()
  })
  
  
  data_fr <- reactive({
    tibble(
      ((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/historical-price-full/%s?apikey=c34a828bdd72b3933d449ea419de9562", input$TickerData))))[["historical"]]
    )
  })
  
  output$maintable <- renderTable({
    data_fr() %>%
      filter(date == input$Date)
  })
  
  main_data <- reactive({
    tibble(
      Market_Capitalization = ((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/key-metrics-ttm/%s?limit=40&apikey=c34a828bdd72b3933d449ea419de9562", input$TickerData))))[c(10)],
      Enterprise_Value = ((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/key-metrics-ttm/%s?limit=40&apikey=c34a828bdd72b3933d449ea419de9562", input$TickerData))))[c(11)],
      Book_Value_Per_Share = ((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/key-metrics-ttm/%s?limit=40&apikey=c34a828bdd72b3933d449ea419de9562", input$TickerData))))[c(6)],
      Working_Capital = ((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/key-metrics-ttm/%s?limit=40&apikey=c34a828bdd72b3933d449ea419de9562", input$TickerData))))[c(44)],
      DCF = ((jsonlite::fromJSON(sprintf("https://financialmodelingprep.com/api/v3/discounted-cash-flow/%s?apikey=c34a828bdd72b3933d449ea419de9562", input$TickerData))))[c(3)]
      
    )
  })
  
  output$summarytable <- renderTable(
    main_data()
  )
  
  
  
  
}


shinyApp(ui = ui, server = server)






