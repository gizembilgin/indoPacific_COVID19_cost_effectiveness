library(shiny)

CHOICES_booster_vax_scenario = c(
  "high risk adults"
  , "all adults"
  , "all adults who have previously completed their primary schedule but have not recieved a booster"
  , "high-risk adults who have previously completed their primary schedule but have not recieved a booster"
  , "no booster"
)
CHOICES_antiviral_scenario = c(
  "no antiviral",
  "high risk adults",
  "unvaccinated adults",
  "high risk and unvaccinated adults",
  "all adults"
)
CHOICES_outcomes = c("QALY","death","hosp")



##### USER INTERFACE DEFINITION ################################################
ui <- fluidPage(
  
  #COMEBACK - make correspond to Figure/Table # in the paper/SM 
  selectInput("INPUT_select_figure","Select figure:",
              choices = list("Incremental table" = 1, 
                             "Incremental plane" = 2,
                             "WTP curve" = 3,
                             "Tornado plot" = 4), selected = 1),
  checkboxGroupInput("INPUT_include_setting","Settings to include:",
                     choices = c("Fiji","Indonesia","Papua New Guinea","Timor-Leste")),
  
  
  ### Incremental table
  conditionalPanel(
    condition = "input.INPUT_select_figure == 1", 
    
    checkboxGroupInput("INPUT_include_booster_vax_scenario","Booster strategies to include:",
                       choices = CHOICES_booster_vax_scenario), 
    checkboxGroupInput("INPUT_include_antiviral_scenario","Antiviral strategies to include:",
                       choices = CHOICES_antiviral_scenario), 
    radioButtons("INPUT_perspective","Perspective:", c("healthcare perspective","societal perspective")),
    sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
    radioButtons("INPUT_antiviral_cost","Antiviral cost:", c("low_generic_cost","middle_income_cost", "high_income_cost")),
    
    checkboxGroupInput("INPUT_include_outcomes","Include outcomes:",
                       choices = CHOICES_outcomes),
    radioButtons("INPUT_include_net","Include net columns?",
                 choices = c("Yes",
                             "No")),
  ),
  
  
  ### Incremental plane
  conditionalPanel(
    condition = "input.INPUT_select_figure == 2", 
    
    checkboxGroupInput("INPUT_include_booster_vax_scenario","Booster strategies to include:",
                       choices = CHOICES_booster_vax_scenario), 
    checkboxGroupInput("INPUT_include_antiviral_scenario","Antiviral strategies to include:",
                       choices = CHOICES_antiviral_scenario), 
    radioButtons("INPUT_perspective","Perspective:", c("healthcare perspective","societal perspective")),
    sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
    radioButtons("INPUT_antiviral_cost","Antiviral cost:", c("low_generic_cost","middle_income_cost", "high_income_cost")),

    radioButtons("INPUT_include_outcomes","Outcome:",
                 choices = CHOICES_outcomes),
  ),   
  
  
  ### WTP curve
  conditionalPanel(
    condition = "input.INPUT_select_figure == 3", 
    
    radioButtons("INPUT_include_outcomes","Outcome:",
                 choices = CHOICES_outcomes),
    radioButtons("INPUT_perspective","Perspective:", c("healthcare perspective","societal perspective")),
    
    selectInput("INPUT_parameter_to_vary","Select parameter to vary:",
                choices = list("Discounting rate" = 1, 
                               "Antiviral cost" = 2,
                               "Booster strategy" = 3,
                               "Antiviral Strategy" = 4), selected = 1),
    
    conditionalPanel( ## Vary discounting rate
      condition = "input.INPUT_parameter_to_vary == 1",
      
      radioButtons("INPUT_include_booster_vax_scenario","Booster strategy:",
                         choices = CHOICES_booster_vax_scenario), 
      radioButtons("INPUT_include_antiviral_scenario","Antiviral strategy:",
                         choices = CHOICES_antiviral_scenario), 
      checkboxGroupInput("INPUT_discounting_rate","Discounting rate (%):",
                         choices = seq(0,10,by=1)),
      radioButtons("INPUT_antiviral_cost","Antiviral cost:", c("low_generic_cost","middle_income_cost", "high_income_cost")),
      
    ),
    
    conditionalPanel( ## Vary antiviral cost
      condition = "input.INPUT_parameter_to_vary == 2",
      
      radioButtons("INPUT_include_booster_vax_scenario","Booster strategy:",
                   choices = CHOICES_booster_vax_scenario), 
      radioButtons("INPUT_include_antiviral_scenario","Antiviral strategy:",
                   choices = CHOICES_antiviral_scenario), 
      sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
      checkboxGroupInput("INPUT_antiviral_cost","Antiviral cost:", c("low_generic_cost","middle_income_cost", "high_income_cost")),
       
    ),
    
    conditionalPanel( ## Vary booster vaccine scenario
      condition = "input.INPUT_parameter_to_vary == 3",
      
      checkboxGroupInput("INPUT_include_booster_vax_scenario","Booster strategies to include:",
                         choices = CHOICES_booster_vax_scenario), 
      radioButtons("INPUT_include_antiviral_scenario","Antiviral strategy:",
                   choices = CHOICES_antiviral_scenario), 
      sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
      radioButtons("INPUT_antiviral_cost","Antiviral cost:", c("low_generic_cost","middle_income_cost", "high_income_cost")),
      
    ),
    
    conditionalPanel( ## Vary antiviral scenario
      condition = "input.INPUT_parameter_to_vary == 4",
      
      radioButtons("INPUT_include_booster_vax_scenario","Booster strategy:",
                   choices = CHOICES_booster_vax_scenario), 
      checkboxGroupInput("INPUT_include_antiviral_scenario","Antiviral strategies to include:",
                         choices = CHOICES_antiviral_scenario), 
      sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
      radioButtons("INPUT_antiviral_cost","Antiviral cost:", c("low_generic_cost","middle_income_cost", "high_income_cost")),
      
    ),

  ), 
  

  
  ### Tornado plot
  conditionalPanel(
    condition = "input.INPUT_select_figure == 4", 
    
    radioButtons("INPUT_perspective","Perspective:", c("healthcare perspective","societal perspective")),
    radioButtons("INPUT_include_outcomes","Outcome:",
                 choices = CHOICES_outcomes),
    checkboxGroupInput("INPUT_parameters","Parameters to display:",
                       choices = c(
                         "Antiviral schedule price ($25-530)",
                         "Antiviral wastage (0-60%)"          ,
                         "Inpatient costs (±50%)"              ,
                         "Discounting rate (0-5%)"              ,
                         "RAT price ($1-5)"                      ,
                         "RAT wastage factor (3-12)"              ,
                         "Cost per extra LOS (±50%)"               ,
                         "Reduced LOS (±50%)"                       ,
                         "Antiviral operational costs (±50%)"       ,
                         "Booster operational cost ($0.21-$13.04)"  ,
                         "Long COVID (off/on)"                      ,
                         "Booster price ($0.50-$3.00)"              ,
                         "Booster wastage (0-50%)"                  ,
                         "Outpatient costs (±50%)"                  ,
                         "Injection Equipment wastage (0-50%)"      ,
                         "Injection Equipment price ($0.025-$0.050)"
                       )), 
    radioButtons("INPUT_include_GDP","Include GDP as a line?",
                 choices = c("Yes",
                             "No")),
    
  ),

  
)
################################################################################




##### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
}

shinyApp(ui, server)