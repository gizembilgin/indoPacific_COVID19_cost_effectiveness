require(shiny); require(gridExtra)
options(scipen = 1000)

#load("x_results/CommandDeck_result_list_adults_with_comorbidities_healthcare_perspective_2023-07-10 10-06-30.Rdata")

### aligning results with buttons
CommandDeck_result_long = CommandDeck_result_list$CommandDeck_result_long %>%
  mutate(
    setting = case_when(
      setting == "FJI" ~ "Fiji",
      setting == "IDN" ~ "Indonesia",
      setting == "PNG" ~ "Papua New Guinea",
      setting == "TLS" ~ "Timor-Leste",
      TRUE ~ setting
    ),
    booster_vax_scenario = case_when(
      booster_vax_scenario == "booster to all high-risk adults previously willing to be vaccinated" ~ "high risk adults",
      booster_vax_scenario == "booster to all adults previously willing to be vaccinated" ~ "all adults"    ,
      booster_vax_scenario == "booster dose catch-up campaign for all adults" ~ "all adults who have previously completed their primary schedule but have not recieved a booster"  ,
      booster_vax_scenario == "booster dose catch-up campaign for high-risk adults" ~ "high-risk adults who have previously completed their primary schedule but have not recieved a booster"   ,
      booster_vax_scenario == "no booster dose" ~ "no booster",
      TRUE ~ booster_vax_scenario
    ),
    
    #COMEBACK - build in radio button for M vs NR
    antiviral_type = case_when(
      antiviral_scenario %in% c("molunipiravir 2023-01-01 adults_with_comorbidities","molunipiravir 2023-01-01 all_adults", "molunipiravir 2023-01-01 unvaccinated_adults") ~ "molunipiravir",
      antiviral_scenario %in% c("nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities", "nirmatrelvir_ritonavir 2023-01-01 all_adults", "nirmatrelvir_ritonavir 2023-01-01 unvaccinated_adults") ~  "nirmatrelvir_ritonavir"
    ),
    #COMEBACK - this data structure is likely to change soon
    antiviral_scenario = case_when(
      antiviral_scenario %in% c("molunipiravir 2023-01-01 adults_with_comorbidities","nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities") ~ "high risk adults",
      antiviral_scenario %in% c("nirmatrelvir_ritonavir 2023-01-01 all_adults","molunipiravir 2023-01-01 all_adults") ~ "all adults",
      antiviral_scenario %in% c("molunipiravir 2023-01-01 unvaccinated_adults","nirmatrelvir_ritonavir 2023-01-01 unvaccinated_adults" ) ~ "unvaccinated adults",
      antiviral_scenario == "no antiviral" ~ "no antiviral",
      TRUE ~ antiviral_scenario
    ),
    perspective = paste(perspective," perspective",sep = ""),      
    discounting_rate = discounting_rate * 100
  ) 
#_______________________________________________________________________________



CHOICES_include_setting = c("Fiji", "Indonesia", "Papua New Guinea", "Timor-Leste")
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
CHOICES_outcomes = list("QALYs" = "QALYs",
                     "death" = "death",
                     "hosp" = "hosp")
CHOICES_antiviral_cost = list("low generic cost" = "low_generic_cost",
                              "middle income cost" = "middle_income_cost", 
                              "high income cost" = "high_income_cost")
CHOICES_perspective = list("healthcare perspective" = "healthcare perspective",
                           "societal perspective" = "societal perspective" )

CommandDeck_result_long %>%
  filter(setting %in% CHOICES_include_setting &
           booster_vax_scenario %in% CHOICES_booster_vax_scenario & 
           antiviral_scenario %in% CHOICES_antiviral_scenario & 
           outcome %in% CHOICES_outcomes &
           perspective %in% c("healthcare perspective","societal perspective") )


##### USER INTERFACE DEFINITION ################################################
ui <- fluidPage(
  
  titlePanel("Interactive cost-effectiveness analysis of COVID-19 oral antivirals and booster doses in the Indo-Pacific"),
  sidebarLayout(
    
    sidebarPanel(
      #COMEBACK - make correspond to Figure/Table # in the paper/SM 
      selectInput("INPUT_select_figure","Select figure:",
                  choices = list("Incremental table" = 1, 
                                 "Incremental plane" = 2,
                                 "WTP curve" = 3,
                                 "Tornado plot" = 4), selected = 1),
      checkboxGroupInput("INPUT_include_setting","Settings to include:",
                         choices = CHOICES_include_setting),
      
      
      ### Incremental table
      conditionalPanel(
        condition = "input.INPUT_select_figure == 1", 
        
        checkboxGroupInput("INPUT_include_booster_vax_scenario","Booster strategies to include:",
                           choices = CHOICES_booster_vax_scenario), 
        checkboxGroupInput("INPUT_include_antiviral_scenario","Antiviral strategies to include:",
                           choices = CHOICES_antiviral_scenario), 
        radioButtons("INPUT_perspective",
                     label = "Perspective:", 
                     choices = CHOICES_perspective,
                     selected = "healthcare perspective"),
        sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
        radioButtons("antiviral_cost_1", label = h3("Antiviral cost:"),
                     choices = list("low generic cost" = "low_generic_cost",
                                    "middle income cost" = "middle_income_cost", 
                                    "high income cost" = "high_income_cost"), 
                     selected = "middle_income_cost"),
        
        checkboxGroupInput("INPUT_include_outcomes","Include outcomes:",
                           choices = CHOICES_outcomes),
        radioButtons("INPUT_include_net","Include net columns?",
                     choices = c("Yes",
                                 "No"))
      ),
      
      
      ### Incremental plane
      conditionalPanel(
        condition = "input.INPUT_select_figure == 2", 
        
        checkboxGroupInput("INPUT_include_booster_vax_scenario_2","Booster strategies to include:",
                           choices = CHOICES_booster_vax_scenario), 
        checkboxGroupInput("INPUT_include_antiviral_scenario_2","Antiviral strategies to include:",
                           choices = CHOICES_antiviral_scenario), 
        radioButtons("INPUT_perspective_2","Perspective:",
                     choices = CHOICES_perspective),
        sliderInput("INPUT_discounting_rate_2",h4("Discounting rate (%):"),
                    value = 3, min = 0, max = 10),
        radioButtons("INPUT_antiviral_cost_2", label = h4("Antiviral cost:"),
                     choices = list("low generic cost" = "low_generic_cost",
                                    "middle income cost" = "middle_income_cost", 
                                    "high income cost" = "high_income_cost"), 
                     selected = "middle_income_cost"),
        radioButtons("INPUT_include_outcomes_2","Outcome:",
                     choices = CHOICES_outcomes)
        

      ),   
      
      
      ### WTP curve
      conditionalPanel(
        condition = "input.INPUT_select_figure == 3", 
        
        radioButtons("INPUT_include_outcomes","Outcome:",
                     choices = CHOICES_outcomes),
        radioButtons("INPUT_perspective","Perspective:", CHOICES_perspective),
        
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
          radioButtons("INPUT_antiviral_cost","Antiviral cost:", CHOICES_antiviral_cost)
          
        ),
        
        conditionalPanel( ## Vary antiviral cost
          condition = "input.INPUT_parameter_to_vary == 2",
          
          radioButtons("INPUT_include_booster_vax_scenario","Booster strategy:",
                       choices = CHOICES_booster_vax_scenario), 
          radioButtons("INPUT_include_antiviral_scenario","Antiviral strategy:",
                       choices = CHOICES_antiviral_scenario), 
          sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
          checkboxGroupInput("INPUT_antiviral_cost","Antiviral cost:", CHOICES_antiviral_cost)
          
        ),
        
        conditionalPanel( ## Vary booster vaccine scenario
          condition = "input.INPUT_parameter_to_vary == 3",
          
          checkboxGroupInput("INPUT_include_booster_vax_scenario","Booster strategies to include:",
                             choices = CHOICES_booster_vax_scenario), 
          radioButtons("INPUT_include_antiviral_scenario","Antiviral strategy:",
                       choices = CHOICES_antiviral_scenario), 
          sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
          radioButtons("INPUT_antiviral_cost","Antiviral cost:", CHOICES_antiviral_cost)
          
        ),
        
        conditionalPanel( ## Vary antiviral scenario
          condition = "input.INPUT_parameter_to_vary == 4",
          
          radioButtons("INPUT_include_booster_vax_scenario","Booster strategy:",
                       choices = CHOICES_booster_vax_scenario), 
          checkboxGroupInput("INPUT_include_antiviral_scenario","Antiviral strategies to include:",
                             choices = CHOICES_antiviral_scenario), 
          sliderInput("INPUT_discounting_rate","Discounting rate (%):", value = 3, min = 0, max = 10),
          radioButtons("INPUT_antiviral_cost","Antiviral cost:", CHOICES_antiviral_cost)
          
        ),
        
      ), 
      
      
      
      ### Tornado plot
      conditionalPanel(
        condition = "input.INPUT_select_figure == 4", 
        
        radioButtons("INPUT_perspective","Perspective:", choices = CHOICES_perspective),
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
        
      )
    ),
    
    
    mainPanel(
      
      conditionalPanel(
        condition = "input.INPUT_select_figure == 1",
        dataTableOutput("OUTPUT_ICER_table")
      ),
      conditionalPanel(
        condition = "input.INPUT_select_figure == 2",
        textOutput("testing_2"),
        plotOutput("OUTPUT_incremental_plane")
      ),
      conditionalPanel(
        condition = "input.INPUT_select_figure == 3",
        plotOutput("OUTPUT_WTP_curve")
      ),
      conditionalPanel(
        condition = "input.INPUT_select_figure == 4",
        plotOutput("OUTPUT_tornado_plot")
      )
    )
  )
  
  
)
################################################################################




##### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
  
  dataInput <- reactive({
    CommandDeck_result_long %>%
      filter(outcome == input$INPUT_include_outcomes_2 &
               setting %in% input$INPUT_include_setting &
               perspective == input$INPUT_perspective_2  &
               discounting_rate == input$INPUT_discounting_rate_2 &
               antiviral_cost == input$INPUT_antiviral_cost_2 &
               booster_vax_scenario %in% input$INPUT_include_booster_vax_scenario_2 &
               antiviral_scenario %in% input$INPUT_include_antiviral_scenario_2)
  }) 
  
  output$testing_2 <- renderText({
    
    to_plot <- dataInput()
    paste("Rows:", nrow(to_plot))
  })
  
  output$OUTPUT_incremental_plane <- renderPlot({
    
    to_plot <- dataInput()
    
    if (nrow(to_plot)>1){
      ### Create plots
      plot_list = list()
      for (this_setting in input$INPUT_include_setting){
        to_plot_setting = to_plot[to_plot$setting == this_setting,]
        if (length(input$INPUT_include_antiviral_scenario_2)>1){
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=netCost,y=count_outcomes_averted,color=as.factor(antiviral_scenario))) +
            labs(color="antiviral strategy")
        } else if (length(input$INPUT_include_booster_vax_scenario_2)>1){
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=netCost,y=count_outcomes_averted,color=as.factor(booster_vax_scenario))) +
            labs(color="booster strategy")
        } else{
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=netCost,y=count_outcomes_averted))
        }
        plot_list[[length(plot_list)]] = plot_list[[length(plot_list)]] +
          ylab("QALYs averted") +
          xlab("net cost (2022 USD)") +
          theme_bw() +
          theme(legend.position="bottom") +
          labs(title = this_setting) #+
          #ylim(0,max(to_plot_setting$count_outcomes_averted))

      }
      plot_list
      
      if(length(plot_list) == 1){row_num = 1; col_num = 1}
      if(length(plot_list) == 2){row_num = 1; col_num = 2}
      if(length(plot_list) > 2){row_num = 2; col_num = 2}
      
      grid.arrange(grobs = plot_list,nrow = row_num, ncol = col_num)
    }
    
      ### Arrange plots based no number of settings
      # if (length(plot_list) == 1){
      #   plot_list
      # } else if (length(plot_list) == 2){
      #   ggarrange(plot_list[[1]],plot_list[[2]], ncol = 1, nrow = 2, common.legend = TRUE)
      # } else if (length(plot_list) == 3){
      #   ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], ncol = 2, nrow = 2, common.legend = TRUE)
      # } else if (length(plot_list) == 4){
      #   ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]], ncol = 2, nrow = 2, common.legend = TRUE)
      # }
    
  })
  
}

shinyApp(ui, server)