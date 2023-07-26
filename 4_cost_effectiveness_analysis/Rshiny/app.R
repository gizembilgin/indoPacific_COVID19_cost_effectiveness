require(beepr); require(ggplot2); require(gridExtra); require(ggpubr); require(tidyverse); require(shiny)
options(scipen = 1000)



##### LOAD LATEST RESULTS ######################################################
### load latest probabilistic results
rootpath = str_replace(getwd(), "Rshiny","")
list_poss_Rdata = list.files(
  path = "x_results/",
  pattern = "probab_result*"
)
if (length(list_poss_Rdata) > 0) {
  list_poss_Rdata_details = double()
  for (j in 1:length(list_poss_Rdata)) {
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste0(rootpath,'/x_results/', list_poss_Rdata[[j]]))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste0(rootpath,"/x_results/", latest_file))
}
CommandDeck_result_long <- probab_result$CommandDeck_result_long
CommandDeck_result      <- probab_result$CommandDeck_result
CEAC_dataframe          <- probab_result$CEAC_dataframe

### load latest deterministic results
load(file = paste0(rootpath,"/x_results/tornado_result.Rdata"))
################################################################################




##### CONFIGURE CHOICES ########################################################
CHOICES = list(
  antiviral_cost = list("low generic cost" = "low_generic_cost",
                                "middle income cost" = "middle_income_cost", 
                                "high income cost" = "high_income_cost"),
  antiviral_target_group = c("all adults", 
                             "adults with comorbidities", 
                             "unvaccinated adults",
                             "no antiviral"),
  antiviral_type = list("molunipiravir" = "molunipiravir",
                        "nirmatrelvir_ritonavir" = "nirmatrelvir_ritonavir"),
  booster_vax_scenario = c("high risk adults"
                           , "all adults"
                           , "all adults who have previously completed their primary schedule but have not recieved a booster"
                           , "high-risk adults who have previously completed their primary schedule but have not recieved a booster"
                           , "no booster"),
  discounting = list("0%" = 0,
                     "1%" = 1,
                     "2%" = 2,
                     "3%" = 3,
                     "4%" = 4,
                     "5%" = 5),
  outcome = list("QALYs" = "QALYs",
                  "death" = "death",
                  "hosp" = "hosp"),
  perspective = list("healthcare perspective" = "healthcare perspective",
                     "societal perspective" = "societal perspective" ),
  setting = c("Fiji", "Indonesia", "Papua New Guinea", "Timor-Leste"),
  tornado_plot_parameters = c(
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
  )
)

check = CommandDeck_result_long %>%
  filter(setting %in% CHOICES$setting &
           booster_vax_scenario %in% CHOICES$booster_vax_scenario & 
           antiviral_target_group %in% CHOICES$antiviral_target_group & 
           antiviral_type %in% CHOICES$antiviral_type &
           outcome %in% CHOICES$outcome &
           perspective %in% c("healthcare perspective","societal perspective") )
if (nrow(check)==0){stop("something wrong with CHOICES")}
################################################################################




##### USER INTERFACE DEFINITION ################################################
ui <- fluidPage(
  
  titlePanel("Interactive cost-effectiveness analysis of COVID-19 oral antivirals and booster doses in the Indo-Pacific"),
  h6("This R Shiny accompanies the working paper <doi link once submitted>"),
  
  sidebarLayout(

    ### Toggles  
    sidebarPanel( width = 3,
                  ###Available in all plots
                  selectInput("INPUT_select_sentitivity_analysis","Select sensitivity analysis:",
                              choices = list("Probabilistic sensitivity analysis" = 1, 
                                             "Deterministic sensitivity analysis" = 2), 
                              selected = 1),
                  checkboxGroupInput("INPUT_include_setting","Settings to include:",
                                     choices = CHOICES$setting,
                                     selected = CHOICES$setting),
                  radioButtons("INPUT_antiviral_type",
                               label = "Antiviral type:",
                               choices = CHOICES$antiviral_type,
                               selected = "nirmatrelvir_ritonavir"),
                  radioButtons("INPUT_perspective",
                               label = "Perspective:", 
                               choices = CHOICES$perspective,
                               selected = "healthcare perspective"),
                  
                  ### Probabilistic sensitivity analysis
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 1", 
                    
                    checkboxGroupInput("INPUT1_antiviral_cost", label = "Antiviral cost:",
                                 choices = CHOICES$antiviral_cost, 
                                 selected = "middle_income_cost"),
                    selectInput("INPUT1_include_outcomes","Outcome(s):",
                                choices = CHOICES$outcome,
                                multiple = TRUE,
                                selected = "QALYs"), 
                    selectInput("INPUT1_include_booster_vax_scenario","Booster strategies to include:",
                                choices = CHOICES$booster_vax_scenario,
                                multiple = TRUE,
                                selected = c( "all adults","high risk adults", "no booster")), 
                    selectInput("INPUT1_include_antiviral_target_group","Antiviral strategies to include:",
                                choices = CHOICES$antiviral_target_group,
                                selected = "adults with comorbidities",
                                multiple = TRUE), 
                    selectInput("INPUT1_discounting_rate",
                                "Discounting rate (%):",
                                choices = CHOICES$discounting,
                                selected = 3,
                                multiple = TRUE),

                    ### Special conditions for individual figures
                    conditionalPanel(
                      condition = "input.tabset == 'ICER table'", 
                      radioButtons("INPUT1_include_net","Include net columns?",
                                   choices = c("Yes",
                                               "No"))
                    ),
                    conditionalPanel(
                      condition = "input.tabset == 'Willingness to pay curve' ", 
                      actionButton("update_plot","Update plot"),
                    ), 

                  ),
                  
                  ### Deterministic sensitivity analysis
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 2", 
                      radioButtons("INPUT4_include_outcomes","Outcome:",
                                   choices = CHOICES$outcome),
                      checkboxGroupInput("INPUT4_parameters","Parameters to display:",
                                         choices = CHOICES$tornado_plot_parameters,
                                         selected = CHOICES$tornado_plot_parameters ), 
                      radioButtons("INPUT4_include_GDP","Include GDP as a line?",
                                   choices = c("Yes",
                                               "No")),
                  )
    ),
    
    
    mainPanel( width = 9,
               
               conditionalPanel(condition = "input.INPUT_select_sentitivity_analysis == 1",
                                tabsetPanel(
                                  id = "tabset",
                                  tabPanel("Incremental plane",
                                           plotOutput("OUTPUT_incremental_plane", height = "800px")),
                                  tabPanel("Willingness to pay curve",
                                           plotOutput("OUTPUT_WTP_curve", height = "800px")),
                                  tabPanel("ICER table", 
                                           dataTableOutput("OUTPUT_ICER_table"))
                                )),
               conditionalPanel(condition = "input.INPUT_select_sentitivity_analysis == 2",
                                tabsetPanel(tabPanel(
                                  "Tornado plot",
                                  plotOutput("OUTPUT_tornado_plot", height = "800px")
                                )))
    )
  )
  
)
################################################################################




##### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
  
  dataInput_ICERtable <- reactive({
    to_plot = CommandDeck_result %>%
      ungroup() %>%
      filter(setting %in% input$INPUT_include_setting &
               perspective %in% input$INPUT_perspective &
               discounting_rate %in% input$INPUT1_discounting_rate &
               antiviral_cost %in% input$INPUT1_antiviral_cost &
               booster_vax_scenario %in% input$INPUT1_include_booster_vax_scenario &
               antiviral_target_group %in% input$INPUT1_include_antiviral_target_group &
               antiviral_type %in% c("no antiviral",input$INPUT_antiviral_type) &
               (variable_type %in% c("ICER","outcome") | outcome == "netCost") &
               outcome %in% c("netCost",input$INPUT1_include_outcomes )) %>%
      select(-sd,-evaluation_level,-perspective,-discounting_rate)
    
    if (nrow(to_plot)>0){
      if (length(input$INPUT1_include_outcomes)>0){
        cost_column = to_plot %>%
          filter(variable_type == "cost") %>%
          mutate(variable_type = "netCost") %>%
          rename(netCost = mean) %>%
          ungroup() %>%
          select(-variable_type,-outcome,-LPI,-UPI)
        cost_column = crossing(cost_column,outcome = unique(to_plot$outcome[to_plot$outcome != "netCost"]))
        
        to_plot = to_plot %>%
          filter(variable_type != "cost") %>%
          left_join(cost_column, by = join_by( setting, antiviral_type, antiviral_target_group, outcome,booster_vax_scenario)) %>%
          pivot_wider(names_from = variable_type,
                      values_from = c(mean,LPI,UPI)) %>%
          select(-LPI_outcome,-UPI_outcome) %>%
          rename(count_outcome_averted = mean_outcome,
                 net_cost = netCost,
                 ICER = mean_ICER) %>%
          mutate(count_outcome_averted = round(count_outcome_averted,digits=1),
                 net_cost = round(net_cost),
                 ICER = round(ICER),
                 LPI_ICER = round(LPI_ICER),
                 UPI_ICER = round(UPI_ICER),
                 PI = paste(round(LPI_ICER),"to",round(UPI_ICER)),
          ) %>%
          ungroup() %>%
          select(setting,booster_vax_scenario,antiviral_target_group,outcome,count_outcome_averted,net_cost,ICER,LPI_ICER,UPI_ICER)
      }
      if (input$INPUT1_include_net == "No"){
        to_plot = to_plot %>%
          select(-net_cost,-count_outcome_averted)
      }
    }
    to_plot
  }) 
  
  dataInput_IncrementalPlane <- reactive({
    CommandDeck_result_long %>%
      filter(outcome == input$INPUT1_include_outcomes &
               setting %in% input$INPUT_include_setting &
               perspective == input$INPUT_perspective  &
               discounting_rate == input$INPUT1_discounting_rate &
               antiviral_cost == input$INPUT1_antiviral_cost &
               booster_vax_scenario %in% input$INPUT1_include_booster_vax_scenario &
               antiviral_target_group %in% input$INPUT1_include_antiviral_target_group & 
               antiviral_type %in% c("no antiviral",input$INPUT_antiviral_type))
  }) 
  
  dataInput_WTPcurve <- eventReactive( input$update_plot, {
      CEAC_dataframe %>%
        filter(outcome == input$INPUT1_include_outcomes &
                 setting %in% input$INPUT_include_setting &
                 perspective %in% input$INPUT_perspective &
                 discounting_rate %in% input$INPUT1_discounting_rate &
                 antiviral_cost %in% input$INPUT1_antiviral_cost &
                 booster_vax_scenario %in% input$INPUT1_include_booster_vax_scenario &
                 antiviral_type %in% c("no antiviral",input$INPUT_antiviral_type) &
                 antiviral_target_group %in% input$INPUT1_include_antiviral_target_group)

    
  }) 
  
  dataInput_TornadoPlot <- reactive({
    tornado_variable_of_interest = paste("cost_per_",
                                         gsub("QALYs","QALY",input$INPUT4_include_outcomes),
                                         "_averted",
                                         sep ="")
    tornado_result %>%
      filter(evaluation_level == "incremental") %>%
      filter(antiviral_type == input$INPUT_antiviral_type & 
               variable == tornado_variable_of_interest &
               setting %in% input$INPUT_include_setting &
               perspective %in% input$INPUT_perspective) 
    
  })
  
  
  output$OUTPUT_ICER_table <- renderDataTable({
    to_plot <- dataInput_ICERtable()
    if (nrow(to_plot)>0){
      to_plot
    }
  })
  
  output$OUTPUT_incremental_plane <- renderPlot({
    
    to_plot <- dataInput_IncrementalPlane()
    
    if (nrow(to_plot)>1){
      
      plot_list = list()
      for (this_setting in input$INPUT_include_setting){
        to_plot_setting = to_plot[to_plot$setting == this_setting,]
        
        plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
          geom_point(aes(x=netCost,y=count_outcomes,color=as.factor(booster_vax_scenario),shape = as.factor(antiviral_target_group))) +
          labs(shape="antiviral strategy",
               color = "booster strategy") +
          ylab("QALYs averted") +
          xlab("net cost (2022 USD)") +
          theme_bw() +
          theme(legend.position="bottom") +
          labs(title = this_setting) +
          guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) 
        
      }
      plot_list
      
      if(length(plot_list) == 1){row_num = 1; col_num = 1}
      if(length(plot_list) == 2){row_num = 1; col_num = 2}
      if(length(plot_list) > 2){row_num = 2; col_num = 2}
      
      #grid.arrange(grobs = plot_list,nrow = row_num, ncol = col_num)
      plot = ggarrange(plotlist = plot_list, ncol = col_num, nrow = row_num, common.legend = TRUE, legend = "bottom")
      print(plot)
    }
    
    
  }, res = 96)
  
  
  output$OUTPUT_WTP_curve <- renderPlot({
    
    to_plot <- dataInput_WTPcurve()
    
    if (nrow(to_plot)>1){
      
      xmax = to_plot %>% 
        group_by(setting,booster_vax_scenario,antiviral_target_group) %>%
        filter(round(probability,digits=2) >=0.99) %>%
        summarise(min = min(WTP), .groups = "keep") %>%
        ungroup()
      xmax = max(xmax$min)
      
      xmin = to_plot %>% 
        group_by(setting,booster_vax_scenario,antiviral_target_group) %>%
        filter(probability == min(probability,na.rm=TRUE)) %>%
        summarise(max = max(WTP), .groups = "keep") %>%
        ungroup()
      xmin = min(xmin$max)
      
      plot_list = list()
      for (this_setting in input$INPUT_include_setting){
        to_plot_setting = to_plot[to_plot$setting == this_setting,]
        
        if (input$INPUT3_parameter_to_vary == 4){
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=WTP,y=probability,color=as.factor(antiviral_target_group))) +
            labs(color="antiviral strategy") 
          n_options_selected = length(unique(to_plot_setting$antiviral_target_group))
          
        } else if (input$INPUT3_parameter_to_vary == 3){
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=WTP,y=probability,color=as.factor(booster_vax_scenario))) +
            labs(color="booster strategy")
          n_options_selected = length(unique(to_plot_setting$booster_vax_scenario))
          
        } else if (input$INPUT3_parameter_to_vary == 1){
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=WTP,y=probability,color=as.factor(discounting_rate))) +
            labs(color="discounting rate")
          n_options_selected = length(unique(to_plot_setting$discounting_rate))
          
        } else if (input$INPUT3_parameter_to_vary == 2){
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=WTP,y=probability,color=as.factor(antiviral_cost))) +
            labs(color="antiviral cost")
          n_options_selected = length(unique(to_plot_setting$antiviral_cost))
          
        } else{
          plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
            geom_point(aes(x=WTP,y=probability)) 
        }
        plot_list[[length(plot_list)]] = plot_list[[length(plot_list)]] +
          xlab("Willingness to pay ($/QALY)") +
          ylab("Probability cost-effective") +
          theme_bw() +
          theme(legend.position="bottom") +
          labs(title = this_setting) +
          #xlim(xmin,xmax) + 
          guides(colour = guide_legend(nrow = n_options_selected))
        
      }
      
      if(length(plot_list) == 1){row_num = 1; col_num = 1}
      if(length(plot_list) == 2){row_num = 1; col_num = 2}
      if(length(plot_list) > 2){row_num = 2; col_num = 2}
      
      #grid.arrange(grobs = plot_list,nrow = row_num, ncol = col_num)
      plot = ggarrange(plotlist = plot_list, ncol = col_num, nrow = row_num, common.legend = TRUE, legend = "bottom")
      print(plot)
    }
    
    
    
  }, res = 96)
  
  
  
  output$OUTPUT_tornado_plot <- renderPlot({
    to_plot <- dataInput_TornadoPlot()
    
    if (nrow(to_plot)>1){
      
      plot_list = list()
      for (this_setting in input$INPUT_include_setting){
        to_plot_this_setting = to_plot %>%
          ungroup() %>%
          filter(setting == this_setting &   
                   label %in% input$INPUT4_parameters)
        
        if (this_setting == "FJI"){this_setting_GDP = 5316.7}
        if (this_setting == "IDN"){this_setting_GDP = 4788.0}
        if (this_setting == "PNG"){this_setting_GDP = 3020.3}
        if (this_setting == "TLS"){this_setting_GDP = 2358.4}
        
        base.value <- to_plot_this_setting$mean[to_plot_this_setting$direction == "lower" & 
                                                  to_plot_this_setting$label == "Long COVID (off/on)" ] # final value was baseline estimates
        
        # width of columns in plot (value between 0 and 1)
        width <- 0.95
        order_parameters <- to_plot_this_setting %>%
          select(label,mean,direction) %>%
          group_by(label) %>%
          summarise(LB = min(mean),
                    UB = max(mean)) %>%
          mutate(UL_Difference = UB - LB) %>% 
          arrange(UL_Difference) %>%
          mutate(label=factor(x=label, levels=label)) %>%
          select(label) %>% 
          unlist() %>% 
          levels()
        
        # get data frame in shape for ggplot and geom_rect
        df_2 <- to_plot_this_setting %>%
          select(label,mean,direction) %>% 
          rename(value = mean) %>%
          ungroup() %>%
          # create the columns for geom_rect
          mutate(label=factor(label, levels=order_parameters),
                 ymin=pmin(value, base.value),
                 ymax=pmax(value, base.value),
                 xmin=as.numeric(label)-width/2,
                 xmax=as.numeric(label)+width/2)
        
        # create plot
        require(ggtext)
        options(scipen=999) #turn off scientific notation
        
        plot_list[[length(plot_list)+1]] = ggplot() + 
          geom_rect(data = df_2,
                    aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=paste(direction,"estimate"))) + 
          geom_hline(yintercept = base.value) +
          theme_bw() + 
          #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
          theme(axis.title.y=element_blank(), legend.position = 'bottom',
                legend.title = element_blank())  +
          ylab(paste("Cost per",gsub("QALYs","QALY",input$INPUT4_include_outcomes),"averted (2022 USD)")) +
          scale_x_continuous(breaks = c(1:length(order_parameters)), 
                             labels = order_parameters) +
          coord_flip() +
          labs(title = this_setting)
        
        if (input$INPUT4_include_GDP == "Yes"){
          plot_list[[length(plot_list)]] = plot_list[[length(plot_list)]] + 
            geom_hline(mapping = NULL, yintercept = this_setting_GDP, linetype='dashed') +
            annotate("text", x = 4, y = this_setting_GDP*0.9, label = "GDP per capita",
                     angle = 90)
        }
      }
      plot_list
      
      if(length(plot_list) == 1){row_num = 1; col_num = 1}
      if(length(plot_list) == 2){row_num = 1; col_num = 2}
      if(length(plot_list) > 2){row_num = 2; col_num = 2}
      
      #grid.arrange(grobs = plot_list,nrow = row_num, ncol = col_num)
      plot = ggarrange(plotlist = plot_list, ncol = col_num, nrow = row_num, common.legend = TRUE, legend = "bottom")
      print(plot)
    }
    
  }, res = 96)
  
  
}

shinyApp(ui, server)