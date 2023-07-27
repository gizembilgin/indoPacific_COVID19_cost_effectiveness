require(beepr); require(ggplot2); require(gridExtra); require(ggpubr); require(tidyverse); require(shiny);require(shinyWidgets)
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
  antiviral_cost_scenario = list("low generic cost" = "low_generic_cost",
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
  #textOutput("test"),
  
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
                  radioGroupButtons("INPUT_antiviral_type",
                               label = "Antiviral type:",
                               choices = CHOICES$antiviral_type,
                               selected = "nirmatrelvir_ritonavir"),
                  radioGroupButtons("INPUT_perspective",
                               label = "Perspective:", 
                               choices = CHOICES$perspective,
                               selected = "healthcare perspective"),
                  radioGroupButtons("INPUT_include_outcomes","Outcome:",
                                    choices = CHOICES$outcome,
                                    selected = "QALYs"), 
                  
                  ### Probabilistic sensitivity analysis
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 1", 

                    
                    checkboxGroupInput("INPUT_antiviral_cost_scenario", label = "Antiviral cost:",
                                 choices = CHOICES$antiviral_cost_scenario,
                                 selected = "middle_income_cost"),
                    selectInput("INPUT_include_booster_vax_scenario","Booster strategies to include:",
                                choices = CHOICES$booster_vax_scenario,
                                multiple = TRUE,
                                selected = c( "all adults","high risk adults", "no booster")), 
                    selectInput("INPUT_include_antiviral_target_group","Antiviral strategies to include:",
                                choices = CHOICES$antiviral_target_group,
                                selected = "adults with comorbidities",
                                multiple = TRUE), 
                    selectInput("INPUT_discounting_rate",
                                "Discounting rate (%):",
                                choices = CHOICES$discounting,
                                selected = 3,
                                multiple = TRUE),

                    # special conditions for individual figures
                    conditionalPanel(
                      condition = "input.tabset == 'ICER table'", 
                      radioGroupButtons("INPUT_include_net","Include net columns?",
                                   choices = c("Yes",
                                               "No"))
                    ),
                    conditionalPanel(
                      condition = "input.tabset == 'Willingness to pay curve' ", 
                      actionButton(inputId = "update_plot",
                                   label = "Update plot"),
                    ), 
                    conditionalPanel(
                      condition = "input.tabset == 'Willingness to pay curve' | input.tabset == 'Incremental plane'", 
                      uiOutput("switch_shape_and_colour")
                    ), 

                  ),
                  
                  ### Deterministic sensitivity analysis
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 2", 
                      checkboxGroupInput("INPUT_parameters","Parameters to display:",
                                         choices = CHOICES$tornado_plot_parameters,
                                         selected = CHOICES$tornado_plot_parameters ), 
                     radioGroupButtons("INPUT_include_GDP","Include GDP as a line?",
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
  
  # output$test <- renderText({
  #   str(input$INPUT_switch_shape_and_colour)
  #   }) 
  
  ### dataInputs ###############################################################
  dataInput_IncrementalPlane <- reactive({
    CommandDeck_result_long %>%
      filter(outcome %in% input$INPUT_include_outcomes &
               setting %in% input$INPUT_include_setting &
               perspective %in% input$INPUT_perspective  &
               discounting_rate %in% input$INPUT_discounting_rate &
               antiviral_cost_scenario %in% input$INPUT_antiviral_cost_scenario &
               booster_vax_scenario %in% input$INPUT_include_booster_vax_scenario &
               antiviral_target_group %in% input$INPUT_include_antiviral_target_group & 
               antiviral_type %in% c("no antiviral",input$INPUT_antiviral_type))
  }) 
  
  dataInput_WTPcurve <- eventReactive(input$update_plot,{
    CEAC_dataframe %>%
      filter(outcome %in% input$INPUT_include_outcomes &
               setting %in% input$INPUT_include_setting &
               perspective %in% input$INPUT_perspective &
               discounting_rate %in% input$INPUT_discounting_rate &
               antiviral_cost_scenario %in% input$INPUT_antiviral_cost_scenario &
               booster_vax_scenario %in% input$INPUT_include_booster_vax_scenario &
               antiviral_type %in% c("no antiviral",input$INPUT_antiviral_type) &
               antiviral_target_group %in% input$INPUT_include_antiviral_target_group)
  }) 
  
  output$OUTPUT_ICER_table <- renderDataTable({
    this_ICER_table = ICER_table %>%
      filter(setting %in% input$INPUT_include_setting &
               perspective %in% input$INPUT_perspective &
               discounting_rate %in% input$INPUT_discounting_rate &
               antiviral_cost_scenario %in% input$INPUT_antiviral_cost_scenario &
               booster_vax_scenario %in% input$INPUT_include_booster_vax_scenario &
               antiviral_target_group %in% input$INPUT_include_antiviral_target_group &
               antiviral_type %in% c("no antiviral",input$INPUT_antiviral_type) &
               outcome %in% c("netCost",input$INPUT_include_outcomes )) %>%
      select(setting,booster_vax_scenario,antiviral_cost_scenario,antiviral_target_group,count_outcome_averted,net_cost,ICER,LPI_ICER,UPI_ICER) 
    
    if(nrow(this_ICER_table)>0 & input$INPUT_include_net == "No"){
      this_ICER_table = this_ICER_table %>% select(-net_cost,-count_outcome_averted)
    }
    colnames(this_ICER_table)[colnames(this_ICER_table) == "count_outcome_averted"]<- paste(input$INPUT_include_outcomes,"averted")
    colnames(this_ICER_table) <- gsub("_"," ",colnames(this_ICER_table))
    
    this_ICER_table
  }) 
  
  dataInput_TornadoPlot <- reactive({
    tornado_variable_of_interest = paste("cost_per_",
                                         gsub("QALYs","QALY",input$INPUT_include_outcomes),
                                         "_averted",
                                         sep ="")
    tornado_result %>%
      filter(evaluation_level == "incremental") %>%
      filter(antiviral_type %in%  input$INPUT_antiviral_type & 
               variable %in%  tornado_variable_of_interest &
               setting %in% input$INPUT_include_setting &
               perspective %in% input$INPUT_perspective) 
    
  })
  ##############################################################################
  
  
  
  ###<intermission>
  plot_dimensions <- reactive({
    plot_dimension_vector = c()
    if (length(input$INPUT_antiviral_cost_scenario)>1)       {plot_dimension_vector = c(plot_dimension_vector,"antiviral_cost_scenario")}
    if (length(input$INPUT_discounting_rate)>1)              {plot_dimension_vector = c(plot_dimension_vector,"discounting_rate")}
    if (length(input$INPUT_include_antiviral_target_group)>1){plot_dimension_vector = c(plot_dimension_vector,"antiviral_target_group")}
    if (length(input$INPUT_include_booster_vax_scenario)>1)  {plot_dimension_vector = c(plot_dimension_vector,"booster_vax_scenario")}
    
    plot_dimension_vector

  }) 

  apply_plot_dimensions <- function(data_set,aes_x,aes_y,plot_dimensions){
    
     if (length(plot_dimensions)==2 & is.null(input$INPUT_switch_shape_and_colour) == FALSE){
        if (input$INPUT_switch_shape_and_colour){
          plot_dimensions <- isolate(rev(plot_dimensions))
        }
     }
    
    if (length(plot_dimensions) == 0){
      this_plot = ggplot(data_set) +
        geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]]))
    } else if (length(plot_dimensions) == 1){
      this_plot = ggplot(data_set) +
        geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]],color=as.factor(.data[[plot_dimensions[1]]]))) +
        labs(color = paste(gsub("_"," ", plot_dimensions[1])))
    } else if (length(plot_dimensions) == 2){
      this_plot = ggplot(data_set) +
        geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]],color=as.factor(.data[[plot_dimensions[1]]]),shape = as.factor(.data[[plot_dimensions[2]]]))) +
        labs(color = paste(gsub("_"," ", plot_dimensions[1])),
             shape = paste(gsub("_"," ", plot_dimensions[2])))
    }
    


    return(this_plot)
  }
  
  output$switch_shape_and_colour <- renderUI({
    if(length(plot_dimensions()) == 2){
      materialSwitch(inputId = "INPUT_switch_shape_and_colour",
                   label = "Switch shape & colour",
                   value = FALSE)
    }
  })

  
  
  ###(2/4) IncrementalPlane

  output$OUTPUT_incremental_plane <- renderPlot({
    
    validate(need(length(plot_dimensions())<=2, 'You have selected too many plot dimensions.\nPlease select multiple values for a maximum of two of the following variables: antiviral cost, booster strategies, antiviral strategies, discounting rate'))
    #req(length(plot_dimensions())<=2)
    
    to_plot <- dataInput_IncrementalPlane()
    
    if (nrow(to_plot)>1){
      
      plot_list = list()
      for (this_setting in input$INPUT_include_setting){
      
        plot_list[[length(plot_list)+ 1]] = apply_plot_dimensions(data_set = to_plot[to_plot$setting == this_setting,],
                                                               aes_x="netCost",
                                                               aes_y="count_outcomes",
                                                               plot_dimensions = plot_dimensions())  +
          ylab(paste(input$INPUT_include_outcomes,"averted")) +
          xlab("net cost (2022 USD)") +
          theme_bw() +
          theme(legend.position="bottom") +
          labs(title = this_setting) +
          guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) 
        
      }
      
      #arrange
      if(length(plot_list) == 1){row_num = 1; col_num = 1}
      if(length(plot_list) == 2){row_num = 1; col_num = 2}
      if(length(plot_list) > 2) {row_num = 2; col_num = 2}
      plot = ggarrange(plotlist = plot_list, ncol = col_num, nrow = row_num, common.legend = TRUE, legend = "bottom")
      print(plot)
    }
    
    
  }, res = 96)

  
  
  
  ###(3/4) WTP curve
  output$OUTPUT_WTP_curve <- renderPlot({
    validate(need(length(plot_dimensions())<=2, 'You have selected too many plot dimensions.\nPlease select multiple values for a maximum of two of the following variables: antiviral cost, booster strategies, antiviral strategies, discounting rate'))
    
    to_plot <- dataInput_WTPcurve()
    
    if (nrow(to_plot) > 1) {
      # xmax = to_plot %>%
      #   group_by(setting, booster_vax_scenario, antiviral_target_group) %>%
      #   filter(round(probability, digits = 2) >= 0.99) %>%
      #   summarise(min = min(WTP), .groups = "keep") %>%
      #   ungroup()
      # xmax = max(xmax$min)
      # 
      # xmin = to_plot %>%
      #   group_by(setting, booster_vax_scenario, antiviral_target_group) %>%
      #   filter(probability == min(probability, na.rm = TRUE)) %>%
      #   summarise(max = max(WTP), .groups = "keep") %>%
      #   ungroup()
      # xmin = min(xmin$max)
      
      plot_list = list()
      for (this_setting in input$INPUT_include_setting) {
        
        plot_list[[length(plot_list)+ 1]] = apply_plot_dimensions(data_set = to_plot[to_plot$setting == this_setting,],
                                                               aes_x="WTP",
                                                               aes_y="probability",
                                                               plot_dimensions = plot_dimensions())  +
          xlab(paste("Willingness to pay ($/",input$INPUT_include_outcomes,")",sep="")) +
          ylab("Probability cost-effective") +
          theme_bw() +
          theme(legend.position = "bottom") +
          labs(title = this_setting) +
          #xlim(xmin,xmax) +
          guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) 
        

      }
      
      #arrange
      if(length(plot_list) == 1){row_num = 1; col_num = 1}
      if(length(plot_list) == 2){row_num = 1; col_num = 2}
      if(length(plot_list) > 2) {row_num = 2; col_num = 2}
      plot = ggarrange(plotlist = plot_list, ncol = col_num, nrow = row_num, common.legend = TRUE, legend = "bottom")
      print(plot)
    }
    
  }, res = 96)
  
  
  ###(4/4) Tornado plot
  output$OUTPUT_tornado_plot <- renderPlot({
    to_plot <- dataInput_TornadoPlot()
    
    if (nrow(to_plot)>1){
      
      plot_list = list()
      for (this_setting in input$INPUT_include_setting){
        to_plot_this_setting = to_plot %>%
          ungroup() %>%
          filter(setting == this_setting &   
                   label %in% input$INPUT_parameters)
        
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
          ylab(paste("Cost per",gsub("QALYs","QALY",input$INPUT_include_outcomes),"averted (2022 USD)")) +
          scale_x_continuous(breaks = c(1:length(order_parameters)), 
                             labels = order_parameters) +
          coord_flip() +
          labs(title = this_setting)
        
        if (input$INPUT_include_GDP == "Yes"){
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
