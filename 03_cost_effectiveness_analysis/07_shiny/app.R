require(beepr); require(ggplot2); require(gridExtra); require(ggpubr); require(wesanderson);require(ggtext) 
require(tidyverse); require(shiny); require(shinyWidgets); require(reactlog); require(waiter)
#library(rsconnect); rsconnect::deployApp(paste0(getwd(),"/07_shiny/"), appName = "indoPacific_COVID19_costEffectivenessAnalysis"); beep()
options(scipen = 1000) #turn off scientific notation
#rm(list = ls())


##### CONFIGURE CHOICES ########################################################
CHOICES = list(
  antiviral_cost_scenario = list("low generic reference price ($25 USD per schedule)" = "low generic reference price ($25 USD per schedule)",
                                "middle-income reference price ($250 USD per schedule)" = "middle-income reference price ($250 USD per schedule)" , 
                                "high-income reference price ($530 USD per schedule)" = "high-income reference price ($530 USD per schedule)"),
  antiviral_target_group = c("all adults", 
                             "adults with comorbidities", 
                             "unvaccinated adults",
                             "no antiviral"),
  antiviral_type = list("molunipiravir" = "molunipiravir",
                        "nirmatrelvir_ritonavir" = "nirmatrelvir_ritonavir"),
  booster_vax_scenario = c("high risk adults"
                           , "all adults"
                           ,"high-risk adults (catch-up campaign)"
                           , "all adults (catch-up campaign)"  
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
    "Injection Equipment price ($0.025-$0.050)",
    "Productivity loss due to illness (±50%)",
    "Productivity loss due to death (±50%)"
  )
)
################################################################################




##### USER INTERFACE DEFINITION ################################################
ui <- fluidPage(
  
  titlePanel("Interactive cost-effectiveness analysis of COVID-19 oral antivirals and booster doses in the Indo-Pacific"),
  h6(textOutput("shiny_description")),
  textOutput("test"),
  
  sidebarLayout(

    ### Widgets ################################################################  
    sidebarPanel( width = 3,
                  ###Available in all plots
                  selectInput(inputId = "INPUT_select_sentitivity_analysis",
                              label = "Select sensitivity analysis:",
                              choices = list("Probabilistic sensitivity analysis" = "probab", 
                                             "Deterministic sensitivity analysis" = "det"), 
                              selected = "probab"),
                  checkboxGroupInput(inputId = "INPUT_include_setting",
                                     label = "Settings to include:",
                                     choices = CHOICES$setting,
                                     selected = CHOICES$setting),
                  checkboxGroupButtons(inputId = "INPUT_perspective",
                                    label = "Perspective(s):", 
                                    choices = CHOICES$perspective,
                                    selected = "healthcare perspective"),
                  radioGroupButtons(inputId = "INPUT_antiviral_type",
                               label = "Antiviral type:",
                               choices = CHOICES$antiviral_type,
                               selected = "nirmatrelvir_ritonavir"),
                  radioGroupButtons(inputId = "INPUT_include_outcomes",
                                    label = "Outcome:",
                                    choices = CHOICES$outcome,
                                    selected = "QALYs"), 
                  uiOutput("booster_strategy"),
                  uiOutput("antiviral_strategy"),
                                    
                  ### Probabilistic sensitivity analysis
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 'probab'", 

                    checkboxGroupInput("INPUT_antiviral_cost_scenario", label = "Antiviral cost:",
                                 choices = CHOICES$antiviral_cost_scenario,
                                 selected = "middle-income reference price ($250 USD per schedule)"),

                    selectInput("INPUT_discounting_rate",
                                "Discounting rate (%):",
                                choices = CHOICES$discounting,
                                selected = 3,
                                multiple = TRUE),

                    # special conditions for individual figures
                    conditionalPanel(
                      condition = "input.tabset == 'ICER table'", 
                      radioGroupButtons("INPUT_include_net","Include net columns?",
                                   choices = c("Yes","No"))
                    ),
                    conditionalPanel(
                      condition = "input.tabset == 'Willingness to pay curve' | input.tabset == 'Incremental plane'", 
                      uiOutput("switch_shape_and_colour"),
                    ), 
                    conditionalPanel(
                      condition = "input.tabset == 'Willingness to pay curve' ", 
                      checkboxInput("INPUT_fix_xaxis","Fix x-axis between settings",value = FALSE),
                    ), 
                  ),
                  
                  ### Deterministic sensitivity analysis
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 'det'", 
                      checkboxGroupInput("INPUT_parameters","Parameters to display:",
                                         choices = CHOICES$tornado_plot_parameters,
                                         selected = CHOICES$tornado_plot_parameters )
                  ),
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 'det' | input.tabset == 'Willingness to pay curve' ", 
                    uiOutput("GDP_line_toggle")
                  ),
                  conditionalPanel(
                    condition = "input.INPUT_select_sentitivity_analysis == 'probab' & (input.tabset == 'Willingness to pay curve' | input.tabset == 'Incremental plane')", 
                    textOutput("warning_on_unvax_Fijian"),
                    actionButton(inputId = "update_plot",
                                 label = "Update plot")
                  ), 
                  
    ),
    
    ### Outputs ################################################################
    mainPanel( width = 9,
               
               waiter::useWaiter(),
               
               fluidRow(
                 conditionalPanel(condition = "input.tabset != 'ICER table' || input.INPUT_select_sentitivity_analysis == 'det'",
                                  column(1,numericInputIcon(inputId = "plot_height", label = "Plot height:", min = 100, max = 2000, value = 800, step =100)),
                                  column(1,numericInputIcon(inputId = "plot_width", label = "Plot width:", min = 100, max = 2000, value = 1200, step =100))
                                  ),
                 column(10, downloadButton("download"), style = "margin-top: 25px;")
                 ),
               
               textOutput("warning_on_hosp_molnupiravir"),
               
               conditionalPanel(condition = "input.INPUT_select_sentitivity_analysis == 'probab'",
                                tabsetPanel(
                                  id = "tabset",
                                  tabPanel("Incremental plane",
                                           plotOutput("OUTPUT_incremental_plane", height = "800px")),
                                  tabPanel("Willingness to pay curve",
                                           plotOutput("OUTPUT_WTP_curve", height = "800px")),
                                  tabPanel("ICER table", 
                                           dataTableOutput("OUTPUT_ICER_table"))
                                )),
               conditionalPanel(condition = "input.INPUT_select_sentitivity_analysis == 'det'",
                                tabsetPanel(tabPanel(
                                  "Tornado plot",
                                  plotOutput("OUTPUT_tornado_plot", height = "800px")
                                ))), 
    )
  )
)
################################################################################




##### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
  
  is_local <- Sys.getenv("SHINY_PORT") == "" #boolean of whether this shiny is being hosted locally or on posit.co
  
  # output$test <- renderText({
  #   paste0("is_local = ",is_local)
  #   })
  
  output$shiny_description <- renderText({
    string_to_print = c("This R Shiny accompanies the working paper <doi link once submitted>.")
      if (is_local == FALSE){
        string_to_print = c(string_to_print,
                            "This hosted version of the R Shiny contains a subset of model simulations due to restrictions on virtual RAM. We expect that this subset of simulations is representative of the paper's results. For access to complete model simulations please download the full R Shiny available from <GitHub link>")
      } else{
        string_to_print = c(string_to_print,"Please note that this application may take a minute or two to load the underlying simulations.")
      }
    string_to_print
  })
  
  
  ### load latest results ######################################################
  # load latest probabilistic results
  list_poss_Rdata = list.files(
    path = "x_results/",
    pattern = "ICER_table*"
  )
  if (length(list_poss_Rdata) > 0) {
    list_poss_Rdata_details = double()
    for (j in 1:length(list_poss_Rdata)) {
      list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                      file.info(paste0("x_results/", list_poss_Rdata[[j]]))$mtime)
    }
    latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    load(file = paste0("x_results/",latest_file)) #loading ICER table
    
    time_of_result <- gsub("ICER_table","",latest_file)
    
    if (is_local == TRUE){
      #load CommandDeck_result_long
      load(file = paste0("x_results/CommandDeck_result_long_1_",time_of_result)) 
      load(file = paste0("x_results/CommandDeck_result_long_2_",time_of_result)) 
      CommandDeck_result_long <- rbind(CommandDeck_result_long_part1,CommandDeck_result_long_part2); rm(CommandDeck_result_long_part1,CommandDeck_result_long_part2)
      
      #load CEAC_dataframe
      load(file = paste0("x_results/CEAC_dataframe_",time_of_result))
    } else{
      load(file = paste0("x_results/CEAC_dataframe_reduced_",time_of_result))
      load(file = paste0("x_results/CommandDeck_result_long_reduced_",time_of_result))
    }

  } else{
    stop("Can't find underlying simulation to load!")
  }

  # load latest deterministic results
  load(file = paste0("x_results/tornado_result.Rdata"))
  ################################################################################
  
  
  
  ### functions and multi-use reactive ########################################
  # function which subsets data to the widgets displayed for probabilistic sensitivity analysis
  subset_data_to_selected <- function(df){
    df %>%
      filter(
        setting %in% input$INPUT_include_setting &
          perspective %in% input$INPUT_perspective  &
          discounting_rate %in% input$INPUT_discounting_rate &
          antiviral_cost_scenario %in% input$INPUT_antiviral_cost_scenario &
          booster_vax_scenario %in% input$INPUT_include_booster_vax_scenario &
          antiviral_target_group %in% input$INPUT_include_antiviral_target_group &
          antiviral_type %in% c("no antiviral", input$INPUT_antiviral_type)
      )
  }
  
  # function to create waiter spinner while waiting for plot to load
  call_waiter <- function(this_output){
    waiter::Waiter$new(
      id = this_output,
      html = spin_3(), 
      color = transparent(.5)
    )$show()
  }
  
  # reactive containing the INPUTs with multiple values selected
  count_plot_dimensions <- reactive({
    plot_dimension_vector = c()
    
    if (length(input$INPUT_antiviral_cost_scenario)>1)       {plot_dimension_vector = c(plot_dimension_vector,"antiviral_cost_scenario")}
    if (length(input$INPUT_discounting_rate)>1)              {plot_dimension_vector = c(plot_dimension_vector,"discounting_rate")}
    if (length(input$INPUT_include_antiviral_target_group)>1){plot_dimension_vector = c(plot_dimension_vector,"antiviral_target_group")}
    if (length(input$INPUT_include_booster_vax_scenario)>1)  {plot_dimension_vector = c(plot_dimension_vector,"booster_vax_scenario")}
    
    plot_dimension_vector
  }) 
  
  # function to configure aesthetic of ggplot() by count_plot_dimensions() reactive
  apply_plot_dimensions <- function(df,aes_x,aes_y,count_plot_dimensions){
    
    if (length(input$INPUT_perspective)>1){ this_point_size = 1  
    } else{ this_point_size = 1.5}
    
    if (length(count_plot_dimensions)==2 & is.null(input$INPUT_switch_shape_and_colour) == FALSE){
      if (input$INPUT_switch_shape_and_colour){
        count_plot_dimensions <- isolate(rev(count_plot_dimensions))
      }
    }
    
    if (length(count_plot_dimensions) == 0){
      this_plot <- ggplot(df) +
        geom_point(aes(x = .data[[aes_x]],
                       y=.data[[aes_y]]),
                   size = this_point_size)
    } else if (length(count_plot_dimensions) == 1){
      this_plot <- ggplot(df) +
        geom_point(aes(x = .data[[aes_x]],
                       y=.data[[aes_y]],
                       color=as.factor(.data[[count_plot_dimensions[1]]])),
                   size = this_point_size) +
        labs(color = paste(gsub("_"," ", count_plot_dimensions[1])))
      
    } else if (length(count_plot_dimensions) == 2){
      if (aes_x == "WTP"){
        this_plot <- ggplot(df) +
          geom_point(aes(x = .data[[aes_x]],
                         y=.data[[aes_y]],
                         color=as.factor(.data[[count_plot_dimensions[1]]]))) +
          labs(color = paste(gsub("_"," ", count_plot_dimensions[1]))) +
          facet_grid(.data[[count_plot_dimensions[2]]]~.)
      } else{
        this_plot <- ggplot(df) +
          geom_point(aes(x = .data[[aes_x]],
                         y=.data[[aes_y]],
                         color=as.factor(.data[[count_plot_dimensions[1]]]),
                         shape = as.factor(.data[[count_plot_dimensions[2]]]))) +
          labs(color = paste(gsub("_"," ", count_plot_dimensions[1])),
               shape = paste(gsub("_"," ", count_plot_dimensions[2])))
      }
    }
    
    if (length(input$INPUT_perspective)>1){
      this_plot <- this_plot + 
        facet_grid(perspective ~.) 
    }
    if (length(count_plot_dimensions) != 0){
      if (count_plot_dimensions[1] == "booster_vax_scenario"){
        this_plot <- this_plot +
          scale_color_manual(values = #wesanderson::wes_palette( name="Zissou1"))
                               c("high risk adults" = "#e1a500",
                                 "all adults" = "#3b94b2" ,
                                 "high-risk adults (catch-up campaign)" ="#ebd829" ,
                                 "all adults (catch-up campaign)" = "#76c3c4" ,
                                 "no booster" = "#d6607c")
          )
      } else if (count_plot_dimensions[1] == "antiviral_cost_scenario"){
        this_plot <- this_plot +
          scale_color_manual(values = wesanderson::wes_palette( name="FantasticFox1"))
      } else if (count_plot_dimensions[1] == "antiviral_target_group"){
        this_plot <- this_plot +
          scale_colour_manual(values = 
                                c("all adults" = "#619CFF", 
                                  "adults with comorbidities" = "#00BA38", 
                                  "unvaccinated adults" = "#F8766D" ,
                                  "no antiviral" = "#999999"))
      }
    }
    return(this_plot)
  }
  
  # widget to switch variables controlling shape & colour
  output$switch_shape_and_colour <- renderUI({
    if(length(count_plot_dimensions()) == 2){
      materialSwitch(inputId = "INPUT_switch_shape_and_colour",
                     label = "Switch shape/facet & colour",
                     value = FALSE)
    }
  })
  
  #widget for booster and oral antiviral eligibility strategies
  output$booster_strategy <- renderUI({
    if(input$INPUT_select_sentitivity_analysis == "probab"){
      checkboxGroupInput("INPUT_include_booster_vax_scenario","Booster eligibility (select at least one):",
                         choices = CHOICES$booster_vax_scenario,
                         selected = c("all adults","high risk adults", "no booster")) 
    } else{
      radioButtons("INPUT_include_booster_vax_scenario","Booster eligibility:",
                         choices = CHOICES$booster_vax_scenario,
                         selected = c("all adults"))
    }
  })
  output$antiviral_strategy <- renderUI({
    if(input$INPUT_select_sentitivity_analysis == "probab"){
       checkboxGroupInput("INPUT_include_antiviral_target_group",
                          "Antiviral eligibility (select at least one):",
                          choices = CHOICES$antiviral_target_group,
                          selected = "adults with comorbidities") 
    } else{
        radioButtons("INPUT_include_antiviral_target_group",
                     "Antiviral eligibility:",
                     choices = CHOICES$antiviral_target_group,
                     selected = "adults with comorbidities") 
    }
  })
  
  #widget for GDP line
  output$GDP_line_toggle <- renderUI({
    if(input$INPUT_select_sentitivity_analysis == "probab"){
      checkboxInput("INPUT_include_GDP","Include GDP per capita as a dashed line",value = TRUE)
    } else{
      radioGroupButtons("INPUT_include_GDP","Include GDP per capita as a line?",
                        choices = c("Yes","No"))
    }
  })
  
  # function to consolidate plot_list into one figure
  consolidate_plot_list <- function(plot_list){
    if(length(plot_list) == 1){row_num = 1; col_num = 1;this_legend = get_legend(plot_list[[1]])}
    if(length(plot_list) == 2){row_num = 1; col_num = 2; this_legend = get_legend(plot_list[[2]])}
    if(length(plot_list) > 2) {row_num = 2; col_num = 2; this_legend = get_legend(plot_list[[2]])}
    plot <- ggarrange(plotlist = plot_list,
                      ncol = col_num, 
                      nrow = row_num, 
                      common.legend = TRUE, 
                      legend = "bottom",
                      legend.grob = this_legend)
  }
  
  # text to display when too many dimensions have been selected
  too_many_dimensions_text = "You have selected too many plot dimensions.\nPlease select multiple values for a maximum of two of the following variables: antiviral cost, booster strategies, antiviral strategies, discounting rate"
  
  # text to display when unvaccinated FJI points obscured by overlapping no antiviral points
  output$warning_on_unvax_Fijian <- renderText({
    if("Fiji" %in% input$INPUT_include_setting & "unvaccinated adults" %in% input$INPUT_include_antiviral_target_group){
      validate("\nNote: Due to Fiji's high vaccine coverage rates, 'no antiviral' and 'unvaccinated adults' antiviral target simulations overlap")
    }
  })
  
  #text to display when hospitalisation for molnupiravir
  output$warning_on_hosp_molnupiravir <- renderText({
    if(input$INPUT_antiviral_type == "molunipiravir" && input$INPUT_include_outcomes == "hosp"){
      validate("\nNote: This figure does not exist since cohort studies have not observed a statistically significiant effect of molnupiravir on the risk of hospitalisation \n  ")
    }
  })
  ##############################################################################
  
  
  
  ### dataInputs ###############################################################
  dataInput_IncrementalPlane <- reactive({
    subset_data_to_selected(CommandDeck_result_long)%>%
      filter(outcome %in% input$INPUT_include_outcomes)
  }) 
  
  dataInput_WTPcurve <- reactive({
    subset_data_to_selected(CEAC_dataframe)%>%
      filter(outcome %in% input$INPUT_include_outcomes)
  })

  dataInput_ICER_table <- reactive({
    this_ICER_table <- subset_data_to_selected(ICER_table) %>%
      filter(outcome %in% c("netCost",input$INPUT_include_outcomes)) %>%
      rename(incremental_cost = net_cost) %>%
      select(setting, booster_vax_scenario, antiviral_cost_scenario, antiviral_target_group, 
             count_outcome_averted, incremental_cost, ICER, LPI_ICER, UPI_ICER) 
    
    if(nrow(this_ICER_table)>0 & input$INPUT_include_net == "No"){
      this_ICER_table <- this_ICER_table %>% select(-incremental_cost,-count_outcome_averted)
    }
    colnames(this_ICER_table)[colnames(this_ICER_table) == "count_outcome_averted"]<- paste(input$INPUT_include_outcomes,"averted")
    colnames(this_ICER_table) <- gsub("_"," ",colnames(this_ICER_table))
    colnames(this_ICER_table) <- gsub("booster vax scenario","booster eligibility",colnames(this_ICER_table))
    
    this_ICER_table
  }) 
  output$OUTPUT_ICER_table <- renderDataTable({dataInput_ICER_table()})
  
  dataInput_TornadoPlot <- reactive({
    tornado_variable_of_interest <- paste0("cost_per_",
                                         gsub("QALYs","QALY",input$INPUT_include_outcomes),
                                         "_averted")
    tornado_result %>%
      filter(evaluation_level == "incremental" &
               antiviral_type %in% c("no antiviral", input$INPUT_antiviral_type) & 
               variable %in%  tornado_variable_of_interest &
               setting %in% input$INPUT_include_setting &
               perspective %in% input$INPUT_perspective  &
               booster_vax_scenario %in% input$INPUT_include_booster_vax_scenario &
               antiviral_target_group %in% input$INPUT_include_antiviral_target_group) 
  })
  ##############################################################################
  
  
  
  ### defining plot outputs  ####################################################
  #(1/3) Incremental Plane
  PLOT_incremental_plane <- eventReactive({input$update_plot|is.null(input$INPUT_switch_shape_and_colour) == FALSE},{
    
    call_waiter("OUTPUT_incremental_plane")
    
    validate(need(length(count_plot_dimensions())<=2, too_many_dimensions_text))
    to_plot <- dataInput_IncrementalPlane()
    
    if (nrow(to_plot)>1){
      plot_list = list()
      
      for (this_setting in input$INPUT_include_setting){
        workshop <- to_plot[to_plot$setting == this_setting,]
        plot_list[[length(plot_list)+ 1]] <- apply_plot_dimensions(df = workshop,
                                                                  aes_x="netCost",
                                                                  aes_y="count_outcomes",
                                                                  count_plot_dimensions = count_plot_dimensions())  +
          ylab(paste(input$INPUT_include_outcomes,"averted")) +
          xlab("incremental cost (2022 USD)") +
          xlim(min(min(workshop$netCost),0), 
               max(max(workshop$netCost),0)) + 
          ylim(min(min(workshop$count_outcomes),0), 
               max(max(workshop$count_outcomes),0)) +
          theme_bw() +
          theme(legend.position="bottom") +
          labs(title = this_setting) +
          guides(color = guide_legend(ncol = 1),
                 shape = guide_legend(ncol = 1)) 
      }
      rm(to_plot,workshop)
      consolidate_plot_list(plot_list)
    }
  })
  
  output$OUTPUT_incremental_plane <- renderPlot({print(PLOT_incremental_plane())}, 
                                                res = 96, 
                                                width = function() input$plot_width,
                                                height = function() input$plot_height)
  #_____________________________________________________________________________
  
    
  #(2/3) WTP curve
   #PLOT_WTP_curve <- reactive({
  PLOT_WTP_curve <- eventReactive({input$update_plot|is.null(input$INPUT_switch_shape_and_colour) == FALSE | input$INPUT_fix_xaxis | input$INPUT_include_GDP},{
    
    call_waiter("OUTPUT_WTP_curve")
     
    validate(need(length(count_plot_dimensions())<=2, too_many_dimensions_text))
    to_plot <- dataInput_WTPcurve()

    if (nrow(to_plot) > 1) {
      plot_list = list()
      
      if(input$INPUT_fix_xaxis == TRUE){
        to_plot_xmin = min(to_plot$WTP)
        to_plot_xmax = max(to_plot$WTP)
      }

      #withProgress({ #the slow part of the plot is the sub-setting of the data frame NOT the plotting
        for (this_setting in input$INPUT_include_setting) {
          
          if (this_setting == "Fiji"){this_setting_GDP = 5316.7
          } else if (this_setting == "Indonesia"){this_setting_GDP = 4788.0
          } else if (this_setting == "Papua New Guinea"){this_setting_GDP = 3020.3
          } else if (this_setting == "Timor-Leste"){this_setting_GDP = 2358.4}
          
          plot_list[[length(plot_list)+ 1]] <- apply_plot_dimensions(df = to_plot[to_plot$setting == this_setting,],
                                                                    aes_x="WTP",
                                                                    aes_y="probability",
                                                                    count_plot_dimensions = count_plot_dimensions())  +
            xlab(paste0("Willingness to pay ($/",input$INPUT_include_outcomes,")")) +
            ylab("Probability cost-effective") +
            theme_bw() +
            theme(legend.position = "bottom") +
            labs(title = this_setting)  +
            guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) 
          
          if(input$INPUT_fix_xaxis == TRUE){
            plot_list[[length(plot_list)]] <-  plot_list[[length(plot_list)]] +
              xlim(to_plot_xmin,to_plot_xmax)
          }
          if (input$INPUT_include_GDP == TRUE){
            plot_list[[length(plot_list)]] <- plot_list[[length(plot_list)]] + 
              geom_vline(mapping = NULL, xintercept = this_setting_GDP, linetype="dashed") #+
              #annotate("text", y = 0.25, x = this_setting_GDP*annotate_multiple, label = "GDP per capita", angle = 90) 
            #very difficult NOT to get text to overlap something with range of WTP
          }
          #incProgress(1 / length(input$INPUT_include_setting), message = paste("plotting",this_setting))
        }
      #})
      rm(to_plot)
      consolidate_plot_list(plot_list)
    }
  })
   
  output$OUTPUT_WTP_curve <- renderPlot({print(PLOT_WTP_curve())}, 
                                        res = 96, 
                                        width = function() input$plot_width,
                                        height = function() input$plot_height)
  #_____________________________________________________________________________
  
  
  #(3/3) Tornado plot
  PLOT_tornado_plot <- reactive({
    
    call_waiter("OUTPUT_tornado_plot")
    
    #validate(need(length(count_plot_dimensions())<1, "Please select only one booster strategy and one antiviral strategy"))
    to_plot <- dataInput_TornadoPlot() %>%  ungroup() 
    isolate_base_value <- to_plot[to_plot$direction == "lower" &  to_plot$label == "Long COVID (off/on)", ]
    to_plot <- to_plot %>% filter(label %in% input$INPUT_parameters)
    #NB: saved isolate_base_value before subsetting to INPUT_parameters to allow any number of parameters (don't have to include long COVID)
    
    if (nrow(to_plot)>1){
      
      plot_list = list()
      for (this_setting in input$INPUT_include_setting){
        to_plot_this_setting <- to_plot %>%
          filter(setting == this_setting)
        
        if (nrow(to_plot_this_setting)>1){ #NB: FJI doesn't have unvax
          base.value <- isolate_base_value %>% filter(setting == this_setting)
          base.value <- base.value$mean
          
          if (this_setting == "Fiji"){this_setting_GDP = 5316.7;annotate_multiple = 0.9}
          if (this_setting == "Indonesia"){this_setting_GDP = 4788.0;annotate_multiple = 0.9}
          if (this_setting == "Papua New Guinea"){this_setting_GDP = 3020.3;annotate_multiple = 0.9}
          if (this_setting == "Timor-Leste"){this_setting_GDP = 2358.4;annotate_multiple = 0.85}
          
          # width of columns in plot (value between 0 and 1)
          width <- 0.95
          order_parameters <- to_plot_this_setting %>%
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
            select(perspective,label,mean,direction) %>% 
            rename(value = mean) %>%
            ungroup() %>%
            # create the columns for geom_rect
            mutate(label=factor(label, levels=order_parameters),
                   ymin=pmin(value, base.value),
                   ymax=pmax(value, base.value),
                   xmin=as.numeric(label)-width/2,
                   xmax=as.numeric(label)+width/2)
          rm(to_plot_this_setting)
          
          # create plot
          plot_list[[length(plot_list)+1]] = ggplot() + 
            geom_rect(data = df_2,
                      aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=paste(direction,"estimate"))) + 
            geom_hline(yintercept = base.value) +
            theme_bw() + 
            theme(axis.title.y=element_blank(), legend.position = "bottom",
                  legend.title = element_blank())  +
            ylab(paste("Cost per",gsub("QALYs","QALY",input$INPUT_include_outcomes),"averted (2022 USD)")) +
            scale_x_continuous(breaks = c(1:length(order_parameters)), 
                               labels = order_parameters) +
            coord_flip() +
            labs(title = this_setting)
          
          
          if (input$INPUT_include_GDP == "Yes"){
            plot_list[[length(plot_list)]] <- plot_list[[length(plot_list)]] + 
              #geom_hline(aes(yintercept = this_setting_GDP, color = "GDP per capita"),linetype="dashed") +
              geom_hline(mapping = NULL, yintercept = this_setting_GDP, linetype="dashed") +
              annotate("text", x = 4, y = this_setting_GDP*annotate_multiple, label = "GDP per capita", angle = 90) 
            
          } else{
            plot_list[[length(plot_list)]] <- plot_list[[length(plot_list)]] +
              ylim(min(min(df_2$ymin),0),
                   max(max(df_2$ymax),0))
          }
          rm(df_2)
          if (length(input$INPUT_perspective)>1){
            plot_list[[length(plot_list)]] <- plot_list[[length(plot_list)]] + 
              facet_grid(perspective ~.) 
          }
        }
      }
      rm(to_plot)
      consolidate_plot_list(plot_list)
    }
    
  })
  
  output$OUTPUT_tornado_plot <- renderPlot({print(PLOT_tornado_plot())}, 
                                           res = 96, 
                                           width = function() input$plot_width,
                                           height = function() input$plot_height)
  #_____________________________________________________________________________
  
  
  ### defining download options ################################################
  configure_downloaded_figure <- function(result,this_PLOT){
    #NB: I have chosen to output figures as pdfs as these are vector-based objects
    pdf(result,
        #pdf heights are in inches :(, hence this work around!
        width = input$plot_width/96,
        height = input$plot_height/96
         )
    print(this_PLOT)
    dev.off()
  }
  
  output$download <-  downloadHandler(
  
    filename = function() {
      time = Sys.time()
      time = gsub(':','-',time)
      
      if (input$INPUT_select_sentitivity_analysis == "det") { paste(time, "tornado_plot.pdf")
      } else if (input$tabset == "ICER table") {              paste0(time, " ", input$tabset, ".csv")
      } else {                                                paste0(time, " ", input$tabset, ".pdf")}
    },
    
    content = function(result) {
      if (input$INPUT_select_sentitivity_analysis == "det"){ configure_downloaded_figure(result,PLOT_tornado_plot())
      } else if (input$tabset == "Incremental plane"){       configure_downloaded_figure(result,PLOT_incremental_plane())
      } else if (input$tabset == "Willingness to pay curve"){configure_downloaded_figure(result,PLOT_WTP_curve())
      } else if (input$tabset == "ICER table"){              write.csv(dataInput_ICER_table(), result)}
    }
  )
  #_____________________________________________________________________________
  
}


shinyApp(ui, server) #run application!