# This script creates a plot of the likelihood of interventions being cost-effective
# by varying willingness to pay thresholds

options(scipen = 1000); require(ggpubr)

INPUT_plot_name = "antiviral_wastage_rate" # options: static_plot, reduced_static_plot (hosted R Shiny), antiviral_wastage_rate (SM Figure S4.6)

INPUT_include_setting = c("Fiji","Indonesia","Papua New Guinea","Timor-Leste")
INPUT_include_outcome = "QALYs"
INPUT_perspective = "healthcare perspective"
INPUT_discounting_rate = 3
INPUT_antiviral_cost_scenario = c("low generic reference price ($25 USD per schedule)",
                                  "middle-income reference price ($250 USD per schedule)",
                                  "high-income reference price ($530 USD per schedule)")
INPUT_include_booster_vax_scenario = c("no booster")
INPUT_antiviral_type = c("nirmatrelvir_ritonavir")
INPUT_antiviral_type = c(INPUT_include_antiviral_type,"no antiviral")
INPUT_include_antiviral_target_group = c("adults with comorbidities")
INPUT_fix_xaxis = TRUE
INPUT_include_GDP = TRUE


if (INPUT_plot_name %in% c("static_plot","reduced_static_plot")){

  if (INPUT_plot_name == "static_plot")          this_pattern = "CEAC_dataframe_20*"
  if (INPUT_plot_name == "reduced_static_plot")  this_pattern = "CEAC_dataframe_reduced_*"
  
  list_poss_Rdata = list.files(path = "07_shiny/x_results/",pattern = this_pattern)
  if (length(list_poss_Rdata) > 0) {
    list_poss_Rdata_details = double()
    for (j in 1:length(list_poss_Rdata)) {
      list_poss_Rdata_details <- rbind(list_poss_Rdata_details,
                                      file.info(paste0("07_shiny/x_results/", list_poss_Rdata[[j]]))$mtime)
    }
    latest_file <- list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    load(file = paste0("07_shiny/x_results/", latest_file))
  
  } else{
    stop(paste("Can't find results",this_setting))
  }
  
} else if (INPUT_plot_name == "antiviral_wastage_rate"){
  
  load(file = "07_shiny/x_results/antiviral_wastage_results.Rdata")
  CEAC_dataframe <- antiviral_wastage_results
  
}


### load functions _____________________________________________________________
subset_data_to_selected <- function(df) {
  df %>%
    filter(
      setting %in% INPUT_include_setting &
        perspective %in% INPUT_perspective  &
        discounting_rate %in% INPUT_discounting_rate &
        antiviral_cost_scenario %in% INPUT_antiviral_cost_scenario &
        booster_vax_scenario %in% INPUT_include_booster_vax_scenario &
        antiviral_target_group %in% INPUT_include_antiviral_target_group &
        antiviral_type %in% c("no antiviral", INPUT_antiviral_type)
    )
}

consolidate_plot_list <- function(plot_list) {
  if(length(plot_list) == 1){row_num = 1; col_num = 1}
  if(length(plot_list) == 2){row_num = 1; col_num = 2}
  if(length(plot_list) > 2) {row_num = 2; col_num = 2}
  plot <- ggarrange(plotlist = plot_list, 
                    ncol = col_num, 
                    nrow = row_num, 
                    common.legend = TRUE, 
                    legend = "bottom")
}

apply_plot_dimensions <- function(df, aes_x, aes_y, count_plot_dimensions) {
  
  if (length(count_plot_dimensions) == 0){
    this_plot <- ggplot(df) +
      geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]]))
    
  } else if (length(count_plot_dimensions) == 1) {
    if("antiviral_wastage_rate" %in% colnames(df)){
      this_plot <- ggplot(df) +
        geom_point(aes(x = .data[[aes_x]],
                       y=.data[[aes_y]],
                       color=as.factor(.data[[count_plot_dimensions[1]]]))) +
        labs(color = paste(gsub("_"," ", count_plot_dimensions[1]))) +
        facet_grid(paste0(antiviral_wastage_rate*100,"%")~.)
    } else{
      this_plot <- ggplot(df) +
        geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]],color=as.factor(.data[[count_plot_dimensions[1]]]))) +
        labs(color = paste(gsub("_"," ", count_plot_dimensions[1])))
    }
    
  } else if (length(count_plot_dimensions) == 2) {
    if (aes_x == "WTP" & count_plot_dimensions[2] == "booster_vax_scenario"){
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
  
  return(this_plot)
}

count_plot_dimensions <- function(INPUT_antiviral_cost_scenario,
                                  INPUT_discounting_rate,
                                  INPUT_include_antiviral_target_group,
                                  INPUT_include_booster_vax_scenario) {
  
  plot_dimension_vector = c()
  
  if (length(INPUT_antiviral_cost_scenario)>1)        plot_dimension_vector = c(plot_dimension_vector,"antiviral_cost_scenario")
  if (length(INPUT_discounting_rate)>1)               plot_dimension_vector = c(plot_dimension_vector,"discounting_rate")
  if (length(INPUT_include_antiviral_target_group)>1) plot_dimension_vector = c(plot_dimension_vector,"antiviral_target_group")
  if (length(INPUT_include_booster_vax_scenario)>1)   plot_dimension_vector = c(plot_dimension_vector,"booster_vax_scenario")
  
  return(plot_dimension_vector)
}


### Create plots _______________________________________________________________
to_plot = subset_data_to_selected(CEAC_dataframe)%>%
  filter(outcome %in% INPUT_include_outcomes) 

if (nrow(to_plot) > 1) {
  plot_list = list()
  
  if(INPUT_fix_xaxis == TRUE){
    to_plot_xmin = min(to_plot$WTP)
    to_plot_xmax = max(to_plot$WTP)
  }
  
  for (this_setting in INPUT_include_setting) {
    
    if (this_setting == "Fiji"){this_setting_GDP = 5316.7
    } else if (this_setting == "Indonesia"){this_setting_GDP = 4788.0
    } else if (this_setting == "Papua New Guinea"){this_setting_GDP = 3020.3
    } else if (this_setting == "Timor-Leste"){this_setting_GDP = 2358.4}
    
    plot_list[[length(plot_list)+ 1]] <- apply_plot_dimensions(df = to_plot[to_plot$setting == this_setting,],
                                                              aes_x="WTP",
                                                              aes_y="probability",
                                                              count_plot_dimensions = count_plot_dimensions(INPUT_antiviral_cost_scenario,INPUT_discounting_rate,INPUT_include_antiviral_target_group,INPUT_include_booster_vax_scenario))  +
      xlab(paste0("Willingness to pay ($/",INPUT_include_outcomes,")")) +
      ylab("Probability cost-effective") +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(title = this_setting)  +
      guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) +
      scale_color_manual(values = wesanderson::wes_palette( name="FantasticFox1"))
    
    if(INPUT_fix_xaxis == TRUE){
      plot_list[[length(plot_list)]] <- plot_list[[length(plot_list)]] +
        xlim(to_plot_xmin,to_plot_xmax)
    }
    if (INPUT_include_GDP == TRUE){
      plot_list[[length(plot_list)]] <- plot_list[[length(plot_list)]] + 
        geom_vline(mapping = NULL, xintercept = this_setting_GDP, linetype="dashed") #+
      #annotate("text", y = 0.25, x = this_setting_GDP*annotate_multiple, label = "GDP per capita", angle = 90) 
      #very difficult NOT to get text to overlap something with range of WTP
    }
  }

  #rm(to_plot)
  print(consolidate_plot_list(plot_list))
}

