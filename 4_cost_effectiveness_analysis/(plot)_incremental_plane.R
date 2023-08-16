#Require result (CommandDeck_result_long) with variables outcome, setting, perspective, discounting_rate, antiviral_cost_scenario, booster_vax_scenario,antiviral_type,netCost,count_outcomes
require(ggpubr);options(scipen = 1000)

INPUT_plot_name = "long_COVID" # options: static_plot, reduced_static_plot (hosted R Shiny), long_COVID (SM S4.7)


INPUT_include_setting = c("Fiji","Indonesia","Papua New Guinea","Timor-Leste")
INPUT_include_booster_vax_scenario = c("high risk adults"
                                       , "all adults"
                                       , "all adults (catch-up campaign)"
                                       , "high-risk adults (catch-up campaign)"
                                        )
INPUT_antiviral_type = c() # c("nirmatrelvir_ritonavir")
INPUT_antiviral_type = c(INPUT_antiviral_type,"no antiviral")
INPUT_include_antiviral_target_group = c("no antiviral")
INPUT_perspective = c("healthcare perspective","societal perspective")
INPUT_discounting_rate = 3
INPUT_antiviral_cost_scenario = "middle-income reference price ($250 USD per schedule)"
INPUT_include_outcomes = "QALYs"

# INPUT_include_booster_vax_scenario = "no booster"
# INPUT_antiviral_type = c("nirmatrelvir_ritonavir")
# INPUT_include_antiviral_target_group = c("adults with comorbidities","all adults","unvaccinated adults")

### load data
if (INPUT_plot_name %in% c("static_plot","reduced_static_plot")){
  if (INPUT_plot_name == "static_plot"){
    this_pattern = "CommandDeck_result_long_1_*"
  } else if (INPUT_plot_name == "reduced_static_plot"){
    this_pattern = "CommandDeck_result_long_reduced_*"
  }
  
  list_poss_Rdata = list.files(path = "Rshiny/x_results/",pattern = this_pattern)
  if (length(list_poss_Rdata) > 0) {
    list_poss_Rdata_details = double()
    for (j in 1:length(list_poss_Rdata)) {
      list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                      file.info(paste("Rshiny/x_results/", list_poss_Rdata[[j]], sep = ''))$mtime)
    }
    latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    load(file = paste("Rshiny/x_results/", latest_file, sep = ''))
    
    #if part1, also load part2 and join
    if (this_pattern == "CommandDeck_result_long_1_*"){
      load(file = paste("Rshiny/x_results/", gsub("long_1","long_2",latest_file), sep = ''))
      CommandDeck_result_long = rbind(CommandDeck_result_long_part1,CommandDeck_result_long_part2); rm(CommandDeck_result_long_part1,CommandDeck_result_long_part2)
    }
  } else{
    stop(paste("no results to load!"))
  }
} else if (INPUT_plot_name == "long_COVID"){
  load(file = "Rshiny/x_results/long_COVID_results.Rdata")
  CommandDeck_result_long = long_COVID_results
}


### load functions
subset_data_to_widgets <- function(df){
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
consolidate_plot_list <- function(plot_list){
  if(length(plot_list) == 1){row_num = 1; col_num = 1}
  if(length(plot_list) == 2){row_num = 1; col_num = 2}
  if(length(plot_list) > 2) {row_num = 2; col_num = 2}
  plot = ggarrange(plotlist = plot_list, ncol = col_num, nrow = row_num, common.legend = TRUE, legend = "bottom")
}
apply_plot_dimensions <- function(df,aes_x,aes_y,plot_dimensions){
  
  if (length(plot_dimensions) == 0){
    this_plot = ggplot(df) +
      geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]]))
  } else if (length(plot_dimensions) == 1){
    
    if(INPUT_plot_name == "long_COVID"){
      df = df %>%
        mutate("{plot_dimensions[1]}" := paste0(.data[[plot_dimensions[1]]]," (long COVID ",long_COVID,")"))
    }
    this_plot = ggplot(df) +
      geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]],color=as.factor(.data[[plot_dimensions[1]]]))) +
      labs(color = paste(gsub("_"," ", plot_dimensions[1])))
  } else if (length(plot_dimensions) == 2){
    this_plot = ggplot(df) +
      geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]],color=as.factor(.data[[plot_dimensions[1]]]),shape = as.factor(.data[[plot_dimensions[2]]]))) +
      labs(color = paste(gsub("_"," ", plot_dimensions[1])),
           shape = paste(gsub("_"," ", plot_dimensions[2])))
  }
  if (length(INPUT_perspective)>1){
    this_plot = this_plot + 
      facet_grid(perspective ~.) 
  }
  
  return(this_plot)
}
plot_dimensions <- function(INPUT_antiviral_cost_scenario,INPUT_discounting_rate,INPUT_include_antiviral_target_group,INPUT_include_booster_vax_scenario){
  plot_dimension_vector = c()
  
  #if(INPUT_plot_name == "long_COVID"){plot_dimension_vector = c(plot_dimension_vector,"long_COVID")}
  if (length(INPUT_antiviral_cost_scenario)>1)       {plot_dimension_vector = c(plot_dimension_vector,"antiviral_cost_scenario")}
  if (length(INPUT_discounting_rate)>1)              {plot_dimension_vector = c(plot_dimension_vector,"discounting_rate")}
  if (length(INPUT_include_antiviral_target_group)>1){plot_dimension_vector = c(plot_dimension_vector,"antiviral_target_group")}
  if (length(INPUT_include_booster_vax_scenario)>1)  {plot_dimension_vector = c(plot_dimension_vector,"booster_vax_scenario")}
  
  plot_dimension_vector
}

### Subset results
to_plot =  subset_data_to_widgets(CommandDeck_result_long)%>%
  filter(outcome %in% INPUT_include_outcomes)


### Create plots
plot_list = list()
for (this_setting in INPUT_include_setting){
  workshop =  to_plot[to_plot$setting == this_setting,]
  plot_list[[length(plot_list)+ 1]] = apply_plot_dimensions(df = workshop,
                                                            aes_x="netCost",
                                                            aes_y="count_outcomes",
                                                            plot_dimensions = plot_dimensions(INPUT_antiviral_cost_scenario,INPUT_discounting_rate,INPUT_include_antiviral_target_group,INPUT_include_booster_vax_scenario))  +
    ylab(paste(INPUT_include_outcomes,"averted")) +
    xlab("incremental cost (2022 USD)") +
    xlim(min(min(workshop$netCost),0), 
         max(max(workshop$netCost),0)) + 
    ylim(min(min(workshop$count_outcomes),0), 
         max(max(workshop$count_outcomes),0))  +
    theme_bw() +
    theme(legend.position="bottom") +
    labs(title = this_setting) +
    guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) + 
    scale_color_manual(values = wesanderson::wes_palette( name="Zissou1"))
    # scale_colour_manual(values = c("all adults (catch-up campaign)" = "chocolate1",
    #                                "all adults" = "chocolate3",
    #                                "high risk adults" = "orchid3",
    #                                "high-risk adults (catch-up campaign)" = "orchid1"
    #                       
    #                                ))

}

print(consolidate_plot_list(plot_list))

#let's create a table to summarise the impact of long COVID on the cost-effectiveness of booster doses
if (INPUT_plot_name == "long_COVID"){
  table_S4_1 = to_plot %>% 
    group_by(evaluation_level, perspective, discounting_rate,setting,booster_vax_scenario,antiviral_cost_scenario,
             antiviral_type,antiviral_target_group,long_COVID,outcome) %>%
    summarise(
      #NB: costs don't change!
      #expected_interventionCost = mean(interventionCost),           #long COVID doesn't impact the cost of the booster doses
      #expected_healthcareCostAverted = mean(healthcareCostAverted), #long COVID doesn't impact immediate healthcare needs of severe cases
      #expected_productivityLoss = mean(productivityLoss),           #we haven't included any productivity losses associated with long COVID
      #expected_netCost = mean(netCost),
      expected_count_outcome = mean(count_outcomes),
              .groups = "keep") %>%
    ungroup() %>%
    #outcomes averted are not influenced by the CEA perspective
    filter(perspective == "healthcare perspective") %>%
    select(-perspective,-evaluation_level,-antiviral_cost_scenario,-discounting_rate,-antiviral_type,-antiviral_target_group,-outcome) %>%
    pivot_wider(values_from = expected_count_outcome,
                names_from = long_COVID,
                names_prefix = "long_COVID_") 
  
  write.csv(table_S4_1, "x_results/table_S4.1.csv")

}



### make catch-up campaign a shape
# to_plot = to_plot %>%
#   mutate(
#     eligible_group = case_when(
#       booster_vax_scenario %in% c(
#         "all adults who have previously completed their primary schedule but have not recieved a booster",
#         "all adults"
#       ) ~ "all adults",
#       booster_vax_scenario %in% c(
#         "high-risk adults who have previously completed their primary schedule but have not recieved a booster",
#         "high risk adults"
#       ) ~ "high risk adults"
#     ),
#     program_type = case_when(
#       booster_vax_scenario %in% c(
#         "all adults who have previously completed their primary schedule but have not recieved a booster",
#         "high-risk adults who have previously completed their primary schedule but have not recieved a booster"
#       ) ~ "catchup campaign",
#       TRUE ~ "standard campaign"
#     )
#   )
# 
# plot_list = list()
# for (this_setting in INPUT_include_setting){
#   
#   plot_list[[length(plot_list)+ 1]] = ggplot(to_plot[to_plot$setting == this_setting,]) +
#     geom_point(aes(x = netCost,y=count_outcomes,color=as.factor(eligible_group),shape = as.factor(program_type))) +
#     labs(color ="eligible group",
#          shape = "program type") +
#     ylab(paste(INPUT_include_outcomes,"averted")) +
#     xlab("incremental cost (2022 USD)") +
#     theme_bw() +
#     theme(legend.position="bottom") +
#     labs(title = this_setting) +
#     guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) + 
#     scale_shape_manual(
#       values = c("catchup campaign" = 1, "standard campaign" = 16)
#     )
#   
# }
# 
# print(consolidate_plot_list(plot_list))


### try all settings on one axis
# plot_list = list()
# for (this_booster_vax_scenario in INPUT_include_booster_vax_scenario){
#   plot_list[[length(plot_list)+ 1]] = ggplot( to_plot[to_plot$booster_vax_scenario == this_booster_vax_scenario,]) +
#     geom_point(aes(x = netCost,y=count_outcomes,color=as.factor(setting))) +
#     ylab(paste(INPUT_include_outcomes,"averted")) +
#     xlab("incremental cost (2022 USD)") +
#     theme_bw() +
#     theme(legend.position="bottom") +
#     labs(title = this_booster_vax_scenario) +
#     guides(color = guide_legend(ncol = 1),shape = guide_legend(ncol = 1)) +
#     scale_x_continuous(trans = pseudolog10_trans)+
#     scale_y_continuous(trans=pseudolog10_trans)
# 
# }
# 
# print(consolidate_plot_list(plot_list))
#can't log scale since can't log negative numbers

       