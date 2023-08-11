#Require result (CommandDeck_result_long) with variables outcome, setting, perspective, discounting_rate, antiviral_cost_scenario, booster_vax_scenario,antiviral_type,netCost,count_outcomes
require(ggpubr);options(scipen = 1000)


INPUT_include_setting = c("Papua New Guinea","Timor-Leste","Fiji","Indonesia")
INPUT_include_booster_vax_scenario = c("high risk adults"
                                       , "all adults"
                                       , "all adults (catch-up campaign)"
                                       , "high-risk adults (catch-up campaign)"
                                        )
INPUT_antiviral_type = c() # c("nirmatrelvir_ritonavir")
INPUT_antiviral_type = c(INPUT_antiviral_type,"no antiviral")
INPUT_include_antiviral_target_group = c("no antiviral")
INPUT_perspective = "societal perspective"
INPUT_discounting_rate = 3
INPUT_antiviral_cost_scenario = "middle_income_cost"
INPUT_include_outcomes = "QALYs"


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
    this_plot = ggplot(df) +
      geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]],color=as.factor(.data[[plot_dimensions[1]]]))) +
      labs(color = paste(gsub("_"," ", plot_dimensions[1])))
  } else if (length(plot_dimensions) == 2){
    this_plot = ggplot(df) +
      geom_point(aes(x = .data[[aes_x]],y=.data[[aes_y]],color=as.factor(.data[[plot_dimensions[1]]]),shape = as.factor(.data[[plot_dimensions[2]]]))) +
      labs(color = paste(gsub("_"," ", plot_dimensions[1])),
           shape = paste(gsub("_"," ", plot_dimensions[2])))
  }
  
  return(this_plot)
}
plot_dimensions <- function(INPUT_antiviral_cost_scenario,INPUT_discounting_rate,INPUT_include_antiviral_target_group,INPUT_include_booster_vax_scenario){
  plot_dimension_vector = c()
  
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
  plot_list[[length(plot_list)+ 1]] = apply_plot_dimensions(df = to_plot[to_plot$setting == this_setting,],
                                                            aes_x="netCost",
                                                            aes_y="count_outcomes",
                                                            plot_dimensions = plot_dimensions(INPUT_antiviral_cost_scenario,INPUT_discounting_rate,INPUT_include_antiviral_target_group,INPUT_include_booster_vax_scenario))  +
    ylab(paste(INPUT_include_outcomes,"averted")) +
    xlab("incremental cost (2022 USD)") +
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

       