library(shiny)

ui <- fluidPage(
  
  checkboxGroupInput("setting_list","Settings to include",c("Fiji","Indonesia","Papua New Guinea","Timor-Leste")),
  radioButtons("perspective","Perspective", c("healthcare perspective","societal perspective")),
  sliderInput("discounting_rate","Discounting rate (%)", value = 3, min = 0, max = 10),
  radioButtons("antiviral_cost","Antiviral cost", c("low_generic_cost","middle_income_cost", "high_income_cost")),
  checkboxGroupInput("booster_vax_scenario","Booster strategy",c(
    "high risk adults"
    ,"all adults"                    
    ,"all adults who have previously completed their primary schedule but have not recieved a booster"                       
    ,"high-risk adults who have previously completed their primary schedule but have not recieved a booster"           
    ,"no booster"  
  )),
  checkboxGroupInput("antiviral_vax_scenario","Antiviral strategy",c(
    "high risk adults",
    "unvaccinated adults",
    "high risk and unvaccinated adults",
    "all adults",                    
    "no antiviral"  
  ))
  
)

server <- function(input, output, session) {
}

shinyApp(ui, server)