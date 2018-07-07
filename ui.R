
# Libraries
      
      library(shiny)
      library(shinydashboard)
      library(shinyjs)
      library(shinymaterial)
      library(tidyverse)
      library(readxl)
      library(colourpicker)
      
      
# Constants
      
        color_choices <- c("blue", "red", "white", "yellow", "green", "orange", "black")
      
# Data 
      
      
      # Design
      # df_design <- read_excel("Perfect_Mix_Design.xlsx")
      
      
    
      

# Data

# UI

      
      material_page(
              
              title = "The Perfect Mix",
              nav_bar_color = "teal",
              
              useShinyjs(),
              
              tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "myCSS.css")
              ),
              
              tags$br(),
              
              
              material_row(
                      
                      material_column(
                              
                              width = 3,
                              
                              material_card(
                                      
                                      title = "",
                                      depth = 4,
                                      
                                      div(id = "div0",
                                          
                                          tags$i("Directions"),
                                          tags$p(" "),
                                          tags$ol(
                                                  tags$li("Select colors"),
                                                  tags$li("Rate samples"),
                                                  tags$li("Find your perfect mix!")
                                          )
                                      ),
                                      
                                      div(id = "div_colors",
                                          
                                          colourpicker::colourInput(inputId = "color_1", label = "Color 1", 
                                                                    palette = "limited", 
                                                                    allowedCols = c("red","orange","yellow","green","blue","violet","white","black"),
                                                                    value = "violet",
                                                                    showColour = "background"
                                          ),
                                          
                                          colourpicker::colourInput(inputId = "color_2", label = "Color 2", 
                                                                    palette = "limited", 
                                                                    allowedCols = c("red","orange","yellow","green","blue","violet","white","black"),
                                                                    value = "blue",
                                                                    showColour = "background"
                                          ),
                                          
                                          colourpicker::colourInput(inputId = "color_3", label = "Color 3", 
                                                                    palette = "limited", 
                                                                    allowedCols = c("red","orange","yellow","green","blue","violet","white","black"),
                                                                    value = "white",
                                                                    showColour = "background"
                                          )
                                          
                                      ),
                                      
                                      
                                      div(id = "div_progress",
                                          
                                          material_button(input_id = "button_previous", label = "Prev"),
                                          material_button(input_id = "button_next", label = "Next"),
                                          
                                          tags$br(), tags$br(),
                                          
                                          shinyjs::hidden(
                                                  
                                                  div(id = "div_progress_bar",
                                                      
                                                      plotOutput("my_progress", height = "60px")                                              
                                                      
                                                  )
                                          )
                                          

                                          
                                      ),
                                      
                                      shinyjs::hidden(
                                              
                                              div(id = "div_go",
                                                  
                                                  material_button(input_id = "Go", label = "Find My Perfect Mix")
                                                  
                                              )
                                      ),
                                      
                                      shinyjs::hidden(
                                              
                                              div(id = "div_eh",
                                                  
                                                  material_button(input_id = "eh", label = "Eh...Let Me Adjust")
                                                  
                                              )
                                              
                                      ),
                                  
                                      shinyjs::hidden(
                                              
                                              div(id = "div_adjust",
                                                  
                                                  material_button(input_id =  "update", label = "Update"),
                                                  
                                                  tags$br(), tags$br(), 
                                                  
                                                  material_slider(input_id = "col1_opt", label = "Color 1", min_value = 0, max_value = 100, initial_value = 33),
                                                  material_slider(input_id = "col2_opt", label = "Color 2", min_value = 0, max_value = 100, initial_value = 33),
                                                  
                                                  shinyjs::disabled(
                                                          material_slider(input_id = "col3_opt", label = "Color 3 (Calculated on Update)", min_value = 0, max_value = 100, 34, col = "#D3D3D3")        
                                                  ),
                                                  
                                                  # colourpicker::colourInput(inputId = "color_1_viz", label = "", 
                                                  #                           palette = "limited", 
                                                  #                           allowedCols = c("red","orange","yellow","green","blue","violet","white","black"),
                                                  #                           value = "violet",
                                                  #                           showColour = "background"
                                                  #                               
                                                  # ),
                                                   
                                                  tags$br(), tags$br()
                                                  
                                              )                                              
                                      ),
                                      
                                      shinyjs::hidden(
                                              
                                              div(id = "div_yes",
                                                  
                                                  material_button(input_id = "yes", label = "Make it!")
                                                  
                                              )
                                              
                                      )
                                      
                              )
                              
                      ),
                      
                      
                      material_column(
                              
                              width = 9,
                              
                              material_card(
                                      
                                      title = "",
                                      depth = 4,
                                      
                                      uiOutput("ui_input"),
                                      
                                      div(id = "div_plot",
                                          
                                          plotOutput("plot", width = "100%")
                                          
                                      ),
                                      
                                      shinyjs::hidden(
                                              div(id = "div_results_optimal_plot",
                                                  
                                                  plotOutput("optimal_plot")
                                                  
                                              )
                                      ),
                                      
                                      shinyjs::hidden(
                                              
                                              div(id = "div_results_bar_plot",
                                                  
                                                  plotOutput("bar_plot")
                                                  
                                              )
                                      )
                              )
                      )
              )
      )
      
      
###################################### EOF ######################################
            
            