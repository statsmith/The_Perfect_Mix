
# Libraries

      library(shiny)
      library(shinydashboard)
      library(shinyjs)
      library(shinymaterial)
      library(tidyverse)
      library(readxl)
      library(rlang)
      library(ggforce)
      
# Constants

      n_pts <- 200
      
# Functions ====
      
      # Make Slider Inputs ====
      f_input <- function(i){
              
              shinyjs::hidden(
                      
                      div(id = paste0("div",i),
               
                          material_slider(input_id =  paste0("image", i), 
                                       label = tags$i(paste0("Sample ", i, " (0 = Dislike Extremely, 100 = Like Extremely)")), 
                                       min_value = 0, max_value = 100, initial_value = 0
                                      
                          )           
                          
                      )
              )
      }
      
      # Make Plot Data Frame  ====
      f_df_plot <- function(df_dsgn, n_pts){
              
              df_plot <- data_frame(
                      
                      point_id = as.numeric(1:n_pts),
                      x = runif(n = n_pts, min = 0, max = 1),
                      y = runif(n = n_pts, min = 0, max = 1),
                      myorder = runif(n = n_pts, min = 0, max = 1)
              )
              
              
              df_plot <- df_plot %>% mutate(color = 
                                                    
                                                    case_when(
                                                            
                                                            .$point_id <= as.numeric(df_dsgn[1,2]) * n_pts ~ "color_1",
                                                            .$point_id <= (as.numeric(df_dsgn[1,2]) + as.numeric(df_dsgn[1,3])) * n_pts ~ "color_2",
                                                            .$point_id <= n_pts ~ "color_3"
                                                            
                                                    )
                                            
              ) 
              
              df_plot <- df_plot %>% 
                      arrange(myorder)
              
              return(df_plot)
              
      }
      
      # Make Plot for Each Design Run ====
      f_plot <- function(df_plot, color_1, color_2, color_3){
              
              p <- ggplot() + 
                      geom_circle(data = df_plot, aes(x0=x, y0=y, r = 0.1, fill = color), col = "grey50") +
                      scale_fill_manual(values = c("color_1" = color_1, "color_2" = color_2, "color_3" = color_3), guide = FALSE) +
           
                      theme(
                              panel.background = element_rect(fill = "grey90"),
                              axis.text = element_blank(),
                              axis.title = element_blank(),
                              panel.grid = element_blank()
                      ) + 
                      coord_equal()
              
              return(p)
              
      }
      
      
      
# Data ====
      
      # CHANGE THIS DESIGN ??
      df_design <- read_excel("Perfect_Mix_Design.xlsx")
      
      # Randomize
      # set.seed(042735)
      df_design <- df_design %>% 
              mutate(random_number = runif(n = nrow(df_design))) %>% 
              arrange(random_number) %>% 
              select(-random_number)
 
      # Separated Design into Individual Runs
      l_df_plot <- 1:nrow(df_design) %>% 
              map(~df_design[.x,])
      
      # Makes n Points to Plot According to Colors
      l_df_plot <- map2(.x = l_df_plot, .y = n_pts, .f = f_df_plot)
      
      # Make Plots Based on User Selected Colors - Reactive in Server
      
      # load("plots.RData")
      
# Constants ====
      
      
      
      
# Server ====

shinyServer(function(input, output, session) {

# Create Plots for Each Design Run ====
        
        l_plot <- reactive({
                
                l_plot <- l_df_plot %>% 
                        map(f_plot, 
                            color_1 = input$color_1,
                            color_2 = input$color_2,
                            color_3 = input$color_3
                        )
                
                l_plot
                
        })
        
        
# Create Numeric Inputs ====
        
        observe({
                
                output$ui_input <- renderUI({
                        
                        1:nrow(df_design) %>% map(f_input) 
                        
                })
                
        })
        
# Reactive Values to Track Page ====
        
        rv <- reactiveValues(j = 0, col1_opt = 0.333, col2_opt = 0.333, col3_opt = 0.334)

        observeEvent(input$button_previous,{
                rv$j <-  rv$j - 1
        })

        observeEvent(input$button_next,{
                rv$j <-  rv$j + 1
        })
  
        
# User Interface / Flow ====
        
        observe({
                
                # Hide All Inputs
                0:nrow(df_design) %>% map_chr(~paste0("div",.x)) %>%  map(shinyjs::hide, anim = TRUE)
                
                # Show Current Input
                shinyjs::show(id = paste0("div",rv$j), anim = TRUE)
                
                
                # SHow Hide Enable Disable
                if(rv$j > nrow(df_design + 1)){
                        # shinyjs::hide(id = "div_progress", anim = TRUE)
                        shinyjs::show(id = "div_go", anim = TRUE)
                        shinyjs::hide(id = "div_progress", anim = TRUE)
                        shinyjs::hide(id = "div_plot", anim = TRUE)
               
                } else if(rv$j > nrow(df_design)){
                        # shinyjs::disable(id = "button_next")
                        # shinyjs::show(id = "div_results", anim = TRUE)
                        shinyjs::hide(id = "div_plot", anim = TRUE)
                } else if (rv$j == 0) {
                        shinyjs::disable(id = "button_previous")
                        # shinyjs::hide(id = "div_results", anim = TRUE)
                        # shinyjs::hide(id = "div_plot", anim = TRUE)
                } else {
                        shinyjs::enable(id = "button_next")
                        shinyjs::enable(id = "button_previous")
                        shinyjs::hide(id = "div_results", anim = TRUE)
                        shinyjs::hide(id = "div_colors", anim = TRUE)
                        shinyjs::show(id = "div_plot", anim = TRUE)
                        shinyjs::show(id = "div_progress_bar", anim = TRUE)
                }
                
        })
        
  
        observeEvent(input$Go, {
                
                req(input$Go > 0)
                
                shinyjs::hide(id = "div_go", anim = TRUE)
                shinyjs::show(id = "div_eh", anim = TRUE)
                shinyjs::show(id = "div_results_bar_plot", anim = TRUE)
                shinyjs::show(id = "div_results_optimal_plot", anim = TRUE)
                shinyjs::show(id = "div_yes", anim = TRUE)
                
        })     
        
        
        observeEvent(input$eh, {
                
                req(input$eh > 0)
                
                shinyjs::hide(id = "div_eh", anim = TRUE)
                shinyjs::show(id = "div_adjust", anim = TRUE)
                
        })
        
        
        # self Adjust Optimal ====
        
        observeEvent(input$eh,{
                
                update_material_slider(session, input_id =  "col1_opt", value = 100*rv$col1_opt)
                update_material_slider(session, input_id =  "col2_opt", value = 100*rv$col2_opt)
                update_material_slider(session, input_id =  "col3_opt", value = 100*rv$col3_opt)
                
                # shinyjs::hide(id = "div_yes", anim = TRUE)

        })
        
        
        observeEvent(input$update,{
                
                rv$col1_opt <- input$col1_opt/100
                rv$col2_opt <- input$col2_opt/100
                
                rv$col3_opt <- 1 - (rv$col1_opt + rv$col2_opt)
                
                update_material_slider(session, input_id =  "col1_opt", value = 100*rv$col1_opt)
                update_material_slider(session, input_id =  "col2_opt", value = 100*rv$col2_opt)
                update_material_slider(session, input_id =  "col3_opt", value = 100*rv$col3_opt)
                
                # shinyjs::show(id = "div_yes", anim = TRUE)

        })
        
        
        
        
# Current Plot ====
        
        output$plot <- renderPlot({
                
                req(rv$j > 0 & rv$j <=  nrow(df_design))
                print(l_plot()[[rv$j]])
                
        }, height = 350, width = 350)
        
        
# Progress Bar ====
        
        output$my_progress <- renderPlot({
                
                df <- data_frame(
                        Percent = 100 * rv$j / (nrow(df_design) + 1),
                        Result = "Complete"
                )
                
                p <- ggplot(data=df, aes(x=Percent, y=Result))
                
                # inlineCSS(list(.Correct = "color: #79e0c1", .Oops = "color: #f9ba89")),
                
                p <- p + geom_segment(aes(x=0, xend=Percent, y=Result, yend=Result), col="green", size=4)
                p <- p + geom_segment(aes(x=Percent, xend=100, y=Result, yend=Result), col="grey80", size=4)
                
                p <- p + theme_minimal() + xlab(paste0("Progress: ", round(100*rv$j / (nrow(df_design) + 1), 1), "%")) + ylab("")
                p <- p + theme(
                        # axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank())
                print(p)
                
                
        })
        
# Create Table of Respondent Scores ====
        
        df <- reactive({

                df <- 1:nrow(df_design) %>% 
                        map(~input[[paste0("image", .x)]]) %>% 
                        unlist() %>% 
                        data_frame() 
                
                names(df) <- c("myScores")
                
                df

        })

        
# Optimize ====
        
        my_optimal <- eventReactive(input$Go, {
                
                df <- bind_cols(df_design, df())
        
                # Model Equation
                myFit <- lm(myScores ~ X1 + X2 + X3 + X1*X2 + X1*X3 + X2*X3 -1, data = df)

                # My Coefficients to Optimize
                myCoeff <- myFit$coefficients


                # Function
                fLike <- function(theta) {

                        X1 <- theta[1]
                        X2 <- theta[2]
                        X3 <- theta[3]

                        X1 * myCoeff[1] +
                                X2 * myCoeff[2] +
                                X3 * myCoeff[3] +
                                X1*X2 * myCoeff[4] +
                                X1*X3 * myCoeff[5] +
                                X2*X3 * myCoeff[6]

                }

                # UI Matrix

                ui <- matrix(
                        c(1,1,1,
                          -1,-1,-1,
                          1,0,0,
                          -1,0,0,
                          0,1,0,
                          0,-1,0,
                          0,0,1,
                          0,0,-1),
                        nrow = 8,
                        byrow = TRUE
                )

                # CI Matrix

                ci <- matrix(c(0.99999,-1.00001,0,-1,0,-1,0,-1), nrow = 8)

                # Starting Values

                theta0 <- c(0.34, 0.33, 0.33)


                # Optimization

                # ui %*% theta - ci >= 0
                ui %*% theta0 - ci


                out1 <- constrOptim(theta = theta0,
                                    f = fLike,
                                    NULL,
                                    ui = ui,
                                    ci = ci,
                                    control = list(fnscale = -1)
                )

                
                rv$col1_opt <- out1$par[1]
                rv$col2_opt <- out1$par[2]
                rv$col3_opt <- out1$par[3]
                
                out1$par
                
        })
        
        
# Output ====
        
        output$bar_plot <- renderPlot({
                
                req(my_optimal())
                # req(rv$col3_opt > 0)
                
                validate(
                        
                        need(rv$col3_opt > 0, "Total > 100%, Please Adjust!")
                        
                )
                
                df_plot <- data_frame(
                        
                        my_color = factor(
                                x = c(input$color_1, input$color_2, input$color_3), 
                                levels = c(input$color_1, input$color_2, input$color_3)
                        ),
                                        
                        my_percent = c(rv$col1_opt, rv$col2_opt, rv$col3_opt)
                ) 
                
                p <- ggplot() +
                        geom_bar(data = df_plot, aes(x = my_color, y = 100 * my_percent, fill = my_color), stat = "identity", col = "grey37") +
                        scale_fill_manual(values = c(input$color_1, input$color_2, input$color_3), guide = FALSE) +
                        xlab("") + ylab("Percent (%)") +
                        scale_y_continuous(limits = c(0, max(100*df_plot$my_percent) + 10), breaks = seq(0, max(100*df_plot$my_percent) + 10, 10)) +
                        theme_minimal(base_size = 20) +
                        theme(
                                axis.text.x = element_blank(),
                                axis.title.x = element_blank()
                        )
                
                print(p)
                
        }, height = 350, width = 350)
        
        output$optimal_plot <- renderPlot({
                
                req(my_optimal())
                req(rv$col3_opt > 0)
                
                # Duplicate Code - Separate!
                df_plot <- data_frame(
                        
                        Run = 1,
                        
                        X1 = rv$col1_opt,
                        X2 = rv$col2_opt,
                        X3 = rv$col3_opt
                        
                )  
                
                df_plot <- f_df_plot(df_dsgn = df_plot, n_pts = n_pts)
                
                optimal_plot <- f_plot(df_plot = df_plot, color_1 = input$color_1, color_2 = input$color_2, color_3 = input$color_3)
                
                optimal_plot
                
        }, height = 350, width = 350)
        
        
      
# Test ====
        
        output$table <- DT::renderDataTable({df()})

              
      
})