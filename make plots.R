
# Libraries ====
        
        library(tidyverse)
        library(readxl)
        library(DT)
        library(ggforce)


# Constants ====
        
        setwd("C:/Users/HFDSS103/OD/Community/Coffee House Web App")
              
        # Number of Points to Generate for Each Plot  
        n_pts <- 200
        
        color_1 <- "blue"
        color_2 <- "green"
        color_3 <- "yellow"
        
# Design ====
        
        
        df_design <- read_excel("Coffee_House_Design.xlsx")
        
        # Randomize
        set.seed(042735)
        df_design <- df_design %>% 
                mutate(random_number = runif(n = nrow(df_design))) %>% 
                arrange(random_number) %>% 
                select(-random_number)
        
                
        
        
# Make a Function to Make Plot Data Frame  ====
        
        f_df_plot <- function(df_dsgn, n_pts){
                
                # Test 
                # df_dsgn <- df_design
                
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
        
        
# Make Function to Make Plot for Each Desing Run ====
        
        f_plot <- function(df_plot, color_1, color_2, color_3){
                
                p <- ggplot() + 
                        geom_circle(data = df_plot, aes(x0=x, y0=y, r = 0.1, fill = color), col = "grey50") +
                        scale_fill_manual(values = c(color_1, color_2, color_3), guide = FALSE) +
                        # theme_minimal()
                        # theme_dark() +
                        theme(
                                panel.background = element_rect(fill = "grey90"),
                                axis.text = element_blank(),
                                axis.title = element_blank(),
                                panel.grid = element_blank()
                        ) + 
                        coord_equal()
                
                return(p)
                
        }
        
         
        # purrr function Design
        
        # Separated Design into Individual Runs
        l_df_plot <- 1:nrow(df_design) %>% 
                map(~df_design[.x,])
        
        # Makes n Points to Plot According to Colors
        l_df_plot <- map2(.x = l_df_plot, .y = n_pts, .f = f_df_plot)
        
        # Makes Plot for Each Desing Run
        
        
        l_plot <- 
                
                l_df_plot %>% map(f_plot, color_1 = "blue", color_2 = "red", color_3 = "white")
        
        
# Save
        
        # save(l_plot, file = "plots.RData")
        
        