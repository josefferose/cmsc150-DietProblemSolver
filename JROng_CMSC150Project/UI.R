#Joseffe Rose Ong
#CMSC 150 - Final Project
#B-5L
#UI
#########################################################

library(shiny)
library(shinydashboard)
library(bslib)

source("PR.R")
source("QSI.R")
source("Simplex.R")


#for simplex
simplex_csv = read.csv("Complete nutritional values for the foods.csv", header=T)
food = as.vector(simplex_csv$Foods)

#########################################################
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "morph"),
  #styles for the sidebar and the boxes in the main panel
  tags$head(
    tags$style(HTML("
      .custom-box {
        background-color: #ccdaf0;
        padding: 15px;
        border-radius: 5px;
        box-shadow =  2px 2px 6px rgb(109,114,121,0.2);
      }
      .side-bar{
       background-color: #ccdaf0;
      }
      .scrollable-table {
        max-height: 300px;
        overflow-y: auto;
      }
    "))
  ),
  
  navbarPage("Menu", 
             tabPanel("Polynomial Regression",
                      h1("Polynomial Regression", style = 'color: #3e67ab;'),
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(
                          fileInput('csv1', "Upload CSV file"),
                          numericInput('degree', "Enter degree", value = 0),
                          numericInput('est',"Enter x-value", value = 0),
                          actionButton('runButton1', 'Run PR'),
                          class = "side-bar"
                          
                        ),
                        mainPanel(
                          div(
                            column(width = 12,
                                   box(
                                     width = NULL,
                                     title = "Coefficients",
                                     status = "primary",
                                     tableOutput("pr_coeff"),
                                     class = "custom-box scrollable-table"  
                                   ),
                                   box(
                                     width = NULL,
                                     title = "Estimate",
                                     status = "primary",
                                     textOutput("pr_est"),
                                     class = "custom-box"  
                                   ),
                                   box(
                                     width = NULL,
                                     title = "Polynomial String",
                                     status = "primary",
                                     textOutput("pr_poly_str"),
                                     class = "custom-box"  
                                   ),
                                   box(
                                     width = NULL,
                                     title = "Matrices for each Iteration",
                                     status = "primary",
                                     uiOutput("pr_matr_iter"),
                                     class = "custom-box scrollable-table" 
                                   )
                            )
                          )
                        )
                      )
             ),
             tabPanel("QSI",
                      h1("Quadratic Spline Interpolation", style = 'color: #3e67ab;'),
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(
                          fileInput('csv2', "Upload CSV file"),
                          numericInput('pointEst2', "Enter point estimate", value = 0),
                          actionButton('runButton2', 'Run QSI'),
                          class = "side-bar",
                        ),
                        mainPanel(
                          div(
                            column(width = 12,
                                   #checker for unsorted data
                                   # box(
                                   #   width = NULL,
                                   #   title = "Data",
                                   #   status = "primary",
                                   #   tableOutput("qsi_data"),
                                   #   class = "custom-box"
                                   # ),
                                   box(
                                     width = NULL,
                                     title = "Functions per Interval",
                                     status = "primary",
                                     tableOutput("qsi_fxn"),
                                     class = "custom-box"
                                   ),
                                   
                                   box(
                                     width = NULL,
                                     title = "X-interval",
                                     status = "primary",
                                     textOutput("qsi_x"),
                                     class = "custom-box"
                                   ),
                                   
                                   box(
                                     width = NULL,
                                     title = "Correct Function",
                                     status = "primary",
                                     textOutput("qsi_correct_fxn"),
                                     class = "custom-box"
                                   ),
                                   
                                   box(
                                     width = NULL,
                                     title = "Estimate",
                                     status = "primary",
                                     textOutput("qsi_est"),
                                     class = "custom-box"
                                   ),
                                   
                                   box(
                                     width = NULL,
                                     title = "Matrices for each Iteration",
                                     status = "primary",
                                     uiOutput("qsi_matr_iter"),
                                     class = "custom-box scrollable-table"
                                   )
                            )
                          )
                        )
                      ),
                      
             ),
             tabPanel("Diet Solver",
                      h1("Diet Solver", style = 'color: #3e67ab;'),
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(
                          checkboxGroupInput("selected", "Select Food", food),
                          actionButton('runButton3', 'Run'),
                          actionButton('all', 'Select all'),
                          actionButton('reset','Reset choices'),
                          class = "side-bar"
                        ),
                        mainPanel(
                          column(width = 12,
                                 box(
                                   width = NULL,
                                   title = "Food choices",
                                   status = "primary",
                                   tableOutput("choices"),
                                   class = "custom-box"
                                 ),
                                 
                                 box(
                                   width = NULL,
                                   title = "Optimal Cost",
                                   status = "primary",
                                   textOutput("cost_opt"),
                                   class = "custom-box"
                                 ),
                                 
                                 box(
                                   width = NULL,
                                   title = "Optimal Menu",
                                   status = "primary",
                                   tableOutput("menu_opt"),
                                   class = "custom-box"
                                 ),
                                 box(
                                   width = NULL,
                                   title = "Basic Solution for each iteration",
                                   status = "primary",
                                   tableOutput("basic_sol_iter"),
                                   class = "custom-box scrollable-table"
                                 ),
                                 box(
                                   width = NULL,
                                   title = "Matrices for each Iteration",
                                   status = "primary",
                                   uiOutput("matr_iter"),
                                   class = "custom-box scrollable-table"
                                 )
                          )
                        )
                      ),
             )
  )
  
)
#########################################################




#########################################################
server <- function(input, output,session) {
  
  #POLYNOMIAL REGRESSION
  observeEvent(input$runButton1, {
    req(input$csv1,input$degree,input$est)
    #check if a file is uploaded and existing
    if (!is.null(input$csv1) && !is.null(input$csv1$datapath)) {
      #read the CSV file
      data1 = read.csv(input$csv1$datapath, header = FALSE)
      #if not incomplete data (len of col x and col y is not the same)
      if(any(is.na(data1[, 1])) || any(is.na(data1[, 2]))) {
        showNotification("Please input a csv file with complete data.", type = "warning")
        return()  # Stop execution if the condition is not met
      }
      #if negative
      if ((input$degree < 1) || (input$degree) > (length(data1[,1]) - 1) ){
        showNotification("Please input a value within range.", type = "warning")
        return()
      }
      
      #call polynomial regression function
      y = PolynomialRegression(input$degree,data1,input$est)
      
      output$pr_coeff <- renderTable({
        if (!is.null(y)) {
          t(data.frame(as.matrix(y$coefficients)))
        }
      }, colnames = FALSE)
      
      output$pr_est <- renderText({
        if (!is.null(y)) {
          y$estimate
        }
      })
      
      output$pr_poly_str <- renderText({
        if (!is.null(y)) {
          y$polynomial_string
        }
      })
      
      pr_matItr_len = length(y$mat_iter)
      output$pr_matr_iter <- renderUI({
        if (!is.null(y)) {
          lapply(1: pr_matItr_len, function(i){
            pr_mat = tagList(
              h4(paste("Iteration",i)),
              renderTable({
                y$mat_iter[[i]]
              })
            )
            return(pr_mat)
          })
        }
      })
    }
  })
  
  
  #QUADRATIC SPLINE INTERPOLATION
  observeEvent(input$runButton2, {
    req(input$csv2,input$pointEst2)
    #check if a file is uploaded and existing
    if (!is.null(input$csv2) && !is.null(input$csv2$datapath)) {
      #read the CSV file
      data2 = read.csv(input$csv2$datapath, header = FALSE)
      
      #other requirements before proceeding
      #for csv
      if (length(data2[,1]) < 3) {
        showNotification("Please input a csv file with a minimum of three(3) data points.", type = "warning")
        return()  
      }else if (any(is.na(data2[, 1])) || any(is.na(data2[, 2]))) {
        showNotification("Please input a csv file with complete data.", type = "warning")
        return()  
      }else if (!identical(data2[, 1], sort(data2[, 1]))) {
        #showNotification("Please make sure that your data is sorted.", type = "warning")
        #return()
        #corrects the order in ascending order using column 1 as reference
        data2 = data2[order(data2[,1]),]
      }
      #for point estimate
      last_row = length(data2[,1])
      if((input$pointEst2 < data2[1,1]) || (input$pointEst2 > data2[last_row,1])){
        showNotification("Please input a value within range.", type = "warning")
        return()
      }
      #checker if the unsorted file is sorted
      # output$qsi_data <- renderTable({
      #   if(!is.null(r)){
      #     data2
      #   }
      # }, colnames = FALSE)
      
      #call QSI function
      r = QSI(data2, input$pointEst2)
      
      
      
      output$qsi_fxn <- renderTable({
        if (!is.null(r)) {
          data.frame(as.matrix(r$fxns_per_interval))
        }
      }, colnames = FALSE)
      
      output$qsi_x <- renderText({
        if (!is.null(r)) {
          r$x_interval
        }
      })
      
      
      output$qsi_correct_fxn <- renderText({
        if (!is.null(r)) {
          r$Correct_fxn
        }
      })
      
      output$qsi_est <- renderText({
        if (!is.null(r)) {
          r$estimate
        }
      })
      
      qsi_matIter_len = length(r$mat_iter)
      output$qsi_matr_iter <- renderUI({
        if (!is.null(r)) {
          lapply(1:qsi_matIter_len, function(i){
            qsi_mat = tagList(
              h4(paste("Iteration",i)),
              renderTable({
                r$mat_iter[[i]]
              })
            )
            return(qsi_mat)
          })
        }
      })
    }
  })
  
  
  
  
  #DIET PROBLEM
  selected_food <- reactiveVal(character(0))
  observeEvent(input$selected, {
    selected_food(input$selected)
  })
  
  #reset user choices
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "selected", "Select Food", food, selected = NULL)
    selected_food(NULL)  # Reset selected_food vector
  })
  
  #select all food  
  observeEvent(input$all, {
    updateCheckboxGroupInput(session, "selected", "Select Food", food, selected = food)
    selected_food(food)  # Update selected_food vector
  })
  
  observeEvent(input$runButton3, {
    req(length(input$selected) > 0)
    #get the indices of selected foods (not including the the unselected foods)
    selectedFood_indices = which(!is.na(match(input$selected, csv$Foods))) #NA is the value if it does not match
    
    #call the Simplex function with the parameters CSV, data frame, and selectedFood indices
    x = Simplex(simplex_csv, selectedFood_indices)
    if(!is.list(x)){
      showNotification("Not Feasible.", type = "warning")
      return()
    }else{
      #creates a matrix with five columns for food choices
      num_cols =  5
      num_food = length(x$food_choices)
      num_empty_cells = num_cols - (num_food %% num_cols)
      
      #fill the matrix with food names and empty "" cells (to avoid repitition)
      choices_matrix = matrix(c(x$food_choices, rep("", num_empty_cells)), ncol = num_cols, byrow = TRUE)
      
      output$choices <- renderTable({
        data.frame(choices_matrix)
      }, colnames = FALSE)
      
      output$cost_opt <- renderText({
        if (!is.null(x)) {
          x$opt_cost
        }
      })
      
      output$menu_opt <- renderTable({
        if (!is.null(x)) {
          x$opt_menu
        }
      })
      
      simplex_matIter_len = length(x$matr_iter)
      output$matr_iter <- renderUI({
        if (!is.null(x)) {
          lapply(1:simplex_matIter_len, function(i){
            simplex_mat = tagList(
              h4(paste("Iteration",i)),
              renderTable({
                x$matr_iter[[i]]
              })
            )
            return(simplex_mat)
          })
        }
      })
      
      output$basic_sol_iter <- renderUI({
        if (!is.null(x)) {
          lapply(1:simplex_matIter_len, function(i){
            simplex_basic = tagList(
              h4(paste("Basic Solution", i)),
              renderTable({
                as.matrix(x$basic_sol_iter[[i]])
              })
            )
            return(simplex_basic)
          })
        }
      })
    }
    
  })
}
#########################################################



shinyApp(ui, server)


# References:
#   PR: Exercise 6
#   QSI: http://nmbooks.eng.usf.edu/ebooks/05inp_spline/inp_05_spline_300_quadratic_example.html
#   Simplex: https://www.ux1.eiu.edu/~cfcem2/simplex-2.pdf 