#Joseffe Rose Ong
#CMSC 150 - Final Project
#B-5L
#Diet Problem

#########################################################
csv = read.csv("Complete nutritional values for the foods.csv", header=T)

#########################################################
table <- function(tableau,selected_foods){
  foods = length(selected_foods)
  row = 22 + foods  + 1 #constraints + selected food + objective function
  col = row + foods + 1
  #making the tableau
  mat <- matrix(0:0,row,col,byrow=T)
  s = 23 #serving size placeholder
  
  
  for(i in 1:foods){
    mat[1:11,i] = -1* as.numeric(c(csv$Calories[selected_foods[i]],csv$Cholesterol.mg[selected_foods[i]],csv$Total_Fat.g[selected_foods[i]], csv$Sodium.mg[selected_foods[i]],csv$Carbohydrates.g[selected_foods[i]],csv$Dietary_Fiber.g[selected_foods[i]],csv$Protein.g[selected_foods[i]],csv$Vit_A.IU[selected_foods[i]],csv$Vit_C.IU[selected_foods[i]],csv$Calcium.mg[selected_foods[i]],csv$Iron[selected_foods[i]]))
    mat[12:22,i] = as.numeric(c(csv$Calories[selected_foods[i]],csv$Cholesterol.mg[selected_foods[i]],csv$Total_Fat.g[selected_foods[i]], csv$Sodium.mg[selected_foods[i]],csv$Carbohydrates.g[selected_foods[i]],csv$Dietary_Fiber.g[selected_foods[i]],csv$Protein.g[selected_foods[i]],csv$Vit_A.IU[selected_foods[i]],csv$Vit_C.IU[selected_foods[i]],csv$Calcium.mg[selected_foods[i]],csv$Iron[selected_foods[i]]))
    mat[s,i] = 1 #serving
    mat[row,i] = as.numeric(gsub("\\$", "", (csv$Price.Serving[selected_foods[i]]))) #for price per serving
    
    mat[12:22,col] = c(2250,300,65,2400,300,100,100,50000,20000,1600,30)
    mat[1:11,col] = c(-2000,0,0,0,0,-25,-50,-5000,-50,-800,-10)
    mat[s,col] = 10
    s = s+1 
  }
  slack = foods+1
  for(j in 1:row){
    #slack variables && z
    mat[j,slack] = 1
    slack = slack + 1 
  }
  
  cnames = c()
  #foods
  for(k in 1:foods){
    temp = ""
    cnames = c(cnames, paste0("x", k))
  }
  #slacks
  for(l in 1:(col-foods-1)){
    temp2 = ""
    cnames = c(cnames,paste0("s",l))
  }
  cnames = c(cnames,"Solution")
  colnames(mat) = cnames

  
  
  print(mat) #initial matrix  
  #print(mat[row,])
  #print("here")
  #loops until no negative value left in the solution column
  #c = any(mat[1:row-1,col] < 0, na.rm = T)
  c = TRUE
  while(c){
    
    c = FALSE
    feasible = FALSE
    for(i in 1:(row-1)){
      if(mat[i,col] < 0){
        PR = i
        # if pick one negative coefficient in row
        PC = which.min(mat[PR,1:(col-1)])
        
        if (mat[PR,PC] < 0) {
          feasible = TRUE
          PE = mat[PR,PC]
          #print(m)
          
          #normalize
          mat[PR,] = mat[PR,] / PE
          for(j in 1:row){
            if(PR == j){
              next
            }
            NR = mat[j,PC] * mat[PR,]
            mat[j,] = mat[j,] - NR
          }
          break
        }
      }
    }
    
    c = any(mat[1:row-1,col] < 0, na.rm = T)
    
    if(!feasible){
      return(FALSE)
    }
    #print(neg_ind)
    #print(m)
    #print(PE)
    #print(mat)
  }
  
  
  print(mat) #final matrix for simplex
  return(mat)
}
#########################################################


#checks if the last row of the tableau has a negative
#########################################################
neg <-function(tableau,row,col){
  for(i in 1:(col-1))
  {
    if(tableau[row,i]<0)
    {
      return(TRUE)
    }
  }
  return(FALSE)
}

#find pivot col index
#########################################################
index_pc <- function(tableau,row,col,lowest){
  for(i in 1:(col-1)){
    if(tableau[row,i]==lowest){
      pc_index = i
    }
  }
  return(pc_index)
}
#########################################################

#solution and Cost Breakdown by Food
#########################################################
solution <- function(selected_foods,csv,tableau){
  
  Food = c()
  Servings = c()
  Costs = c()
  final_food = c()
  for(w in 1:(length(selected_foods))){
    if(tableau[nrow(tableau),w] == 0){
      final_food = c(final_food,w)
    }
  }
  
  basic_sol_dataFrame = data.frame()
  #print(final_food)
  for (q in 1:length(final_food)) {
    v = final_food[q]
    one_loc = which(tableau[,v] == 1, arr.ind = T)
    #print(one_loc)
    serving = tableau[one_loc,ncol(tableau)]
    cost = serving*as.numeric(gsub("\\$", "", (csv$Price.Serving[selected_foods[v]]))) 
    
    Food = c(Food,csv$Foods[selected_foods[v]])
    Servings = c(Servings,serving)
    Costs = c(Costs,cost)
    Servings = round(Servings,2)
    Costs = round(Costs,2)
    basic_sol_dataFrame = data.frame(Food, Servings, Costs)
    #print(matrices)
  }
  return (basic_sol_dataFrame)
}
#########################################################


#Main Function
#########################################################
Simplex <- function(csv,selected_foods){
  tableau = table(csv,selected_foods) #make initial tableau
  if(!is.matrix(tableau)){
    return("Not feasible")
  }
  
  row = nrow(tableau)
  col = ncol(tableau)
  foods = length(selected_foods)
  
  matrices = list() #for phase 2 only
  matrices[[length(matrices)+1]] = tableau
  basic_Sol = list() #for phase 2 only
  basic_Sol[[length(basic_Sol)+1]] = solution(selected_foods,csv,tableau)
  
  #checks if the last row still has a negative value
  h = neg(tableau,row,col)
  while(h){
    
    #find pivot column
    PC = index_pc(tableau, row,col, min(tableau[row,1:(col-1)]))
    #print(PC)
    
    #test ratio = soln/pivot col
    TR = tableau[1:row-1,col]/tableau[1:row-1,PC]
    #print(TR)
    
    #finding smallest positive value in TR
    smallest_positive_value = Inf  
    for (value in TR) {
      if (is.finite(value) && value > 0 && value < smallest_positive_value) {
        smallest_positive_value = value
      }
    }
    #print(smallest_positive_value)
    
    if (smallest_positive_value < Inf) {
      PE_index = which(TR == smallest_positive_value)[1]  #for pivot row
      PE = tableau[PE_index, PC]  #getting the pivot element
    }
    
    #normalize (nPR)
    tableau[PE_index,] = tableau[PE_index,]/PE
    
    #print(PE_index)
    #elimination 
    for(i in 1:row){
      if(i==PE_index){
        next
      }else{
        C = tableau[i,PC]
        #Rn = Rn - npr*C
        tableau[i,] = tableau[i,] - (tableau[PE_index,]*C)
      }
      #print(tableau)
    }
    #print(tableau)
    matrices[[length(matrices)+1]] = tableau
    basic_Sol[[length(basic_Sol)+1]] = solution(selected_foods,csv,tableau)
    
    h = neg(tableau,row,col)
    #not feasible
    
  }
  
  z = round(-1*tableau[row,col], 2)
  opt_cost = ""
  opt_cost = paste0("The cost of this optimal diet is $", z, " per day.")
  
  #final solution
  sol_dataFrame = solution(selected_foods,csv,tableau)
  
  #food choices
  choices = c()
  for (g in 1:length(selected_foods)){
    choices = c(choices,csv$Foods[selected_foods[g]])
  }
  results = list( food_choices = choices, opt_cost = opt_cost, opt_menu = sol_dataFrame, matr_iter = matrices, basic_sol_iter = basic_Sol)
  #for checking
  print("===SIMPLEX RESULTS===")
  print(results)
  return (results)
}
#########################################################
#Checkers
#supposed to be vector containing the index of food items
#selected =c(18,19,20,8,9,7,1) #csv$Foods[1:20]
#selected = c(1:20)
#selected = c(1:64)
#selected = c(2,4,5,7,8,9)
#print(selected)
#print(table(csv,selected))


#tableau = table(csv,selected)
#print(tableau)

#Simplex(tableau, selected)
#Simplex(csv,selected)

#end
# References:
#   Simplex: https://www.ux1.eiu.edu/~cfcem2/simplex-2.pdf 
