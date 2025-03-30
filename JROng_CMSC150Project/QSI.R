#Joseffe Rose Ong
#CMSC 150 - Final Project
#B-5L
#Quadratic Spline Interpolation

#########################################################
GaussJordanMethod <- function(mat){
  
  a = mat
  n = nrow(a)
  mat_iter = list()
  mat_iter[[1]] = a 
  for(i in 1:n){
    if(i!=n){
      #getting the pivot row (revised from exer 4) 
      #print((abs(a[i:n, i]))) #abs value of that column
      #print(which.max(abs(a[i:n, i])))  #index of the max 
      #print(i-1) #to get the current index
      #print(which.max(abs(a[i:n, i])) + (i - 1))
      PR = which.max(abs(a[i:n, i])) + (i - 1)
      
      #checking if pivot row is in the main diagonal
      if(a[PR,i] == 0){
        return(NA)
      }
      #swapping
      tempo = a[PR,]
      a[PR,] = a[i,]
      a[i,] = tempo
      #print(a)
    }
    
    a[i,] = a[i,] / a[i,i]
    for(j in 1:n){
      if(i == j){
        next
      }
      NR = a[j,i] * a[i,]
      a[j,] = a[j,] - NR
    }
    #print(a)
    mat_iter[[i+1]] = a
  }
  b = a[,(n+1)]
  #print(b)
  #labelled list containing augmented coefficient matrix and  the solution vector
  to_return = list(mat_iter = mat_iter, augcoeffmatrix = a, solution = b)
  
  return(to_return)
  
}
#########################################################


#Main Function
#########################################################
QSI <- function(mat, z){
  #mat is the csv file
  #z is the point estimate
  
  #to access the x and f(x)->y
  x = mat[,1]
  y = mat[,2]
  n = length(x)
  RHS = c() #to bind later
  mat <- matrix(0:0,(3*(n-1)),(3*(n-1)),byrow = TRUE) 
  
  
  #quadratic splines pass thru two consecutive data points
  #placeholders
  l=1 #start col
  w=3 #end col
  r = 1 #row
  for(i in 2:n){
    #first eq
    mat[r,l:w] = c(x[i-1]^2,x[i-1],1) #coefficients of: a, b, c 
    #second  eq
    mat[r+1,l:w] = c(x[i]^2,x[i],1)
    
    RHS = c(RHS,y[i-1])
    RHS = c(RHS,y[i])
    r = r+2
    l = l+3
    w = w+3
    
  }
  
  #for interior knots 
  #placeholders 
  l = 1 #mag start ulit sa first column 
  w = 5 #end col
  for(k in 2:(n-1)){
    mat[r,l:w] = c(x[k]*2,1,0,-(x[k]*2), -1) #coefficients of: +- a&b
    r = r+1
    l = l+3
    w = w+3
    RHS = c(RHS, 0)
  }
  
  #assuming first spline is linear 
  mat[3*(n-1),1] = 1 #last row first col of matrix
  RHS = c(RHS,0)
  
  #matrix w/ solution column
  mat_RHS = cbind(mat,RHS)
  
  #print(mat_RHS)
  
  GJ = GaussJordanMethod(mat_RHS) #matrix and solution
  solution = GJ$solution #to get the solution
  
  #creating the functions
  j=1
  fin_Fxn = list()
  fin_Fxn_str = c()
  for(i in 1:(n-1))
  {
    fxn = "function (x) "
    fxn = paste0(fxn,solution[j], "*x^2 + ", solution[j+1], "*x + ",solution[j+2])
    fxn_exp = eval(parse(text = fxn))
    fin_Fxn_str = c(fin_Fxn_str,fxn)
    fin_Fxn = append(fin_Fxn,fxn_exp)
    j = j+3
  }
  
  
  #checker
  #print(fin_Fxn)
  
  #finding the correct interval/fxn of the given point estimate
  index = 1
  for (i in 1:n) {
    if (z >= x[i] && z <= x[i+1]) {
      index = i
      break
    }
  }
  #print(index)
  #print(length(fin_Fxn_str))
  #interval of point estimate
  interval = ""
  interval = paste0(interval,x[index]," <= x <= ", x[index+1])
  
  #evaluating the correct fxn w/ the given point estimate
  ans = fin_Fxn[[index]](z)
  #print(z)
  #print(interval)
  #print(ans)
  
  results = list(fxns_per_interval = fin_Fxn_str, x_interval = interval, Correct_fxn = fin_Fxn_str[[index]], estimate = ans, mat_iter = GJ$mat_iter)
  #for checking
  print("===QSI RESULTS===")
  print(results)
  return (results)
}
#########################################################
#Checkers
#x = c(0, 10,15, 20, 22.5, 30)
#y = c(0, 227.04, 362.78, 517.35, 602.97, 901.67)
#print(table(x,y))

#x = c(3.0,4.5,7.0,9.0)
#y = c(2.5,1.0,2.5,0.5)
#z = 5

#z = 19
#csv = read.csv("input_QSI.csv", header=F)
#print(as.matrix(csv))

#QSI(csv,z)

#end
# References:
#   QSI: http://nmbooks.eng.usf.edu/ebooks/05inp_spline/inp_05_spline_300_quadratic_example.html
