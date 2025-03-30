#Joseffe Rose Ong
#CMSC 150 - Final Project
#B-5L
#Polynomial Regression

#########################################################
PolynomialRegression <- function(m,z,est){
  #m is the degree
  #z is the csv file
  #est is the x-value
  
  #to access independent(x) and dependent(y) data 
  x = z[,1]
  y = z[,2]

  #size r(row), col(column)
  r = m+1
  col = m+2
  
  #limitations: different size or the degree is less than 1
  if(length(x)!=length(y) || m<1){
    return (NA)
  }
  #matrix
  augcoeffmatrix <- matrix(0:0,r,col,byrow = TRUE)
  cnames = c()
  for(k in 1:(col-1)){
    temp = ""
    cnames = c(cnames, paste0("x", k))
  }
  cnames = c(cnames, "RHS")
  colnames(augcoeffmatrix) = cnames
  for (i in 1:(m+1)){ #row
    for (j in 1:(m+2)){ #col
      if(j == (m+2)){ #for RHS column
        augcoeffmatrix[i,j] = sum((x^(i-1)) * y) #summation of the vectors raised to corresponding degree multiplied to y-vector
      }else{ 
        augcoeffmatrix[i,j] = sum(x^(j+i-2)) #summation of the x-vectors raised to corresponding degree
      }
    }
  }
  
  #b = augcoeffmatrix #for original augcoeff matrix
  a = augcoeffmatrix #for Gauss-Jordan Elimination
  
  #Gauss-Jordan Elimination
  n = nrow(a)
  coeff = c()
  mat_iter = list()
  mat_iter[[1]] = a
  for(i in 1:n){
    if(i!=n){
      #getting the maximum
      maximum = max(abs(a[i:n,i]))
      PR1 = as.vector(which(abs(a)==maximum,arr.ind = T))

      #getting the pivot row
      PR = PR1[1]
      #print(PR)
      #checking if pivot row is in the main diagonal
      if(a[PR,i] == 0){
        return(NA)
      }else{
        #swapping
        tempo = a[PR,]
        a[PR,] = a[i,]
        a[i,] = tempo
      }
    }
    
    a[i,] = a[i,] / a[i,i]
    for(j in 1:n){
      if(i == j){
        next
      }
      NR = a[j,i] * a[i,]
      a[j,] = a[j,] - NR
    }
    mat_iter[[i+1]] = a
  }
  
  #matrix to vector format: coefficients
  coeff = as.vector(a[,(n+1)])
  
  #making the polynomial string
  poly = "function (x)"
  for (k in 1:(length(coeff))){
    if(k==1){ #to paste the constant value in the function string
      poly = paste(poly, coeff[k])
    }else{ #starts at index 2, coefficients of x variable with degree greater than 0
      poly = paste(poly, coeff[k], "* x ^", k-1) 
    }
    if(k!=length(coeff)){ #to separate terms
      poly = paste(poly,"+")
    }
  }
  #to execute the expression string->function
  poly_exp = eval(parse(text = poly))
  
  
  #labelled list
  results = list(coefficients = coeff, polynomial_function = poly_exp, estimate = poly_exp(est), polynomial_string = poly, mat_iter = mat_iter)
  #for checking
  print("===PR RESULTS===")
  print(results)
  return (results)
}
#########################################################
#Checkers
#csv = read.csv("input_Regression.csv", header=F)
#print(PolynomialRegression(4,csv,150))

#end
# References:
#   PR: Exercise 6
