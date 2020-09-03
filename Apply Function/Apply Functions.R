# ===== DIFERENCES BETWEEN APPLY(), LAPPLY(), SAPPLY(), AND TAPPLY() FUNCTION =====

# ===== NOTE
# The apply collection can be viewed as a substitute to the loop
# The purpose of apply() is primarily to avoid explicit uses of loop 
# constructs. They can be used for an input list, matrix or array and 
# apply a function. Any function can be passed into apply(). 

# ===== APPLY() FUNCTION
# What: We use apply() over a matrice
# --------------------------------------------------
# apply(X, MARGIN, FUN)
# Here:
# -x: an array or matrix
# -MARGIN:  take a value or range between 1 and 2 to define where to apply the function:
#   -MARGIN = 1`       : the manipulation is performed on rows
#   -MARGIN = 2`       : the manipulation is performed on columns
#   -MARGIN = c(1,2)`  : the manipulation is performed on rows and columns
# -FUN: tells which function to apply. Built functions like mean, median, sum, min, max and even user-defined functions can be applied>
# --------------------------------------------------
# Objective: The simplest example is to sum a matrice over all the columns.

m1 = matrix(data = c(1:10),
            nrow = 5, 
            ncol = 6)
a_m1 = apply(X = m1,
              MARGIN = 2, 
              FUN = sum) # Sum a matrice over all the columns

# ===== LAPPLY() FUNCTION
# What: l in lapply() stands for list. The difference between lapply() 
#       and apply() lies between the output return. The output of 
#       lapply() is a list. lapply() can be used for other objects 
#       like data frames and lists. 
# --------------------------------------------------
# lapply(X, FUN)
# Arguments:
#   -X    : A vector or an object
#   -FUN  : Function applied to each element of x	
# --------------------------------------------------
# Objective: A very easy example can be to change the string value of a 
#            matrix to lower case with tolower function.

movies = c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower = lapply(X = movies, 
                      FUN = tolower)
str(movies_lower)
# We can use unlist() to convert the list into a vector.
movies_lower = unlist(lapply(X = movies,
                             FUN = tolower))
str(movies_lower)

# ===== SAPPLY() FUNCTION
# What: sapply() function does the same jobs as lapply() function 
#       but returns a vector.
# --------------------------------------------------
# sapply(X, FUN)
# Arguments:
#   -X: A vector or an object
#   -FUN: Function applied to each element of x
# --------------------------------------------------
# Objective: We can measure the minimum speed and stopping distances 
#            of cars from the cars dataset.

dt = cars
lmn_cars = lapply(X = dt,
                  FUN = min)
smn_cars = sapply(X = dt,
                  FUN = min)

# ===== TAPPLY() FUNCTION
# What: The function tapply() computes a measure (mean, median, min, 
#       max, etc..) or a function for each factor variable in a vector.
# --------------------------------------------------
# tapply(X, INDEX, FUN = NULL)
# Arguments:
#   -X: An object, usually a vector
#   -INDEX: A list containing factor
#   -FUN: Function applied to each element of x
# --------------------------------------------------
# Objective: As a prior work, we can compute the median of the length 
#            for each species. tapply() is a quick way to perform this 
#            computation.

data(iris)
tapply(X = iris$Sepal.Width,
       INDEX = iris$Species,
       FUN = median)