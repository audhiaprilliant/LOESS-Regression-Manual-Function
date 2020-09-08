# ===== AUTOCORRELATION ANALYSIS - TIME SERIES =====

generate.beta = function(ti,
                         rho,
                         repetition) {
  # VECTOR BETA AND ALPHA
  vector.beta.null = rep(x = 0,
                         len = repetition)
  vector.beta.one = rep(x = 0,
                        len = repetition)
  vector.alpha.null = rep(x = 0,
                          len = repetition)
  vector.alpha.one = rep(x = 0,
                         len = repetition)
  
  j = 1
  while (j < (repetition + 1)) {
    t = 1:ti
    number.data = length(t)
    error.t = rnorm(n = number.data,
                    mean = 0,
                    sd = 1)
    # MATRIX VECTOR Vt
    vector.vt = rep(x = 0,
                    len = number.data)
    # GENERATE VECTOR Vt
    vector.vt[1] = rho*5 + error.t[1]
    for (i in 2:30) {
      vector.vt[i] = rho*vector.vt[i-1] + error.t[i]
    }
    # GENERATE VECTOR Ut
    scalar.v = mean(vector.vt)
    vector.ut = vector.vt - scalar.v
    # GENERATE VECTOR Yt and Zt
    vector.yt = 1 + 0.8*t + vector.ut
    vector.zt = 1 + 0.8*t + error.t
    
    # REGRESSION WITH INTERCEPT AND SLOPE BETA
    model.beta = lm(formula = vector.yt ~ t)
    vector.beta.null[j] = as.numeric(model.beta$coefficients[1])
    vector.beta.one[j] = as.numeric(model.beta$coefficients[2])
    
    # REGRESSION WITH INTERCEPT AND SLOPE ALPHA
    model.alpha = lm(formula = vector.zt ~ t)
    vector.alpha.null[j] = as.numeric(model.alpha$coefficients[1])
    vector.alpha.one[j] = as.numeric(model.alpha$coefficients[2])
    
    # INDEX
    j = j + 1
  }
  
  # PRINT BETA AND ALPHA AS DATAFRAME
  beta.alpha = data.frame(vector.beta.null,
                          vector.beta.one,
                          vector.alpha.null,
                          vector.alpha.one)
  colnames(beta.alpha) = c('Beta Null',
                           'Beta One',
                           'Alpha Null',
                           'Alpha One')
  return(beta.alpha)
}

beta.alpha = generate.beta(ti = 30,
                           rho = -0.3,
                           repetition = 1000)
View(beta.alpha)
write.csv(x = beta.alpha,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Time Series Analysis/Assignment/6th Assignment/Dataset/Data Beta Alpha -0.3.csv',
          row.names = FALSE)
