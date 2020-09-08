# ===== EXPLORATION DATA =====

library(ggplot2)

data.beta.alpha = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Time Series Analysis/Assignment/6th Assignment/Dataset/Data Beta Alpha -0.7.csv',
                           header = TRUE,
                           sep = ',')
View(data.beta.alpha)
str(data.beta.alpha)
summary(data.beta.alpha$Beta.One)
for (i in 1:4) {
  print(sd(data.beta.alpha[,i]))
}

modify.data = function(data.beta.alpha) {
  # Modify Data
  beta.null = data.beta.alpha$Beta.Null
  beta.one = data.beta.alpha$Beta.One
  alpha.null = data.beta.alpha$Alpha.Null
  alpha.one = data.beta.alpha$Alpha.One
  data.beta.alpha.mod = rbind(beta.null,
                              beta.one,
                              alpha.null,
                              alpha.one)
  data.beta.alpha.mod = as.data.frame(beta.null)
  data.beta.alpha.mod[1001:2000,1] = as.data.frame(beta.one)
  data.beta.alpha.mod[2001:3000,1] = as.data.frame(alpha.null)
  data.beta.alpha.mod[3001:4000,1] = as.data.frame(alpha.one)
  Estimator = c(rep('Beta Null', 1000),
                rep('Beta One', 1000),
                rep('Alpha Null', 1000),
                rep('Alpha One', 1000))
  data.beta.alpha.mod = cbind(data.beta.alpha.mod,
                              Estimator)
  
  data.null.mod = data.beta.alpha.mod[(data.beta.alpha.mod$Estimator == 'Beta Null' | data.beta.alpha.mod$Estimator == 'Alpha Null'),]
  data.null.mod$Estimator = as.factor(data.null.mod$Estimator)
  data.one.mod = data.beta.alpha.mod[data.beta.alpha.mod$Estimator == 'Beta One' | data.beta.alpha.mod$Estimator == 'Alpha One',]
  data.one.mod$Estimator = as.factor(data.one.mod$Estimator)
  
  list.return = list(data.null.mod,
                     data.one.mod)
  return(list.return)
}
data.null.mod = modify.data(data.beta.alpha)[[1]]
data.one.mod = modify.data(data.beta.alpha)[[2]]

# ===== HISTOGRAM
# BETA NULL
p = ggplot(data.beta.alpha)+
  geom_histogram(aes(Beta.Null),
                 binwidth = 0.14,
                 fill = 'red', # Color of object
                 col = 'white', # Color of line
                 alpha = 0.7) + # Transparency
  labs(title = 'Histogram of Beta Null Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Beta Null') +
  ylab('Count')
# ALPHA NULL
ggplot(data.beta.alpha)+
  geom_histogram(aes(Alpha.Null),
                 binwidth = 0.14,
                 fill = 'red', # Color of object
                 col = 'white', # Color of line
                 alpha = 0.7) + # Transparency
  labs(title = 'Histogram of Alpha Null Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Alpha Null') +
  ylab('Count')
# COMBINE BETA NULL AND ALPHA NULL
ggplot(data.null.mod)+
  geom_histogram(aes(x = beta.null,
                     fill = Estimator),
                 binwidth = 0.1,# Color of object
                 col = 'white', # Color of line
                 alpha = 0.7,
                 position = 'identity') + # Transparency
  labs(title = 'Histogram of Beta Null and Alpha Null Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Beta Null and Alpha Null') +
  ylab('Count')

# BETA ONE
ggplot(data.beta.alpha)+
  geom_histogram(aes(Beta.One),
                 binwidth = 0.008,
                 fill = 'blue', # Color of object
                 col = 'white', # Color of line
                 alpha = 0.7) + # Transparency
  labs(title = 'Histogram of Beta One Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Beta One') +
  ylab('Count')
# Alpha ONE
ggplot(data.beta.alpha)+
  geom_histogram(aes(Alpha.One),
                 binwidth = 0.008,
                 fill = 'blue', # Color of object
                 col = 'white', # Color of line
                 alpha = 0.7) + # Transparency
  labs(title = 'Error Correlation -0.1',
       subtitle = 'Binwidth = 0.8',
       caption = 'Forecasting Course') + 
  xlab('Alpha One') +
  ylab('Count')
# COMBINE BETA NULL AND ALPHA NULL
ggplot(data.one.mod)+
  geom_histogram(aes(x = beta.null,
                     fill = Estimator),
                 binwidth = 0.009,# Color of object
                 col = 'white', # Color of line
                 alpha = 0.7,
                 position = 'identity') + # Transparency
  labs(title = 'Histogram of Beta One and Alpha One Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') +
  xlab('Beta One and Alpha One') +
  ylab('Count')

# ===== DENSITY PLOT
# BETA NULL
ggplot(data.beta.alpha)+
  geom_density(aes(Beta.Null),
               fill = 'red', # Color of object
               col = 'white', # Color of line
               alpha = 0.7) + # Transparency
  labs(title = 'Density of Beta Null Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Beta Null') +
  ylab('Count')
# ALPHA NULL
ggplot(data.beta.alpha)+
  geom_density(aes(Alpha.Null),
               fill = 'red', # Color of object
               col = 'white', # Color of line
               alpha = 0.7) + # Transparency
  labs(title = 'Density of Alpha Null Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Alpha Null') +
  ylab('Count')
# COMBINE BETA NULL AND ALPHA NULL
ggplot(data.null.mod)+
  geom_density(aes(x = beta.null,
                   fill = Estimator),
               col = 'white', # Color of line
               alpha = 0.7,
               position = 'identity') + # Transparency
  geom_vline(xintercept = 1)+
  labs(title = 'Density of Beta Null and Alpha Null Values',
       subtitle = 'Error Correlation 0',
       caption = 'Forecasting Course') + 
  xlab('Beta Null and Alpha Null') +
  ylab('Count')

# BETA ONE
ggplot(data.beta.alpha)+
  geom_density(aes(Beta.One),
               fill = 'blue', # Color of object
               col = 'white', # Color of line
               alpha = 0.7) + # Transparency
  labs(title = 'Density of Beta One Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Beta One') +
  ylab('Count')
# Alpha ONE
ggplot(data.beta.alpha)+
  geom_density(aes(Alpha.One),
               fill = 'blue', # Color of object
               col = 'white', # Color of line
               alpha = 0.7) + # Transparency
  labs(title = 'Density of Alpha One Values',
       subtitle = 'Error Correlation -0.1',
       caption = 'Forecasting Course') + 
  xlab('Alpha One') +
  ylab('Count')
# COMBINE BETA ONE AND ALPHA ONE
ggplot(data.one.mod)+
  geom_density(aes(x = beta.null,
                   fill = Estimator),
               col = 'white', # Color of line
               alpha = 0.7,
               position = 'identity') + # Transparency
  geom_vline(xintercept = 0.8)+
  labs(title = 'Density of Beta One and Alpha One Values',
       subtitle = 'Error Correlation 0',
       caption = 'Forecasting Course') +
  xlab('Beta One and Alpha One') +
  ylab('Count')

# ===== BOXPLOT
ggplot(data.null.mod)+
  geom_boxplot(aes(y = beta.null,
                   group = Estimator,
                   fill = Estimator))+
  scale_x_discrete(limits = c(''))+
  labs(title = 'Boxplot of Beta Null and Alpha Null Values',
       caption = 'Forecasting Course') + 
  ylab('Beta Null and Alpha Null')
  
ggplot(data.one.mod)+
  geom_boxplot(aes(y = beta.null,
                   group = Estimator,
                   fill = Estimator))+
  scale_x_discrete(limits = c(''))+
  labs(title = 'Boxplot of Beta One and Alpha One Values',
       caption = 'Forecasting Course') + 
  ylab('Beta One and Alpha One')

# BETA AND ALPHA NULL
data.beta.alpha.sample = sample(x = data.beta.alpha$Alpha.Null,
                                size = 1000,
                                replace = FALSE)
mean.alpha.null = mean(data.beta.alpha.sample)
sd.alpha.null = sd(data.beta.alpha.sample)
integrand = function(x) {x*(1/sqrt(2*pi*(sd.alpha.null^2)))*exp(-0.5*((x-mean.alpha.null)/sd.alpha.null)^2)}
expected.value.alpha.null = integrate(f = integrand,
                                      lower = -Inf,
                                      upper = Inf)
mean.alpha.null = mean(data.beta.alpha$Alpha.Null)
sd.alpha.null = sd(data.beta.alpha$Alpha.Null)
integrand = function(x) {x*(1/sqrt(2*pi*(sd.beta.null^2)))*exp(-0.5*((x-mean.beta.null)/sd.beta.null)^2)}
expected.value.beta.null = integrate(f = integrand,
                                     lower = -Inf,
                                     upper = Inf)

# UNBIEASED ESTIMATOR
# Population
data.alpha.null.pop = data.beta.alpha$Alpha.Null
mean(data.alpha.null.pop)
# Samples
sampSize = 1000
sampChamp = sample(data.alpha.null.pop,
                   size = sampSize,
                   replace = TRUE)
mean(sampChamp)
sampDist = replicate(n = 10000,
                     expr = mean(sample(sampChamp)))
mean(sampDist)
