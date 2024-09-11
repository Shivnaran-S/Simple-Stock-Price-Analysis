library(readr)      # For reading the data
library(reshape2)   # For HeatMap
library(lubridate)  # For ts (time series) function
library(ggplot2)    # For heatmap
customised_column_names<-c("Date","Company1","Company2","Company3","Company4","Company5","Company6","Company7","Company8","Company9","Company10")
stock_data <- read.table("D:/Naran/Sem 3/AS & RP/R package/caseStudy6.txt", col.names = customised_column_names)
company_names <- customised_column_names[2:11]

### Using statistical software 
### 1. Making Histograms of the price series
customised_colors <- c("blue", "red", "green", "purple", "orange", "cyan","black","deeppink","gold","snow3")

par(mfrow = c(2, 5))
for(i in 1:10){
  company<-company_names[i]
  hist(stock_data[, company] , main = paste("Histogram of Company",i), xlab = paste("Price Series of Company",i) , ylab = "Frequency")
  abline(v=mean(stock_data[,company]))
  Sys.sleep(1)
}
par(mfrow = c(1, 1))

hist(stock_data$Company1 , main = paste("Histogram of all Companies"), xlab = paste("Price Series of Companies") , ylab = "Frequency", xlim<-c(min(stock_data[2:11]),max(stock_data[2:11])), ylim<-c(0,nrow(stock_data)), freq=TRUE ,col="wHITE")
for(i in 1:10){
  company<-company_names[i]
  hist(stock_data[, company] , col = customised_colors[i],add=TRUE)
  #Sys.sleep(1)
}
legend("topright", legend = company_names, fill = customised_colors,cex=0.75)

### 2. Time Plots
par(mfrow = c(2, 5))
for(i in 1:10){
  ts_object <- ts(stock_data[i+1], start = decimal_date(ymd("1988-01-04")),frequency = 300)
  plot(ts_object,main=paste("Time plot of Company",i),xlab="Date",ylab="Price Series")
  #Sys.sleep(0.5)
}
par(mfrow = c(1, 1))

ts.plot(stock_data[2:11],gpars = list(col = customised_colors))
legend("topleft", legend = company_names, fill = customised_colors,cex=0.32)

print("The Y-axis scales are not the same for all plots")
print("Advantage : Making all the scales same makes the analysis easier")
print("Disadvantage : Making the scales different make the analysis harder , might lose some fine-grained details in individual stock price movements, especially for companies with low price variability also it is difficult for identifying extreme movements ")

### 3 . Descriptive Statistics
##  (i)MEAN :
means<-c()
for(i in 1:10){
  x<-company_names[i]
  means[i]<-mean(stock_data[, x])
  #print(means[i])
}
cat("Company ",which.max(means)," is having the highest mean price\n")
cat("Company ",which.min(means)," is having the lowest mean price\n")
print("Plotting and comparing the histograms of companies with highest and lowest means")

min_max<-c(which.max(means),which.min(means))
hist(stock_data$Company1 , main = paste("Histogram of all Companies"), xlab = paste("Price Series of Companies") , ylab = "Frequency", xlim<-c(min(stock_data[2:11]),max(stock_data[2:11])), ylim<-c(0,nrow(stock_data)), freq=TRUE ,col="wHITE")
for(i in min_max){
  company<-company_names[i]
  hist(stock_data[, company] , col = customised_colors[i],add=TRUE)
  abline(v=mean(stock_data[,company]))
  Sys.sleep(1)
}
print(" The company with the higher mean is a better investment than the company with the lower mean depends on various factors, including risk tolerance, investment goals, and the specific industry or sector in which the company operates")
## (ii)VARIANCE :
variance<-c()
for(i in 1:10){
  x<-company_names[i]
  variance[i]<-var(stock_data[, x])
  #print(variance[i])
}
cat("Company ",which.max(variance)," is having the highest variance price\n")
cat("Company ",which.min(variance)," is having the lowest variance of price\n")

### CORRELATION : 
correlation_matrix<-list()
for(i in 1:9){
  correlation_matrix[[i]]<-1:(10-i)
  }

cat("Here the some companies which can be used for diversification")
for(i in 1:9){
  x<-company_names[i]
  position <- i+1
  for(j in 1:(10-i)){
    y<-company_names[position]
    position<-position+1
    correlation_matrix[[i]][j] <- cor(stock_data[, x], stock_data[, y])
    if(correlation_matrix[[i]][j]<(-0.1)){
      cat("For the ",x," and ",y," the correlation value is : ",correlation_matrix[[i]][j],"\n")
    }
    #cat("For the ",x," and ",y," the correlation value is : ",correlation_matrix[[i]][j],"\n")
    #plot(stock_data[, x], stock_data[, y], main = "Scatter Plot", xlab = x, ylab = y)
    #Sys.sleep(0.5)
  }
}
# Display the correlation coefficient on the plot
#text(mean(stock_data$Company1), mean(stock_data$Company2), labels = paste("Correlation:", round(correlation, 2)))

# Here we are calculating the correlation matrix for stock prices (excluding the 'Date' column)
correlation_matrice <- cor(stock_data[, -1])

# Now a heatmap of the correlation matrix is plotted
ggplot(data = melt(correlation_matrice), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

###ANOVA
#HYPOTHESIS: U1=U2=U3=U4=...=Un
#CALCULATING TJ's and T
tjs<-c()
for(i in 2:11){
  tjs[i-1]<-sum(stock_data[i])
}
tjs
t<-sum(tjs)

#MSR & SSR OF STOCKS, W/ DF=10-1=9
df_r<-ncol(stock_data)-2
ssr<-sum(tjs^2)/10-(t^2/(312*10))
msr<-ssr/df_r

#degree of freedom of total
df_t<-((nrow(stock_data))*(ncol(stock_data)-1))-1
totmat<-as.matrix(stock_data[2:11])
totmat<-totmat^2

#calculating SST & MST
sst<-sum(totmat)-(t^2/312*10)
mst<-sst/df_t

#CALCULATING MSE AND DF FOR ERRORS
mse<-mst-msr
df_e<-df_t-df_r

#calculating F-ratio for comparison
ftest<-msr/mse

#F-CRITICAL
alpha<-0.05
crit_value<-qf(1-(alpha/2),df_r,df_e)

#ANALYZING HYPOTHESIS
if(ftest>crit_value){
  paste("REJECT HYPOTHESIS")
}else{
  paste("FAILED TO REJECT HYPOTHESIS")
}

### FORECAST 
library(vars)

stock_prices <- stock_data[, -1]
n_companies <- ncol(stock_prices)
stock_prices_ts <- ts(stock_prices)
var_model <- VAR(stock_prices_ts, p = 1, type = "none")

from_date <- as.Date("29.03.1989", format = "%d.%m.%Y")
to_date <- as.Date("08.04.1989", format = "%d.%m.%Y")

future_dates <- data.frame(Date = seq(from_date, to_date,length.out=10))
future_data <- tail(stock_prices, 1)
forecast <- predict(var_model, n.ahead = 10, newdata = future_data)
print(forecast)

