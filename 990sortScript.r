#Kasey Schoeff
#10/10/20
#I split the data into two parts, Some of the companies had negative revenue I think to avoid being taxed they invested a bunch of money or falsified losses im not sure
#Correlation between negative revenue and investment income
#Analyze the companies that submitted a 990 tax form in 2017 and 2016. Find correlations between the revenue of the company and the amount of benefits paid to employees. Correlation between received grants and total revenue. Correlation between grants paid and toatal revenue. 
library(xlsx)
library(readxl)
irs990_main_2017 <- read_excel("C:/Users/kasey/Downloads/irs990_main_2017.xlsx")
averagePriorCurrent <- function(x, y){
  (x+y)/2
}
numOfCompaniesGreater <- 0 #initiate variable for function that holds the size for arrays
arrOfCompaniesGreater <- c[] #array that gathers and holds the data for the input above average
arrOfCompaniesGreaterCp <- c[] #array that gathers and holds the data for the inputs counterpart above average
listOfReturns <- c() #my intention was to return a matrix to be able to return all 3 of the above things in the same function I was unable to figure this ouot
numOfCompaniesAboveAverageRevenue <- function(col, av, size, cp){
  for(k in 1:size){ #loops through size inputed always is the num of rows in excel sheet
    if(col[k] > av){ #if the value is above average
      numOfCompaniesGreater = numOfCompaniesGreater + 1 #this value is returned in this funciton
      arrOfCompaniesGreater[numOfCompaniesGreater] <- col[k] 
      arrOfCompaniesGreaterCp[numOfCompaniesGreater] <- cp[k]
    }
  }
  plot(arrOfCompaniesGreater, arrOfCompaniesGreaterCp, #plot function that plots the correlation between the data inputted and the counterpart
       xlab = "Revenue",
       ylab = "Salaries Paid")
  abline(lm(arrOfCompaniesGreaterCp~arrOfCompaniesGreater), col="red") #red regression line to show the trend
  return(numOfCompaniesGreater)
}
numOfCompaniesAboveAverage <- function(col, av, size, cp){ #same function as the function above just returns a different part of the function because I was unable to figure returning and accessing a matrix
  for(k in 1:size){
    if(col[k] > av){#if the value is above average
      numOfCompaniesGreater = numOfCompaniesGreater + 1
      arrOfCompaniesGreater[numOfCompaniesGreater] <- col[k] #this is the data returned
      arrOfCompaniesGreaterCp[numOfCompaniesGreater] <- cp[k]
    }
  }
  plot(arrOfCompaniesGreater, arrOfCompaniesGreaterCp,
       xlab = "Revenue",
       ylab = "Salaries Paid")
  abline(lm(arrOfCompaniesGreaterCp~arrOfCompaniesGreater), col="red")
  return(numOfCompaniesGreater)
}
arrOfCompaniesAboveAverageCp <- function(col, av, size, cp){ #same function as the function above just returns a different part of the function because I was unable to figure returning and accessing a matrix
  for(k in 1:size){
    if(col[k] > av){#if the value is above average
      numOfCompaniesGreater = numOfCompaniesGreater + 1
      arrOfCompaniesGreater[numOfCompaniesGreater] <- col[k]
      arrOfCompaniesGreaterCp[numOfCompaniesGreater] <- cp[k] #this value is returned in the function
    }
  }
  return(arrOfCompaniesGreaterCp[])
}

numOfRows = nrow(irs990_main_2017)
print(numOfRows)
x <- mean(irs990_main_2017$`Current year || Investment Income`); #gets the mean of the column investment income to use in function
print(x)
y <- mean(irs990_main_2017$`Current year || Total revenue`); #gets the mean of the column total revenue  to use in function
print(y)
i <- mean(irs990_main_2017$`Current year ||  Received Grants contributions`); #gets the mean of the column grants/contributions income to use in function
print(i)
p <- mean(irs990_main_2017$`End of year || Total liabilities`); #gets the mean of the column of total liabilities to use in function
print(p)
o <- mean(irs990_main_2017$`End of year  || Total assets`);#gets the mean of the column of total assets to use in function
print(o)
#calls and prints the number of companies above the average for investment
numOfCompaniesAboveAverageInvestment <- numOfCompaniesAboveAverageRevenue(irs990_main_2017$`Current year || Investment Income`, x, numOfRows, irs990_main_2017$`Current year || Salaries paid`)
print(numOfCompaniesAboveAverageInvestment)
#calls and prints the number of companies above the average for total revenue
numOfCompaniesAboveAverageTotal <- numOfCompaniesAboveAverageRevenue(irs990_main_2017$`Current year || Total revenue`, y, numOfRows, irs990_main_2017$`Current year || Salaries paid`)
print(numOfCompaniesAboveAverageTotal)
#calls and prints the number of companies above the average for grants
numOfCompaniesAboveAverageGrants <- numOfCompaniesAboveAverageRevenue(irs990_main_2017$`Current year ||  Received Grants contributions`, i, numOfRows, irs990_main_2017$`Current year || Benefits paid for members`)
print(numOfCompaniesAboveAverageGrants)
#calls and prints the number of companies above the average for liabilities
numOfCompaniesAboveAverageLiab <- numOfCompaniesAboveAverageRevenue(irs990_main_2017$`End of year || Total liabilities`, p, numOfRows, irs990_main_2017$`Current year || Salaries paid`)
print(numOfCompaniesAboveAverageLiab)
#calls and prints the number of companies above the average for liabilities with a counter part of total revenue
numOfCompaniesAboveAverageLiabVr <- numOfCompaniesAboveAverageRevenue(irs990_main_2017$`End of year || Total liabilities`, p, numOfRows, irs990_main_2017$`Current year || Total revenue`)
print(numOfCompaniesAboveAverageLiabVr)
#calls and prints the number of companies above the average for assets with counterpart of total revenue
numOfCompaniesAboveAverageAssetVr <- numOfCompaniesAboveAverageRevenue(irs990_main_2017$`End of year  || Total assets`, o, numOfRows, irs990_main_2017$`Current year || Total revenue`)
print(numOfCompaniesAboveAverageAssetVr)
#m <- (matrix('',nrow = 235799,ncol = 31))  This was my write function I am pretty sure it would work but since the data form is 230k entries I do no think my free version of R is allowed to go past like 9000 
#m[235799, 7] <- c(numOfCompaniesAboveAverageInvestment) If I did it wrong or it was an error on my end please let me know
#write.xlsx(x = m, file = "irs990_main_2017.xlsx",sheetName = "test", row.names = FALSE, col.names=FALSE) The error message I was getting "java.lang.OutOfMemoryError: GC overhead limit exceeded"