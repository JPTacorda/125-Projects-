#Last Name      : TACORDA
#First Name     : JOSEPH PAULO
#Middle Name    : GUMABON
#Student Number : 2019-00060

rm(list=ls())
#### Question number 1 -----
#### A -----
#although my first code creates a dictionary stored in a list, this one uses while loops
#triangle is a function that uses two loops

#the first loop creates the "negative slope" of the "twisted" triangle 
  #it is possible by increasing the asterisk every single time it enters the loop
  #until it reaches h, the loop stops
triangle <- function(h){
  x=1
  while(x<=h){
    message(rep("* ",x))
    x=x+1 
  }
  
#the second loop creates the "positive slope" of the "twisted" triangle 
  #it is possible by decreasing the asterisk every single time it enters the loop
  #until it reaches one, the loop stops
  while(x!=1){
    x=x-1
    message(rep("* ", x-1))
  }
}
triangle(10) #don't try this at home hihihi

#### B -----
#this set of codes adds a limit to the function
#before entering the two while loops a while ago, it creates a condition.
#the conditions are
 
triangle <- function(h){
  #i. if h is an integer it will proceed to the next condition
  if(is.integer(h) == FALSE){
    stop("Please input integers from 2 to 10 only!")
    
  #ii. if h is an integer in [2,10] it will proceed to the next condition
  }else if(h<2L || h>10L){
    stop("Please input integers from 2 to 10 only!")
  
  #iii. if all conditions FALSE then it will proceed to triangle creation
  }else{
    x=1
    while(x<=h){
      message(rep("* ",x))
      x=x+1 
    }
    while(x!=1){
      x=x-1
      message(rep("* ", x-1))
    }
  }
}
triangle(1L)
triangle(2L)
triangle(3)
triangle(10L)
triangle(100L)


class(3)

#### C -----
#this will set a default value if there is no argument given in the function 
triangle <- function(h=5L){
  #i. if h is an integer it will proceed to the next condition
  if(is.integer(h) == FALSE){
    stop("Please input integers from 2 to 10 only!")
    
    #ii. if h is an integer in [2,10] it will proceed to the next condition
  }else if(h<2L || h>10L){
    stop("Please input integers from 2 to 10 only!")
    
    #iii. if all conditions FALSE then it will proceed to triangle creation
  }else{
    x=1
    while(x<=h){
      message(rep("* ",x))
      x=x+1 
    }
    while(x!=1){
      x=x-1
      message(rep("* ", x-1))
    }
  }
}
triangle()


#### Question number 2 -----
chisqtest <- function (data,alpha){
  #exp_data is the expected value of the given observed data 
  #by multiplying 0 to the observed data we create the same matrix as obs;
  #however, exp_data contains 0 for all values - placeholders.
  exp_data <- 0*obs_data
  
  #these two loops create the expected values for exp_data
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      exp_data[i,j] <- sum(data[i, ])*sum(data[ ,j])/sum(data)
    }
  }
  
  #the test statistic is stored in the variable chi - a computational formula to reduce errors.
  chi <- sum((data)^2/(exp_data)) - sum(data)
  
  #crit contains the critical value given the 
    #i. 1-alpha since a chi distribution is not symmetric 
    #ii. degrees of freedom (df) = (#_of_rows - 1)(#_of_columns - 1)
  crit <- qchisq(1-alpha,df=(nrow(data)-1)*(ncol(data)-1))
  
  #the if statement contains the non-rejection of null hypothesis since statistic is less than critical value 
  if(chi<crit){
    message("Since the test statsitic - ", round(chi,digits=4), " - does not exceed the critical value - ", 
              round(crit,digits=4), ". We do not reject the null hypothesis of independence at ", 
              alpha*100, "% level of significance.")
  
  #while the else contains the rejection of the null hypothesis since statistic is greater than critical value
  }else{
    message("Since the test statsitic - ", round(chi,digits=4), " - exceeds the critical value - ", 
              round(crit,digits=4), ". We reject the null hypothesis of independence at ", 
              alpha*100, "% level of significance.")
  }
}

#Before using the function chisqtest w/ arguments data and alpha
#a user must input their data here. NOTE! This is by column format! Be careful
obs_data <- matrix(c(90,15,82,12,68,27,76,30),nrow=2)

#a user must also declare an alpha value here
LOS <- 0.05

#all set for chi-square test for independence 
chisqtest(obs_data,LOS)




