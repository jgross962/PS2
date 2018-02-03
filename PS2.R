#Jonthan Gross
#435133
#Pol Sci 5625
#PS2
#Due Feb 6 2018

rm(list = ls())

#1 Benfords Law
#Function to calcluate Leemis' m statistic, and Cho's Gains d given a vector of election returns.
#Also performs hypothesis tests at various significance levels based off calculated values
#Inputs:
#@X vecotr or matrix of election returns
#@Control, determines which statistics should be returned. 1 indicates just Leemis' m statistic.
#   2 indicates just Cho-Gains' d will be returned. Any other number will return both statistics. 
#   By default if no value is specificed, both will be returned.
BenfordsLaw = function(X,control=3){ # By default, print both statistics
  
  SigDigit = substr(X,1,1) #Just get first digit of each number
  counts = (table(SigDigit)) #Count occurances of each number
  freq = counts/(sum(counts)) #Calculate relative frequncy
  foundDigits = rownames(counts) #Identify which leading significant digits occur
  foundDigits = as.numeric(foundDigits) #Recast as numeric
  mVector = freq-log10(1+1/foundDigits) #Part 1 of Lemmis calculation
  m = max(mVector) # Get lemmis m by taking max above mVector
  d = sqrt(sum(mVector^2)) #Calculate Cho Gain's D
  
  hypothTest(m,d) # Determine Siginifance by calling hypoth test fn below
  
  # Only return desired output, as per user specification
  if (control == 1){
    return(m)
  }
  if (control == 2){
    return(d)
  }
  else{
    return(c(m,d))
  }
 
}

# 2 Hypothesis Testing
# Perform hpothesis testing based off the Lemmis' m statistic and the Cho Gains' d statistic.
# Prints an output statement indicating conclusion and significance level.
# Inputs: 
# @LemmisM = lemmis' m statistic as a scalar numeric type (numeric/integer/etc.)
# @ChoGainsD = Cho-Gain's D statistic as a scalar numeric type (numeric/integer/etc.)
hypothTest = function(LemmisM,ChoGainD){
  # Determine Conclusion and signifance level using values specified in assignment
  if(LemmisM>1.212 & ChoGainD>1.569){
    print("Reject Null Hypothesis at .01 Significance Level")
  }else if(LemmisM>.967 & ChoGainD>1.330){
    print("Reject Null Hypothesis at .05 Significance Level")
  }else if(LemmisM>.851 & ChoGainD>1.212){
    print("Reject Null Hypothesis at .10 Significance Level")
  }else{
    print("Fail to Reject Null Hypothesis")
  }
}


# BenfordsLaw(as.numeric(1:9))
m = BenfordsLaw(1:9,3)
# Check Code
x = c(1598,2001,193,26,35,78)
BenfordsLaw(x,1)
x2 = matrix(c(605,887,7991,123,115,1009,212,345,607))
BenfordsLaw(x,2)


