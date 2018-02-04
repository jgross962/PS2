#Jonthan Gross
#435133
#Pol Sci 5625
#PS2
#Due Feb 6 2018

rm(list = ls())

# 1 Benfords Law
# Function to calcluate Leemis' m statistic, and Cho's Gains d given a vector of election returns.
# Also performs hypothesis tests at various significance levels based off calculated values
# Then returns statistical signifance of statistic from hypothesis test
# Inputs:
# @X vecotr or matrix of election returns
# @Control, determines which statistics should be returned. 1 indicates just Leemis' m statistic.
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
  
  # Determine Siginifance by calling hypoth test functions below
  alphaM = hypothTestLemmis(m) 
  alphad = hypothTestChoGains(d) 
  
  # Only return desired output, as per user specification
  if (control == 1){ # Just Lemmis
    return(c(m,alphaM))
  }
  if (control == 2){ # Just ChoGains
    return(c(d,alphad))
  }
  else{ # Both Stats
    return(c(c(m,alphaM),c(d,alphad)))
  }
 
}

# 2 Hypothesis Testing
# Perform hpothesis testing based off the Lemmis' m statistic
# Prints an output statement indicating conclusion and significance level. Also returns signficance level
# Inputs: 
# @LemmisM = lemmis' m statistic as a scalar numeric type (numeric/integer/etc.)
hypothTestLemmis = function(LemmisM){
  # Determine Conclusion and signifance level using values specified in assignment
  if(LemmisM>1.212){
    print("Reject Null Hypothesis at .01 Significance Level using Lemmis m")
    return(.01)
  }else if(LemmisM>.967){
    print("Reject Null Hypothesis at .05 Significance Level using Lemmis m")
    return(.05)
  }else if(LemmisM>.851){
    print("Reject Null Hypothesis at .10 Significance Level using Lemmis m")
    return(.1)
  }else{
    print("Fail to Reject Null Hypothesis using Lemmis m")
    return(1)
  }
}

# Perform hpothesis testing based off the Cho-Gains' d statistic
# Prints an output statement indicating conclusion and significance level. Also returns signficance level
# Inputs: 
# @ChoGainsD = Cho-Gains d statistic as a scalar numeric type (numeric/integer/etc.)
hypothTestChoGains = function(ChoGainD){
  # Determine Conclusion and signifance level using values specified in assignment
  if( ChoGainD>1.569){
    print("Reject Null Hypothesis at .01 Significance Level using Cho-Gains d")
    return(.01)
  }else if( ChoGainD>1.330){
    print("Reject Null Hypothesis at .05 Significance Level using Cho-Gains d")
    return(.05)
  }else if(ChoGainD>1.212){
    print("Reject Null Hypothesis at .10 Significance Level using Cho-Gains d")
    return(.1)
  }else{
    print("Fail to Reject Null Hypothesis using Cho-Gains d")
    return(1)
  }
}

# Part 3, crate Print Bedford's function
# Takes in Election results in matrix or vector form, applies Benfords 
# Law then prints a table containing  the statistic name, statistic, 
# signifcance level and legend option to specify desired output
# @ x= election results in matrix or vector form
# @ control = specify output; 1 = just Leemis' m; 2 = just Cho Gain's; else = both statistics
print.benfords = function(x, control = 3){
  temp = BenfordsLaw(x)
  statistic.value = c(temp[1],temp[3])
  sigLevel = c(temp[2],temp[4])
  #Recast Significance Level into Asterisk notation
  signifiance.level = NULL
  for (i in 1:length(sigLevel)){
    if (sigLevel[i] ==.01){
      signifiance.level[i] = "****"
    } else if (sigLevel[i] ==.05){
      signifiance.level[i] = "***"
    }else if (sigLevel[i] ==.10){
      signifiance.level[i] = "**"
    }else{
      signifiance.level[i] = "*"
    }
  }
  statNames = c('Lemmis\' m' , "Cho Gains\' d" )
  # Creae Data Frame Containing Necessary Information
  stats.table = data.frame(statistic.value,signifiance.level,row.names = statNames)
  
  legend = rbind("* = fail to reject the null hypothesis at 10% signifance level",
                 "** = reject the null hypothesis at 10% signifance level",
                 "*** = reject the null hypothesis at the 5% significance level",
                 "**** = reject the null hypothesis at the 1% signifance level ")
  
  return(list(stats.table,legend))
}


# Part 4. Create CSV from Print.Bedford Function
# Calls Print Bedford Function, applied functions defined above, then writes a csv to desired file path
# Inputs
# @x = election result vector or matrix
# @ Benfords.filePath = file path to write folder too
Benfords.writeCSV = function (x,Benfords.filePath){
  setwd(Benfords.filePath)
  doc.writeTo = file("BenfordsLawData.csv")
  benTable = print.benfords(x)
  sink(doc.writeTo)
  write.csv(benTable,doc.writeTo)
  close(doc.writeTo)
  
}




# Test Code
# BenfordsLaw(as.numeric(1:9))
m = BenfordsLaw(1:9,3)
x = c(1598,2001,193,26,35,78) #Try Vector
BenfordsLaw(x,1)
x2 = matrix(c(605,887,7991,123,115,1009,212,345,607)) #Try Matrix
BenfordsLaw(x2,2)
BenfordsLaw(as.vector(x2)) # Check that vector and matrix get same result

t = print.benfords(x)
t
Benfords.writeCSV(x,"C:/Users/Me/Desktop/5625")
