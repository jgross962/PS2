#1 Benfords Law
BenfordsLaw = function(X,control=3){ # By default, print both statistics
  
  SigDigit = substr(X,1,1)
  counts = (table(SigDigit))
  freq = counts/(sum(counts))
  foundDigits = rownames(counts)
  foundDigits = as.numeric(foundDigits)
  mVector = freq-log10(1+1/foundDigits)
  m = max(mVector)
  d = sqrt(sum(mVector^2))
  
  hypothTest(m,d)
  
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

hypothTest = function(LemmisM,ChoGainD){
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

# Check Code
x = c(1598,2001,193,26,35,78)

# BenfordsLaw(as.numeric(1:9))
m = BenfordsLaw(1:9,3)
BenfordsLaw(x,1)


