# Maggie Lalor
# PS 02 Due 02/06/2014

##1) Calculating Violations
# Two ways of testing violations of Benford’s law are proposed below. Let Xi represent the observed
#proportional frequency of the integer i in observed vote totals.
# Leemis' m statistic and Cho-Gains' d

#write a function to calculate these statistics:
#input: (i) a matrix or vector of election returns and options(s) for m,d, both
# output: list containing the results, including full digit distribution

##Calc Function
violationstats <- function(data, stats="both") { #default to calculate both
  if (!stats %in% c("both","leemis","chogaines")) { #Check that option is correct
    print("Error: incorrect stats option") 
    break
  }
  #Process dataset
  output <- list() #Output for later
  #Extract first digit
  extract <- function(x){
    return(as.numeric(substr(as.character(x),1,1)))
  }
  #First digit for each piece of data
  sigs <- sapply (data, extract)
  #Frequency calculation 
  digitfrequency <- table(sort(sigs))
  output$data <- as.list(digitfrequency) 
  #Change from frequency to proportion
  for (i in 1:length(output$data)) { 
    output$data[[i]] <- output$data[[i]]/length(data)
  }
  
  #Calculate stats
  if (stats %in% c("both","leemis")) { #If Leemis or both
    vals <- rep(0, length(output$data))
    for (i in 1:length(output$data)) { #all potential Leemis stats
      vals[i] <- abs(output$data[[i]] - log10( 1+ (1/as.numeric(names(output$data[i]))) ))
    }
    output$leemis <- max(vals)*sqrt(length(data)) #Select max * sqrt(n) per Murrow
  } # end L if

  if (stats %in% c("both","chogaines")) { #If Cho-Gaines or both
    val <- 0
    for (i in 1:length(output$data)) { #sum square of (actual proportion - benford predicted) 
      val <- val + (output$data[[i]] - log10(1+(1/as.numeric(names(output$data[i])))))^2 #distance is sum
    }
    output$chogaines <- sqrt(length(data))*sqrt(val) #Cho-Gaines is squareroot of sum * sqrt(n)
  } #end CG if
  return(output)
}

#Initial test - outputs ok
#datamatrix<- matrix(seq(1,99,by=3), ncol=3) #Test matrix
#datavector <- seq(1,33,3)
#(outmatrix <- violationstats(datamatrix))
#(outvec <- violationstats(datavector, stats="leemis"))   
outbaddist <- violationstats(notmetsample) #Run for non-Benford data



##2) Critical Values
#names(outmatrix)
#violoutput <- outmatrix #Test, has both

#Mcrit <- list("0.10" = 0.851,"0.05" = 0.967,"0.01" = 1.212) #Listed Leemis crits
#Dcrit <- list("0.10" = 1.212,"0.05" = 1.330,"0.01" = 1.569) #Listed CG crits

## PRINT FUNCTION
print.benfords <- function(violoutput) {
  #Reject Null of no fraud if reach critical values:
  cat("--------------------------------------------------\n" )
  if ("leemis" %in% names(violoutput)) { #Print Leemis' value
    stars <- ""
    if (violoutput$leemis[1] > 1.212 ) { #if above .01 crit
      stars <-"***"
    } else if (violoutput$leemis[1] > 0.967 ) { # if above .05 crit
      stars <-"**"
    } else if (violoutput$leemis[1] > 0.851 ) { # if above .10 crit
      stars <-"*"
    }
    #print line
    cat("Leemis' m:", signif(violoutput$leemis[1], digits=4),stars,"\n") #3 sig digits given crits
  } #end Leemis if
  
  if ("chogaines" %in% names(violoutput)) { #Print Cho-Gaines' value
    stars <- ""
    if (violoutput$chogaines[1] > 1.569 ) { #if above .01 crit
      stars <-"***"
    } else if (violoutput$chogaines[1] > 1.330 ) { # if above .05 crit
      stars <-"**"
    } else if (violoutput$chogaines[1] > 1.212 ) { # if above .10 crit
      stars <-"*"
    }
    #print line
    cat("Cho-Gaines' d:", signif(violoutput$chogaines[1], digits=4),stars,"\n") #3 sig digits given crits  
  } #end Cho-Gaines if
  
  # Print legend
  cat("Sig code: \"*\" = .10, \"**\" = .05, \"***\" = .01\n" )
  cat("--------------------------------------------------\n" )  
} #end function

#test - output ok
#print.benfords(violoutput) #both
#print.benfords(outvec) #just leemis
#print.benfords(outbaddist) #non-Benford dataset


##3) Testing

#function to unit test function

##FUNCTION##
#truetestdist is true Beneford distribution for dataset inputdata
#truelem  is true Leemis' m for dataset inputdata
#truecg is true Cho-Gaines' d for dataset inputdata
test.funct <- function(inputdata, truetestdist, truelem, truecg) { #will test both stats values
  Pass <- "TRUE"
  testout <- violationstats(inputdata)
  
  #Compare distributions
  #Make sure lengths are the same
  if (length(testout$data) != length(truetestdist)) {
    print("Distribution Unit Test Failed")
    Pass <- "FALSE"
  }
  #Compare each proportion, don't check names so input doesn't have to be named list
  for(i in 1:length(testout$data)) {
    if (signif(testout$data[[i]], digits = 3) != signif(truetestdist[[i]], digits = 3)) {
      print("Distribution Unit Test Failed")
     Pass <- "FALSE"
    }
  }
  
  #Compare Leemis Stat
  if (signif(testout$leemis, digits = 3) != signif(truelem, digits = 3)) {
    print("Leemis' Unit Test Failed")
    Pass <- "FALSE"
  }
  
  #Compare Cho-Gaines Stat
  if (signif(testout$chogaines, digits = 3) != signif(truecg, digits = 3)) {
    print("Cho-Gaines' Unit Test Failed")
    Pass <- "FALSE"
  }
  return(Pass)
} 
  

##TESTING##

#data where Benford's law is met
library(BenfordTests)
set.seed(25)
#random sample satisfying Benford's law
benfordsample <- rbenf(25000)
#sample NOT satisfying Benford's law 
notmetsample <- c(1,2,3,4,rep(5,5),6,7, rep(9,3^9)) #most values being high int opposite of Benford

#Compare my function to results
results <- violationstats(benfordsample)

#From Beneford Package
firstdig <- signifd(benfordsample) #first digits function
#Distribution calculation 
digitfrequency <- table(sort(firstdig)) #frequencies
truetestdist <- (digitfrequency/length(firstdig)) #change to proportion
#truetestdist == results$data #TRUE
#Stat Tests

#Calculate d: sqrt(n)* sqrt (sum( observed frequencies - benford predicted)^2 )
val <- 0
for (i in 1:length(truetestdist)) { #all potential Leemis stats - do for incase some digits missing
  val<- val + (truetestdist[[i]] -pbenf(digits=1)[[as.numeric(names(truetestdist[i]))]])^2
}
truecg <- (sqrt(length(notmetsample))*sqrt(val)) 
#same as edist.benftest in BenfordTests package
#results$chogaines == truecg #TRUE

#Calcualte m: sqrt(n) Max abs(observed frequencies - benford predicted)
vals <- rep(0, length(truetestdist))
for (i in 1:length(truetestdist)) { #all potential Leemis stats - do for incase some digits missing
  vals[i] <- abs(truetestdist[[i]] -pbenf(digits=1)[[as.numeric(names(truetestdist[i]))]])
}
truelem <- max(vals)*sqrt(length(notmetsample)) 
}  
#same as edist.benftest in BenfordTests package
#results$leemis == truelem #TRUE

#use your functions above to compare to the “truth” for the digit distributions and two test statistics

#Test Function: Beneford data
resultsmet <- test.funct(benfordsample, truetestdist, truelem, truecg)
#resultsmet #TRUE when proper data for variables truetestdist, truelem, truecg

#Test Function: Non-Benford data
resultsmet <- test.funct(notmetsample, truetestdist, truelem, truecg)
#resultsmet #TRUE when proper data for variables truetestdist, truelem, truecg





##3) Part 2 For each way that the function can fail this test, create a 
#branch where you edit the code in some way to make the code fail to pass the unit testing
#MESS UP CODE

