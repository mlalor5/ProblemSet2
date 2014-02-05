# Maggie Lalor
# PS 02 Due 02/06/2014

##1) Calculating Violations
# Two ways of testing violations of Benford’s law are proposed below. Let Xi represent the observed
#proportional frequency of the integer i in observed vote totals.
# Leemis' m statistic and Cho-Gains' d

#write a function to calculate these statistics:
#input: (i) a matrix or vector of election returns and options(s) for m,d, both
# output: list containing the results, including full digit distribution

#Calc Function
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
  #Make sure no zeros? IS THIS NEEDED?
  
  #Calculate stats
  if (stats %in% c("both","leemis")) { #If Leemis or both
    vals <- rep(0, length(output$data))
    for (i in 1:length(output$data)) { #all potential Leemis stats
      vals[i] <- abs(output$data[[i]] - log10( 1+ (1/as.numeric(names(output$data[i]))) ))
    }
    output$leemis <- max(vals) #Select max
  }

  if (stats %in% c("both","chogaines")) { #If Cho-Gaines or both
    val <- 0
    for (i in 1:length(output$data)) {
      val <- val + (output$data[[i]] - log10(1+(1/as.numeric(names(output$data[i])))))^2 #distance is sum
    }
    output$chogaines <- sqrt(val) #Cho-Gaines is squareroot of sum
  } 
  return(output)
}

#Initial test
datamatrix<- matrix(seq(1,99,by=3), ncol=3) #Test matrix
datavector <- seq(1,33,3)
(outmatrix <- violationstats(datamatrix))
(outvec <- violationstats(datavector, stats="leemis"))   
##2) Critical Values



##3) Testing

##3-1) Develop a function that will unit test your function

##3-2) For each way that the function can fail this test, create a branch where you edit the code in
#some way to make the code fail to pass the unit testing

