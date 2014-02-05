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
  } # end L if

  if (stats %in% c("both","chogaines")) { #If Cho-Gaines or both
    val <- 0
    for (i in 1:length(output$data)) {
      val <- val + (output$data[[i]] - log10(1+(1/as.numeric(names(output$data[i])))))^2 #distance is sum
    }
    output$chogaines <- sqrt(val) #Cho-Gaines is squareroot of sum
  } #end CG if
  return(output)
}

#Initial test - outputs ok
#datamatrix<- matrix(seq(1,99,by=3), ncol=3) #Test matrix
#datavector <- seq(1,33,3)
#(outmatrix <- violationstats(datamatrix))
#(outvec <- violationstats(datavector, stats="leemis"))   

##2) Critical Values
names(outmatrix)
violoutput <- outmatrix #Test, has both
print.benfords <- function(violoutput) {
  #Reject Null of no fraud if reach critical values:
  #Mcrit <- list("0.10" = 0.851,"0.05" = 0.967,"0.01" = 1.212) #Listed Leemis crits
  #Dcrit <- list("0.10" = 1.212,"0.05" = 1.330,"0.01" = 1.569) #Listed CG crits
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

##3) Testing

##3-1) Develop a function that will unit test your function

##3-2) For each way that the function can fail this test, create a branch where you edit the code in
#some way to make the code fail to pass the unit testing

