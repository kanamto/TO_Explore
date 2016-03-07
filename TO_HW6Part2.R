#Part2.1

#Figure out how to generically extract all numeric columns 
#   from any data frame so the method you develop can be used on any data.
NumericColumns <-function(data) {       #Create function that take data variable (dataframe type)
    extracted <- data[{sapply(data,class) == "numeric"}] #Assigns to extracted the data with class "numeric" from 
    ##str(extracted)   #Show structure of extracted dataframe to confirm proper extraction. 
    ##Prof G: Need to return something here! I added the line below to return extracted
    return(extracted)
}


#Part 2.2 - Pearson Correlation Dataframe - Note that only calculates Pearson Correlation Coefficient for numeric values.
Pearson_calc <-function(data) { #Create a function that takes data variable (dataframe type)
  extracted <- data[{sapply(data,class) == "numeric"}] #Assign to extracted the data with class "numeric" from 
  title = NULL #Initialize empty variables (for vectors)
  correlation = NULL #Initialize empty variables (for vectors)
  for (i in 1:length(extracted)) { #For each index position i (starting at 1) up to the last indexed position
    for (j in {i+1}:length(extracted)) { #For each index position j = i+1 up to the last indexed position
      #The two for loops create combination function that does not repeat the pairing of two parameters
      if (j <= length(extracted)) { #if j is less than or equal to length of vector, then perform the following:
        title <- rbind(title, paste(colnames(extracted)[i],"-",colnames(extracted)[j])) 
        #Row binding the title variable that is a paste of parameters names at the current index positions of i and j (up to last index name). 
        correlation <- rbind(correlation,cor(extracted[[i]], extracted[[j]], method =c("pearson")))
        #Row binding the calculation correlation coeff into a column of results based on the current index positions of i and j (up to last index name). 
      }
    }
  }
  df <- data.frame("Paired_Parameters" = title, "Pearson_Correlations" = correlation) #Create dataframe using the two columns, title names and correlation values
  df #print dataframe
  
  ##Prof G: Same problem here, need to return something.
} #Call Pearson_calc function to see resultant dataframe of correlation coeff for numeric values in a dataframe.

#Part 2.3 Scatter Plot for Every Pair of Numeric Variables
scatter <- ggplot(Pearson_calc(diamonds), aes(Paired_Parameters, Pearson_Correlations)) + 
  #Assigns ggplot to use two columns of the dataframe resultant from 
  #Pearson Correlation Coeff calculation (in part 2 above): Paired Parameters and Correlation Coeff.
  geom_point() + #Point representations of the data (i.e., scatterplot)
  labs(list(title = "Pearson Correlation Coefficients")) #Create plot title.
  
##Prof G: Nothing got plotted.
