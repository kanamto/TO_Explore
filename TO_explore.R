diamonds$vs <- rep((mtcars$vs == 1)[3:22], 2697) #Creates VS logic column in the diamonds dataset.

#Explore Function (df, # of bins represented by binwidth (NOT BIN SIZE), correlation threshold)
explore <- function(df,binsize,cor_thres) {
  require(ggplot2) #Requiring packagings that includes methods and attributes to be accessed below.
  require(stats)
  require(grid)
  require(gridExtra) #Includes grid.arrange method for displaying multiple plots on 1 page. 
  
  title = NULL #Initialize empty variable for list of LM titles. 
  r2 = NULL   #Initialize empty variable for list of R^2 values.
  correlation = NULL #Initialize empty variable for list of Pearson Correlation values.
  df_cor_thres = NULL #Initialize empty variable for dataframe of correlation values above the set threshold.
  
  #df = dataframe dataset  
  #binsize = binwidth variable
  #cor_thres = threshold for correlations to show
  numerics <- df[which(lapply(df, is.numeric) == TRUE)] #Assigns to numerics variable a dataframe containing the numeric attributes
  numname <- colnames(numerics) #Assigns to numname the list of variable names from the numerics dataframe
  
  nonnumerics <- df[which(lapply(df, is.numeric) == FALSE)] #Assigns to numerics variable dataframe containing the nonnumeric attributes
  nonnumname <- colnames(nonnumerics) #Assigns to numname the list of variable names from the numerics dataframe
  
  if (length(numerics) > 0) { #If there are any columns of data in numerics dataframe... 
    for (i in 1:length(numerics)) { #For each numeric parameter/attribute...
      for (j in 1:length(binsize)) { #For each each binsize listed...
        
        #Part 1: Generate Plots of Numeric Parameters Count and Density Histograms
        a<- ggplot(numerics , aes_string(numname[i])) + #Assign to ggplot numerics dataframe referencing the string value of the currently indexed column name
          geom_histogram(binwidth=binsize[j],  #Make histogram using indexed data column, setting binsize to currently indexed binsize in list
                         fill="blue") +  #Histogram columns are filled with blue. 
          geom_vline(xintercept = mean(numerics[[i]]),color="red") + #Draw red vertical line at the mean of the indexed column of data
          labs(title=paste(numname[i]," data count,"," binsize=",binsize[j])) #Title graph as indexed column name + data count, along with binsize value.
         
        b <- ggplot(numerics, aes_string(numname[i])) + #Assign to ggplot numerics dataframe referencing the string value of the currently indexed column name
          geom_histogram(aes(y=..density..),  #Create histogram based showing y-axis as a density of instances along x-axis.
                         binwidth = binsize[j], #Set binsize to currently indexed binsize in list
                         position="stack", #Set the graph to show data along x-axis stacked  
                         fill="blue") + #Stacked instances are filled with blue. 
          geom_vline(xintercept = mean(numerics[[i]]),color="red") + #Draw red vertical line at the mean of the indexed column of data
          labs(title=paste(numname[i]," data density")) #Title graph as indexed column name + data density, along with binsize value.
       
        grid.arrange(a, b, ncol=1,  nrow=2) #Plot the Count and Density Histograms on 1 plot for each indexed numerical attribute
      }
    }
    
    #Tabulate R^2 values for every pair of numeric variables
    #Prints table of summary statistics of each numerical variable 
    for (i in 1:length(numerics)) { #For each numeric parameter/attribute... 
      print(summary(numerics[i]))   #Part 4bi: Print a summary statistics table based on data under each attribute
      for (j in 2:length(numerics)) { #A 2nd for looop to create paired variables that..
        if (i != j) {                 #Creates the combinations of all paired attributes without repeats (by never letting i = j) 
          
          #Part 3: Calculate R-squared for each paired attribute. 
          title <- rbind(title, paste(numname[i],"-",numname[j])) #Update title list with next name of paired parameters. 
          z<- lm(numerics[[i]]~numerics[[j]]) #Assign LM output to variable z
          r2 <- rbind(r2, summary(z)$r.squared) #Update r2 list with r-squared extracted from the summary of variable z. 
          
          correlation <- rbind(correlation,cor(numerics[[i]], numerics[[j]], method =c("pearson"))) 
          #Update correlation list with calculation of Pearson Correlation Coeff based on current pair of attributes
        }
      }
    }
    print(df_r2 <- data.frame("Paired_Variable" = title, "R.squared" = r2)) #Part 4bii: dataframes the R^2 data.
    df_cor <- data.frame("Paired_Variable" = title, "Pearson_Correlation" = correlation) #Dataframes Pearson Correlation data. 
    
    #Part 4biii: Create dataframe of correlation values above correlation threshold.
    for (i in 1:length(df_cor[,1])) { #For each calculated Pearson correlation...
      if (abs(df_cor[i,2])>cor_thres) { #If the absolute value of that value is greater than the correlation threshold...
        df_cor_thres <- rbind(df_cor_thres,df_cor[i,]) #Update dataframe entire row of data from df_cor dataframe calculated above.
      }}
    print(df_cor_thres) #Print the resultant dataframe.
  }
  
  #Part 2: Generate Plots of Non-Numeric Parameters Count Histogram
  if (length(nonnumerics)>0) { #If there are any columns of data in nonnumerics dataframe... 
    for (i in 1:length(nonnumerics)) { #For each nonnumeric parameter..
      c <- ggplot(nonnumerics, aes_string(nonnumname[i])) + #Assign to ggplot nonnumerics dataframe referencing the string value of the currently indexed column name
        geom_bar(color="gray") + #Create bar graph of the referenced data with gray bars 
        labs(title=paste(nonnumname[i]," data count")) #Title graph as indexed column name + data count.
      plot(c) #Plot the Count Histogram for each indexed non-numerical attribute
    }
  }
} #Function closing bracket

#Part 5: run code (currently commented out) below
#explore(diamonds,c(5,20,50), 0.25)
#explore(mtcars,c(5,20,50), 0.25)
  