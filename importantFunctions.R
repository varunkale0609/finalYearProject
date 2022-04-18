# ************************************************
# discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCREET, ORDINAL}
# ************************************************
discreetNumeric<-function(dataset,field_types,bins_cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      
      scaled_column<-normalise(dataset[,field])
      
      
      #Generate the "bins_cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      
      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0
      
      #If the number of bins with less than 1% of the values is greater than the bins_cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>bins_cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL
      
      
    } #endif numeric types
  } #endof for
  return(field_types)
}

# ************************************************
# FieldTypes() :
#
# Test each field for NUMERIC or SYMBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
FieldTypes<-function(dataset){
  manualTypes <- data.frame()
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}

# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    #NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}


# ****************
# oneHotEncoding() :
#   Pre-processing method to convert appropriate 
#   categorical fields into binary representation
#
# INPUT       :   dataframe - dataset           - dataset to one hot encode
#                 vector    - fieldsForEncoding -  
#
# OUTPUT      :   Encoded fields
# ****************
oneHotEncoding<-function(dataset,fieldsForEncoding){
  # Combine input fields for encoding
  stringToFormulate <- substring(paste(" + ", fieldsForEncoding, sep = "", collapse = ""), 4)
  
  OHEFormula <- as.formula(paste("~",stringToFormulate))
  
  # One hot encode fields listed in function
  dmy <<- dummyVars(OHEFormula, data = dataset)
  trsf<- data.frame(predict(dmy, newdata = dataSetToUse_02[,which(field_types==TYPE_SYMBOLIC)]))
  
  # Combine the encoded fields back to the originalDataset
  encodedDataset <- cbind(dataSetToUse_02[,which(field_types==TYPE_SYMBOLIC)],trsf)
  
  # Remove original fields that have been hot encoded
  newData<- encodedDataset %>% select(-c(fieldsForEncoding))
  # Return new dataset
  return(newData)
}

# ************************************************
# normalise() :
#   Normalise fields between 1 and 0
#
# INPUT       :   Fields to normalise
#
# OUTPUT      :   Normalised fields between 1 and 0
# ************************************************
normalise <- function(values) {
  return ((values - min(values)) / (max(values) - min(values)))
}


# ************************************************
# createHoldoutDataset() :
#
# Function for creating the holdout data
#
# INPUT   : dataset - data.frame - dataset to be used
#         : holdout - integer    - value to be used as the holdout value 
#
# OUTPUT  : list - list of datasets with the training and testing datasets  
#
#*************************************************

createHoldoutDataset <-function(dataset, holdout){
  trainingSampleSize <- round(nrow(dataset))*(holdout/100)
  
  #Create the training Set
  trainingSet <- dataset[1:trainingSampleSize,]
  
  #Create the test Set
  testSet <- dataset[-(1:trainingSampleSize),]
  
  return(list(training = trainingSet, test = testSet))
}

# ************************************************
# unfactorNumeric() :
#
# Unfactor numeric data
#
# INPUT   : factor - value to unfactor
#
# OUTPUT  : numeric - unfactored numeric value
#
#*************************************************

unfactorNumeric <- function(x){
  as.numeric(levels(x))[x]
}

