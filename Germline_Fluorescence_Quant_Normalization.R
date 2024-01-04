
#This is a tool developed by Jacob McAuley to normalize the quantification of fluorescence intensity measurements across variable lengths.

##EXPLANATION OF THIS PROGRAM##

#Images were taken on Leica Sp8 Lightsheet confocal microscope
#Using the freehand line tool, setting width to measure the entire germline while minimizing highlighting other structures, draw line over proximal germline starting at the bend and click measure>plot profile 
#Save each plot profile in a single folder
#this program will normalize and combine all seperate plot profile tables in a folder 

# the output is a csv where each plot profile is a column that represents the fluorescence from 0-100%{bend-proximal oocyte 0}


##USING THIS PROGRAM##

# When run, this code will prompt user input the folder path to the data needs to be normalized.
#input the full path to ensure that the program can find what your looking for (this means the program can be saved in a separate place from the data)
#the folder should contain the .csv files from the quantification from ImageJ, measurements from all conditions should be placed in a single folder
#this program will normalize the measurements and place all measurements in one csv with each column showing a measurement with the column name of the initial .csv from which it was normalized
#The normalized data will be saved into the same folder as the data as normalized.csv



# data input ------------------------------------------------------------------
# Prompt the user to input a folder
folder_path <- readline("Enter the folder path: ")

# List all the files in the folder
file_list <- list.files(path = folder_path, full.names = TRUE)

# Initialize an empty list to store data frames
data_frames <- list()

# Loop through the files and load them as data frames
for (file in file_list) {
  if (file_ext(file) %in% c(".csv", ".txt")) {
    df_name <- tools::file_path_sans_ext(basename(file))
    df <- read.csv(file)
    data_frames[[df_name]] <- df
  } else {
    cat("Skipping file:", file, " (unsupported file type)\n")
  }
}

# Provide a list of loaded data frames
if (length(data_frames) > 0) {
  cat("Data frames loaded:\n")
  print(names(data_frames))
} else {
  cat("No compatible files found in the specified folder.\n")
}

#normalization==================================================================

funfcn <- function(data){
  
  percent = c()
  
  variable = data
  
  for(int in 1:nrow(variable)){
    
    percent = append(percent, variable[int,1]/variable[nrow(variable),1]*100)
  }
  
  variable[,1] = percent
  
  # define the intervals
  
  intervals <- c(seq(0,100,0.21))
  
  # use cut function to group values into intervals
  
  variable$interval_group <- cut(variable[,1], breaks = intervals)
  
  # calculate the mean of values in each interval
  result <- aggregate(variable[,2], by = list(variable$interval_group), FUN = mean)
  
  # define the distance of intervals 
  
  # create a vector to store the middles
  middles <- numeric(length(intervals) - 1)
  
  # loop over adjacent pairs of values
  
  for (i in 1:(length(intervals) - 1)) {
    # calculate the middle and store it in the vector
    middles[i] <- mean(c(intervals[i], intervals[i+1]))
  }
  
  result[,1] <- middles
  
  # return result
  return(result)
}


