## funtion to read Campbell Scientific TOA5 format files to dataframes (metadata are lost)
read.TOA5 <- function(filename, Timestamp_as_POSIXct = TRUE){
      require(dplyr)
      
      file_header<-read.table(filename, header=F,skip=1, nrows = 4, 
                              na.strings = "NAN", sep="," )
      
      c_names <- as.vector(t(file_header[1,]))
      
      file_data<-read.table(filename, header=F, skip = 4, na.strings = c("NAN","NA"),
                            sep=",", stringsAsFactors = F)
      
      colnames(file_data) <- c_names
      
      #check for columns with all NA's and change their class to numeric
      for (x in 1:ncol(file_data)){
            
            if (all(is.na(file_data[,x]))) {
                  file_data[,x] <- as.numeric(file_data[,x])
            }
      }
      
      #coerce character TIMESTAMP to POSIXct
      if (Timestamp_as_POSIXct) {
            require(lubridate)
            file_data$TIMESTAMP <- ymd_hms(file_data$TIMESTAMP)
      }
      
      return(file_data)
      
}


