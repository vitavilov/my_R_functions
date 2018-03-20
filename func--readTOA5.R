## funtion to read Campbell Scientific TOA5 format files to dataframes (metadata are lost)
read.TOA5 <- function(filename, Timestamp_as_POSIXct = TRUE, output_list_meta = FALSE){
      require(dplyr)
      
      file_header<-read.table(filename, header=F,skip=1, nrows = 3, 
                              na.strings = "NAN", sep="," )
      
      
      file_data<-read.table(filename, header=F, skip = 4, 
                            na.strings = c("NAN","NA"),
                            sep=",", stringsAsFactors = F) %>%
            `names<-`(as.character(t(file_header)[,1]))
      
      
      
      
      #check for columns with all NA's and change their class to numeric
      for (x in 1:ncol(file_data)){
            
            if (all(is.na(file_data[,x]))) {
                  file_data[,x] <- as.numeric(file_data[,x])
            }
      }
      
      
      #coerce character TIMESTAMP to POSIXct:
      if (Timestamp_as_POSIXct) {
            require(lubridate)
            file_data$TIMESTAMP <- ymd_hms(file_data$TIMESTAMP)
      }
      
      
      if (output_list_meta){
            meta_file <-read.csv(filename, nrows = 1, header = F)
            meta_file <- as.vector(t(meta_file[1,])) %>%
                  `names<-`(c("file_format", 
                              "station_name",
                              "table_name",
                              "model_name",
                              "os_version",
                              "dld_name",
                              "dld_signature",
                              "cpu_serial_number"))
            
            
            meta_vars <- as.data.frame(t(file_header)) %>%
                  `names<-`(c("variable", "units", "stats"))
            
            #if Units are not stated in TOA5 file, make it "NA"
            meta_vars$units = replace(meta_vars$units, meta_vars$units=="", NA)
            
            meta <- list(file = as.list(meta_file), vars = meta_vars)
            
            out <- list(data = file_data, meta = meta)
            
            return(out)
            
      } else {
            return(file_data)
      }
      
}


