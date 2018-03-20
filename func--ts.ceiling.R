#makes a new variable in dataframe with ceiled timestamp by period

ts.ceiling <- function (df, timeVar="ts", period.sec=60*30, origin.POSIXct="1970-01-01"){
        
        if (all(class(df[[timeVar]])!="POSIXct")){
                stop("timeVar must be POSIXct")
                
        } else {
                
                ts.ceil <- as.POSIXct(
                        x = ceiling(as.numeric(df[[timeVar]])/period.sec)*period.sec,
                        origin = origin.POSIXct, 
                        tz = unique(format(df[[timeVar]],"%Z"))
                )
                
                df <- cbind(ts.ceil, df)
                
                return(df)
        }
}