
look_sr_co2 <- function(filename){
        
        require(dplyr)
        require(lubridate)
        require(manipulate)
        require(readr)
        
        SR <- read_delim(filename, 
                         delim = " ", 
                         escape_double = FALSE, 
                         col_types = cols(date = col_character(), 
                                          time = col_character()), 
                         trim_ws = TRUE) 
        
        names(SR) <- make.names(names(SR))
        
        b<-select(SR, date, time, chamber_on_f, co2.ppm.) %>%
                filter(chamber_on_f==1, co2.ppm.!=is.na(co2.ppm.))%>%
                mutate(TS=ymd_hms(paste(date,time)))

        windows()
        manipulate(
                plot(b$co2.ppm., type = "l", 
                     xlim=c(x.min,x.min+range), ylim=c(400,1000)
                     ),
                range=slider(1,nrow(b), initial=1000, step=200),
                x.min=slider(1,nrow(b), initial=1, step=500)
                )
}
