ts.pad.NAs <- function (df, timeVar, period.sec = 30*60){
      require(dplyr)
      
      ## find exact time period by rounding average difference between records
      # period.sec = round(mean(diff(as.numeric(df$timeVar))))
      
      #создать последовательность, потом соединить
      start = df[[timeVar]][1]
      end = df[[timeVar]][nrow(df)-1]
      
      ts.reg <- seq.POSIXt(from = start, 
                           to = end,
                           by = paste(period.sec, "sec"))
      
      #подготовить регулярную шкалу времени для слияния
      ts.reg <- data.frame(ts.reg) 
      names(ts.reg) <- timeVar
      
      #наложить имеющиеся данные на регулярную шкалу:
      dfReg <- left_join(ts.reg, df, by = timeVar)
      
      return(dfReg)
      
}