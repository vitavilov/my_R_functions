# На входе - датафрейм с регулярными отметками времени и какими-то переменными,
# на выходе - график доступности данных
# 
# Временной ряд должен быть регулярным (или регуляризоваться внутри функции) 
# 
# 
# сделать по дням, цвет - наполнение дня 
# ПРОВЕРКУ РЕГУЛЯРНОСТИ

plot_ts_na <- function(df, ts_posixct = "timestamp", facet.years = TRUE, day_sum = TRUE){
      require(ggplot2)
      require(dplyr)
      require(tidyr)
      require(lubridate)
      
      names(df)[which(names(df) == ts)] <- "t.stamp"
      
      df_long_cc <- gather(data = df, key = "parameter", value = "value", -t.stamp ) %>%
            mutate(parameter = as.factor(parameter),
                   yr  = year(t.stamp),
                   date = date(t.stamp)) %>%
            filter(!is.na(value))
      
      if (day_sum){
            df_long_cc_day <- df_long_cc %>%
                  group_by(parameter, date) %>%
                  summarize(n = n()) %>%
                  mutate(yr = year(date))
            
            
            p <- ggplot(df_long_cc_day, aes(x=date, y=parameter)) +
                  geom_point(shape = 15, alpha=0.5, aes(colour = n)) +
                  scale_colour_gradient(low = "firebrick", high = "forestgreen")
            
            
            
            if (facet.years){
                  p <- p +
                        facet_wrap("yr", ncol = 1, scales = "free_x", strip.position = "right") +
                        scale_x_date(date_labels = "%b")
            }
            
      } else{ #if not day.sum
            
            
            p <- ggplot(df_long_cc, aes(x=t.stamp, y=parameter)) + 
                  geom_point(shape = "|", aes(colour = parameter),show.legend=F)
            
            if (facet.years) {
                  p <- p + 
                        facet_wrap("yr", ncol = 1, scales = "free_x", strip.position = "right") +
                        scale_x_datetime(date_labels = "%b")
            }
      }
      
      return(p)
} #end of the function


