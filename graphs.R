library(ggplot2)

#### Continent graph


day_counter = function(day_ind){
  day= l[[day_ind]]
  conf = sum(day$Confirmed,na.rm = T)
  death = sum(day$Deaths, na.rm = T)
  rec = sum(day$Recoverd, na.rm = T)
  act = sum(day$Active, na.rm = T)
  return(c(day_ind,conf, death, rec,act))
}

result <- lapply(seq_along(l), day_counter)
df = as.data.frame(do.call(rbind,result))
colnames(df) <- c("Day", "Confirmed", "Deaths", "Recovered", "Active")

top = df$Confirmed+ df$Deaths+df$Recovered+df$Active
bot = df$Deaths
m1 = df$Deaths+df$Recovered
m2 = df$Deaths+df$Recovered+ df$Active

plot(df$Day,top, type = "l", col = "red")
lines(df$Day,bot, type = "l", col = "blue")
lines(df$Day,m1, type = "l", col = "green")
lines(df$Day,m2, type = "l", col = "yellow")


plot(df$Day[1:400],df$Confirmed[1:400], type = "l", col = "red")
lines(df$Day[1:400],df$Deaths[1:400], type = "l", col = "blue")
lines(df$Day[1:400],df$Recovered[1:400], type = "l", col = "green")
lines(df$Day[1:400],df$Active[1:400], type = "l", col = "yellow")


ggplot(data.frame(days,df$Confirmed), aes(df$Day,df$Confirmed))+
  geom_line()
