library(plyr)

# .rs.files.restoreBindings()

mydir = "E:/Cricket_Project_Personal/T20I_02_Apr_2022"
t20_data = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
t20_data

class(t20_data[1])
length(t20_data)

t = vector(mode = "list", length = length(t20_data)/2)

for(i in 1:(length(t20_data)/2))
{
  t[[i]] = read.csv(file = t20_data[2*i-1], header = T)
}
t20 = rbind.fill(t)

write.csv(t20, file = "E:/Cricket_Project_Personal/t20_data_02_Apr_2022.csv")

