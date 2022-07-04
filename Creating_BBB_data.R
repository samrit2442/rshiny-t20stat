library(plyr)

.rs.files.restoreBindings()

url <- "https://cricsheet.org/downloads/t20s_male_csv2.zip"
download.file(url, dest = "dataset.zip", mode = "wb")
unzip("dataset.zip", exdir = "./data_sets")

mydir = "./data_sets/"
t20_data = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
# t20_data

class(t20_data[1])
length(t20_data)

t = vector(mode = "list", length = length(t20_data)/2)

for(i in 1:(length(t20_data)/2))
{
  t[[i]] = read.csv(file = t20_data[2*i-1], header = T)
}
t20 = rbind.fill(t)

# write.csv(t20, file = "C:/Users/Samrit Pramanik/Documents/Cricket_Project_Personal/t20_data_12_May_2022.csv")

