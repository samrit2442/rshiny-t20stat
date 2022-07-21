library(plyr)
# .rs.files.restoreBindings()

date <- format(Sys.time(), c("%d","%b","%Y","_","%H","%M"))
dataset_name <- paste0(date, collapse = '') |> toupper()
dataset_name <- paste0("./DATASET", dataset_name)

url <- "https://cricsheet.org/downloads/t20s_male_csv2.zip"
download.file(url, dest = "dataset.zip", mode = "wb")
unzip("dataset.zip", exdir = dataset_name)

mydir = paste0(dataset_name, "/")
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

# write.csv(t20, file = "./t20_data_12_May_2022.csv")

first_date <- t20$start_date |> unique() |> as.Date(format = "%Y-%m-%d") |> min()
m_f <- format(first_date, "%m") |> as.numeric()
d_f <- format(first_date, "%d")
y_f <- format(first_date, "%Y")
first_date_updated <- paste0(month.abb[m_f], " ", d_f,", ",y_f)
first_date_updated

last_date <- t20$start_date |> unique() |> as.Date(format = "%Y-%m-%d") |> max()
m <- format(last_date, "%m") |> as.numeric()
d <- format(last_date, "%d")
y <- format(last_date, "%Y")
last_date_updated <- paste0(month.abb[m], " ", d,", ",y)
last_date_updated







