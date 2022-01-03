holidays=read.csv("data-raw/holidays.csv")
holidays$ds=as.Date(holidays$ds)
usethis::use_data(holidays)
