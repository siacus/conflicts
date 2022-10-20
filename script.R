library(RMySQL)

drv = dbDriver("MySQL")
con = dbConnect(drv, dbname="conflict",user="root", password="root")

countries = c(
"DZ","AO","BJ","BW","BF",
"BI","CI","CV","CM","CF","TD","CG",
"CD","DJ","EG","GQ","ER","ET","GA",
"GM","GH","GN","GW","KE","LS","LR",
"LY","MG","MW","ML","MR","MU","MA",
"MZ","NA","NE","NG","RW","SN","SC",
"SL","SO","ZA","SS","SD","SZ","TZ",
"TG","TN","UG","ZM","ZW")

# CREATE TABLE countries AS SELECT distinct(origin) from indexes;

#countries <- dbGetQuery(con,"select * from countries")
nc <- length(countries)
for(k in 1:nc){
  country <- countries[k]
  cat(sprintf("\n[%.2d/%.2d] Creating table for country: %s...", k, nc, country))
  query <- sprintf('CREATE TABLE %s AS SELECT * FROM indexes where frequency="monthly" and year>=2004 and origin="%s"',
                     country, country)
  out <- try(dbSendQuery(con, query), TRUE)
  if(class(out)[1]=="try-error"){
    cat("failed!\n")
  } else {
    cat("copy success!\n")
  }
  query <- sprintf('select * from  %s', country)
  x <- try(dbGetQuery(con, query), TRUE)
  if(class(x)[1]=="try-error"){
    cat("dump failed!\n")
  } else {
    save(x, file=sprintf("%s.rda", country))
    cat("dump success!\n")
  }
}


dbDisconnect(con)

library(lubridate)
start <- as.Date("1989-12-31")

getDate <- function(date){
  m <- month(date)
  mn <-  as.character(month(ymd(date),label=TRUE, abbr=FALSE))
  y <- year(date)
  id <- as.integer(interval(ymd("1989-12-13"),ymd(date))   %/% months(1)  +120) 
  return(list(month=m,mname=mn, year=y,id=id,date=date))
}

for(i in 1:(492-120)){
  print( sprintf("%d %s", 120+i, ymd(start) %m+% months(i) ))
}
