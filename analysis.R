# library(RMySQL)
# 
# drv = dbDriver("MySQL")
# con = dbConnect(drv, dbname="conflict",user="root", password="root")

# DZ
# x <- dbGetQuery(con, "select * from DZ") 

rm(list=ls())
countries = c(
  "DZ","AO","BJ","BW","BF",
  "BI","CI","CV","CM","CF","TD","CG",
  "CD","DJ","EG","GQ","ER","ET","GA",
  "GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","MA",
  "MZ","NA","NE","NG","RW","SN","SC",
  "SL","SO","ZA","SS","SD","SZ","TZ",
  "TG","TN","UG","ZM","ZW")

tname <- "ln_ged_best_sb"
etname <- "ged_best_sb"
tot <- NULL
for(country in countries){
cat(sprintf("\nDoing country %s\n", country))
  #country <- "DZ"

load(sprintf("%s.rda", country))
dim(x)
series <- unique(tolower(read.csv("vars.csv", header = TRUE,stringsAsFactors = FALSE)$series))

head(series)
x$series <- tolower(x$series)
idx <- (x$series %in% series)
if(length(idx)>0){
  x <- x[idx,]
}
dim(x)


library(xts)
idx <- which(x$series == tname)
target <- xts(x$value[idx],order.by = as.Date(x$date[idx]))
colnames(target)[1] <- tname
plot(target)
idx <- which(x$series == etname)
etarget <- xts(x$value[idx],order.by = as.Date(x$date[idx]))
colnames(etarget)[1] <- etname
plot(etarget)

dates <- as.character(time(target))

vars <- series[-which(series %in% c(tname,etname))]
nv <- length(vars)
nv

tmp <- NULL
todrop <- NULL
for(i in 1:nv) {
  cat(sprintf("\n[%.3d/%.3d] %s",i,nv, vars[i]))
  idx <- which(x$series==vars[i])
  if(length(idx)>0){
    obj <- xts(x$value[idx], order.by=as.Date(x$date[idx])) 
    colnames(obj) <- vars[i]
    tmp <- cbind(tmp, obj )
  } else {
    todrop <- c(todrop, i)
  }
}
dim(tmp)
length(todrop)
length(todrop)+ncol(tmp)
if(length(todrop)>0){
  vars <- vars[-todrop]
}

refDate <- as.Date("2020-08-31")
ref.time <- sprintf("/%s", refDate)
data <- tmp[ref.time] # cut up to reference date

all <- cbind(target,etarget, data)
all <- na.approx(all,rule=2)
xx <- apply(all,2, function(u) length(which(is.na(u)))) 
idx <- which(xx>0)
if(length(idx)>0){
  todrop <- colnames(all)[idx]
  all <- all[,-idx]  
  vars <- vars[-match(todrop,vars)]
}


idx <- which(as.character(time(all)) %in% dates)
length(idx)
finaldata <- all[idx,]


save(finaldata, etname, tname, vars, file=sprintf("fd-%s.rda",country))



#load(sprintf("fd-%s.rda",country))
# mod.arima <- try(arima(as.ts(finaldata[,tname]),order=c(1,0,1)))
# fit.arima <- as.numeric(predict(mod.arima,n.ahead=7)$pred)
# names(fit.arima) <- 1:7
# head(fit.arima)

library(xts)
library(glmnet)

rdate <- "/2020-08-31"
target <- finaldata[rdate,tname]
plot(target)
etarget <- finaldata[rdate,etname]

x.last <- matrix(as.matrix(finaldata["2020-08-31",vars]), nrow=1)
alpha <- 0.5
myfamily <- "gaussian"
myrelax <- FALSE
tmp.target <- target
srange <- 1:7
pmin <- NULL
p1se <- NULL
prlast.min <- NULL
prlast.1se <- NULL
cat("\n")
for(nh in srange){
  n.ahead <- nh
   cat(".")
  n <- length(tmp.target)
  n
  y0 <- as.numeric(tmp.target)[n]

  y.range <- (n.ahead+1):(n-n.ahead)
  x.pred <- n-n.ahead
  x.range <- n.ahead:(n-n.ahead-1)
  x.mat <- as.matrix(finaldata[,vars])[x.range,]
  dim(x.mat)
  length(y.range)
  length(x.range)

  x.new <- matrix(as.matrix(finaldata[,vars])[x.pred,], nrow=1)

  y <- as.numeric(tmp.target)[y.range]
  if(myfamily=="poisson"){
    y <- as.integer(tmp.target)[y.range]
  }

  cvfit <- try(cv.glmnet(x.mat, y, alpha=alpha, family=myfamily, grouped=FALSE, relax = myrelax), silent = TRUE)
  cvfit
  if(class(cvfit)!="try-error"){
    fit.min <- glmnet(x.mat, y, family = myfamily, alpha=alpha, lambda=cvfit$lambda.min, relax = myrelax)
    fit.1se <- glmnet(x.mat, y, family = myfamily, alpha=alpha, lambda=cvfit$lambda.1se, relax = myrelax)
  
    pred.min <- predict(fit.min, newx = x.new, s=cvfit$lambda.min,  type="response" )
    pred.1se <- predict(fit.1se, newx = x.new, s=cvfit$lambda.1se,  type="response" )
    pr.min <- predict(fit.min, newx = x.last, s=cvfit$lambda.min,  type="response" )
    pr.1se <- predict(fit.1se, newx = x.last, s=cvfit$lambda.1se,  type="response" )
    y0
  } else {
    pred.min <- pred.1se <-  pr.min <- pr.1se <- mean(target,na.rm=TRUE)
  }
  pmin <- c(pmin, pred.min)
  p1se <- c(p1se, pred.1se)
  prlast.min <- c(prlast.min, pr.min)
  prlast.1se <- c(prlast.1se, pr.1se)
}
names(pmin) <- srange
names(p1se) <- srange
names(prlast.min) <- srange
names(prlast.1se) <- srange
pmin
p1se
prlast.min
prlast.1se

output <- data.frame(prlast.1se,step=srange,country=country, ACI_AdaENet_pred
 =   paste0(sprintf("forecast for %s ",month.name[(7+srange) %% 12 +1]),
            c(rep(2020,3),rep(2021,4)),sep =""), stringsAsFactors = FALSE)
                    
colnames(output)[1] <- tname
output

tot <- rbind(tot, output)
}

save(tot,file="totAug2020.rda")
# adding step 0
countries = c(
  "DZ","AO","BJ","BW","BF",
  "BI","CI","CV","CM","CF","TD","CG",
  "CD","DJ","EG","GQ","ER","ET","GA",
  "GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","MA",
  "MZ","NA","NE","NG","RW","SN","SC",
  "SL","SO","ZA","SS","SD","SZ","TZ",
  "TG","TN","UG","ZM","ZW")

load("totAug2020.rda")
library(xts)
for(ct in countries){
  cat(sprintf("%s,",ct))
  load(sprintf("fd-%s.rda",ct))
  rdate <- "2020-08-31"
  target <- as.numeric(finaldata[rdate,tname])
  tot <- rbind(tot, data.frame(ln_ged_best_sb=target, step=0, 
             country=ct, ACI_AdaENet_pred ="actual value August 2020",
             stringsAsFactors = FALSE))
}

rownames(tot) <- NULL
task1 <- NULL
delta <- NULL
for(ct in countries){
  idx <- which(tot$country==ct)
  tt <- tot[idx,]
  tt <- tt[order(tt$step),]
  dt <- tt
  dt[2,1] <- dt[2,1] - dt[1,1]
  dt[3,1] <- dt[3,1] - dt[1,1]
  dt[4,1] <- dt[4,1] - dt[1,1]
  dt[5,1] <- dt[5,1] - dt[1,1]
  dt[6,1] <- dt[6,1] - dt[1,1]
  dt[7,1] <- dt[7,1] - dt[1,1]
  dt[8,1] <- dt[8,1] - dt[1,1]
  dt[1,1] <- dt[1,1] - dt[1,1]
  colnames(dt)[1] <- "delta_ln_ged_best_sb"
  dt$ln_ged_best_sb <- tt$ln_ged_best_sb
  dt <- dt[,c("ln_ged_best_sb","delta_ln_ged_best_sb","step","country","ACI_AdaENet_pred")]
  task1 <- rbind(task1, tt)
  delta <- rbind(delta, dt)
}



save(delta,file="task1-delta.rda")
save(task1,file="task1.rda")
openxlsx::write.xlsx(delta,file="task1-delta.xlsx")
openxlsx::write.xlsx(task1,file="task1.xlsx")


est <- delta
idx <- which(est$step %in% 0:1)
est <- est[-idx,]
est$ln_ged_best_sb <- NULL
head(est)
colnames(est)[4] <- "Notes"
head(est)
colnames(est)[1] <- "ACI_AdaENet_pred"
head(est)
ccodes <- read.csv("ccodes.csv",stringsAsFactors = FALSE, na.strings = "")
est$country_id <- ccodes$country_id[match(est$country, ccodes$ISO)]
est$month_id <- 0
est <- est[,c("country_id",	"country",	"month_id",	"ACI_AdaENet_pred",	"step",	"Notes")]
est$month_id[grep("October",est$Notes)] <- 490
est$month_id[grep("November",est$Notes)] <- 491
est$month_id[grep("December",est$Notes)] <- 492
est$month_id[grep("January",est$Notes)] <- 493
est$month_id[grep("February",est$Notes)] <- 494
est$month_id[grep("March",est$Notes)] <- 495

head(est)

save(est,file="task1-estimates.rda")
openxlsx::write.xlsx(est,file="task1-estimates.xlsx")



# back-testing
library(xts)
library(lubridate)
library(glmnet)
cnames <- c("country_ID","month_ID","Country","Year",	"Month",	
            "ACI_AdaENet_pred_step1",	"ACI_AdaENet_pred_step2",	
            "ACI_AdaENet_pred_step3",	"ACI_AdaENet_pred_step4",	
            "ACI_AdaENet_pred_s5",	"ACI_AdaENet_pred_s6",	
            "ACI_AdaENet_pred_s7")


getDate <- function(date){
  m <- month(date)
  mn <-  as.character(month(ymd(date),label=TRUE, abbr=FALSE))
  y <- year(date)
  id <- as.integer(interval(ymd("1989-12-13"),ymd(date))   %/% months(1)  +120) 
  return(list(month=m,mname=mn, year=y,id=id,date=date))
}

# prepare dates
endDates <- NULL
for(i in 1:(492-120)){
   date <- as.character( ymd(start) %m+% months(i) ) 
   x <- getDate(date)
   endDates <- rbind( endDates, data.frame(month_ID = x$id, Year=x$y, Month=x$mname,
                            m = x$month, date=date, stringsAsFactors = FALSE))
}
idx <- which(endDates$date>="2014-01-01" & endDates$date<="2019-12-31")
endDates <- endDates[idx,]
rownames(endDates) <- NULL

countries = c(
  "DZ","AO","BJ","BW","BF",
  "BI","CI","CV","CM","CF","TD","CG",
  "CD","DJ","EG","GQ","ER","ET","GA",
  "GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","MA",
  "MZ","NA","NE","NG","RW","SN","SC",
  "SL","SO","ZA","SS","SD","SZ","TZ",
  "TG","TN","UG","ZM","ZW")

ccodes <- read.csv("ccodes.csv",stringsAsFactors = FALSE, na.strings = "")
idx <- match(countries,ccodes$ISO)
countries <- data.frame(ISO=countries, code=ccodes$country_id[idx], stringsAsFactors = TRUE)

library(glmnet)
library(xts)
library(ranger)

big <- NULL
set.seed(123)
nd <- nrow(endDates)
nct <- nrow(countries)
for(i in 1:nct){
  tmp <- NULL
  country <- countries$ISO[i]
  ccode <- countries$code[i]
  cat(sprintf("\nCountry: %s\n", country))
  bigcf <- NULL
  for(j in 1:nd){
    load(sprintf("fd-%s.rda",country))
    edate <- endDates$date[j]
    mid <- endDates$month_ID[j]
    mname <- endDates$Month[j]
    year <- endDates$Year[j]
    rdate <- sprintf("/%s", edate)
    fdate <- sprintf("%s/", edate)
    target <- finaldata[rdate,tname]
    future <- finaldata[fdate,tname]
    future <- as.numeric(future[-1,])
    plot(target)
    cat(sprintf("\n%s", edate))
    x.last <- matrix(as.matrix(finaldata[edate,vars]), nrow=1)
    alpha <- 0.5
    myfamily <- "gaussian"
    myrelax <- FALSE
    tmp.target <- target
    srange <- 1:7
    pmin <- NULL
    p1se <- NULL
    prlast.min <- NULL
    prlast.1se <- NULL
    cat("\n")
    for(nh in srange){
      n.ahead <- nh
      cat(".")
      n <- length(tmp.target)
      n
      y0 <- as.numeric(tmp.target)[n]
      
      y.range <- (n.ahead+1):(n-n.ahead)
      x.pred <- n-n.ahead
      x.range <- n.ahead:(n-n.ahead-1)
      x.mat <- as.matrix(finaldata[,vars])[x.range,]
      dim(x.mat)
      length(y.range)
      length(x.range)
      
      x.new <- matrix(as.matrix(finaldata[,vars])[x.pred,], nrow=1)
      
      y <- as.numeric(tmp.target)[y.range]
      if(myfamily=="poisson"){
        y <- as.integer(tmp.target)[y.range]
      }
      
      cvfit <- try(cv.glmnet(x.mat, y, alpha=alpha, family=myfamily, grouped=FALSE, relax = myrelax), silent = TRUE)
      cvfit
      if(class(cvfit)!="try-error"){
        fit.min <- glmnet(x.mat, y, family = myfamily, alpha=alpha, lambda=cvfit$lambda.min, relax = myrelax)
        fit.1se <- glmnet(x.mat, y, family = myfamily, alpha=alpha, lambda=cvfit$lambda.1se, relax = myrelax)
        cf <- coef(fit.1se,s=cvfit$lambda.1se)
        ncf <- rownames(cf)
        poscf <- which(as.numeric(cf)>0 & ncf != "(Intercept)")
        if(length(poscf)>0){
          tmp.cf <- data.frame(country=country, coef=ncf[poscf], value=cf[poscf], date=edate, step=nh, stringsAsFactors = FALSE)
          tmp.df <- data.frame(y,x.mat[,which(as.numeric(cf)>0) -1 ])
          rg <- ranger(y~., data=tmp.df, importance="impurity")
          imp <- sort(importance(rg), decreasing = TRUE)
          rank <- (length(imp):1)/length(imp)
          names(rank) <- names(imp)
          idd <- match(tmp.cf$coef,names(rank))
          tmp.cf$rank <- rank[idd]
          bigcf <- rbind(bigcf, tmp.cf)
        }
        pred.min <- predict(fit.min, newx = x.new, s=cvfit$lambda.min,  type="response" )
        pred.1se <- predict(fit.1se, newx = x.new, s=cvfit$lambda.1se,  type="response" )
        pr.min <- predict(fit.min, newx = x.last, s=cvfit$lambda.min,  type="response" )
        pr.1se <- predict(fit.1se, newx = x.last, s=cvfit$lambda.1se,  type="response" )
        y0
      } else {
        pred.min <- pred.1se <-  pr.min <- pr.1se <- mean(target,na.rm=TRUE)
      }
      pmin <- c(pmin, pred.min)
      p1se <- c(p1se, pred.1se)
      prlast.min <- c(prlast.min, pr.min)
      prlast.1se <- c(prlast.1se, pr.1se)
    }
    names(pmin) <- srange
    names(p1se) <- srange
    names(prlast.min) <- srange
    names(prlast.1se) <- srange
    pmin
    p1se
    prlast.min
    prlast.1se
    
    o1 <- data.frame(type="estimate",country_ID=ccode, month_ID= mid, Country = country,
               Year=year, Month=mname,	
                last = y0,
                step1 = prlast.1se[1],
                step2 = prlast.1se[2],
                step3 = prlast.1se[3],
                step4 = prlast.1se[4],
                step5 = prlast.1se[5],
                step6 = prlast.1se[6],
                step7 = prlast.1se[7],
               stringsAsFactors = FALSE)
       o2 <-   data.frame(type="true",country_ID=ccode, month_ID= mid, Country = country,
                         Year=year, Month=mname,	
                         last = y0,
                         step1 = future[1],
                         step2 = future[2],
                         step3 = future[3],
                         step4 = future[4],
                         step5 = future[5],
                         step6 = future[6],
                         step7 = future[7],
                         stringsAsFactors = FALSE)
       
    tmp <- rbind(tmp, o1, o2)  
  }
  
  save(tmp,file=sprintf("backtest-%s.rda", country))
  write.csv(tmp,file=sprintf("backtest-%s.csv", country), row.names = FALSE)
  openxlsx::write.xlsx(tmp,file=sprintf("backtest-%s.xlsx", country))
  save(bigcf,file=sprintf("coef-%s.rda", country))
  write.csv(bigcf,file=sprintf("coef-%s.csv", country), row.names = FALSE)
  openxlsx::write.xlsx(bigcf,file=sprintf("coef-%s.xlsx", country))
  big <- rbind(big, tmp)
}
save(big,file="big-backtest.rda")
write.csv(big,file="big-backtest.csv", row.names = FALSE)
openxlsx::write.xlsx(big,file="big-backtest.xlsx")

load("big-backtest.rda")

big2 <- big
big2[,8] <- big2[,8] - big[,7] 
big2[,9] <- big2[,9] - big[,7] 
big2[,10] <- big2[,10] - big[,7] 
big2[,11] <- big2[,11] - big[,7] 
big2[,12] <- big2[,12] - big[,7] 
big2[,13] <- big2[,13] - big[,7] 
big2[,14] <- big2[,14] - big[,7] 


write.csv(big2,file="big2-backtest.csv", row.names = FALSE)
openxlsx::write.xlsx(big2,file="big2-backtest.xlsx")



