# This script generates several types of heatmaps
# generates a score = number of times feature is selected x relative rank
# summarizes this through cluster analysis

# plots of coefficients
library(ggplot2) 
library(reshape2)

Topics <- tolower(c("Conflict","Travel", "visa",
            "War", "Passport",
            "Weapon", "Right of asylum",
            "Gun", "Immigration",
            "Firearm", "Refugee",
            "Aggression",
            "Bombardment",
            "Embargo",
            "Curfew"))

SIPRI <- c("milex_usd","milex_sharegov","import")

countries = c(
  "DZ","AO","BJ","BW","BF",
  "BI","CI","CV","CM","CF","TD","CG",
  "CD","DJ","EG","GQ","ER","ET","GA",
  "GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","MA",
  "MZ","NA","NE","NG","RW","SN","SC",
  "SL","SO","ZA","SS","SD","SZ","TZ",
  "TG","TN","UG","ZM","ZW")

nct <- length(countries)

library(pheatmap)
makeColorRampPalette <- function(colors, cutoff.fraction, num.colors.in.palette)
{
  stopifnot(length(colors) == 4)
  ramp1 <- colorRampPalette(colors[1:2])(num.colors.in.palette * cutoff.fraction)
  ramp2 <- colorRampPalette(colors[3:4])(num.colors.in.palette * (1 - cutoff.fraction))
  return(c(ramp1, ramp2))
}
cutoff.distance <- 0.5  
cols <- makeColorRampPalette(c("white", "steelblue",    # distances 0 to 3 colored from white to red
                               "steelblue", "red"), # distances 3 to max(distmat) colored from green to black
                             cutoff.distance  ,
                             100)


for(i in 1:nct){
  country <- countries[i]
  cat(sprintf("\nCountry: %s\n", country))
  load(sprintf("coef-%s.rda", country))
  if(is.null(bigcf)){
    next
  }
  mat <- bigcf
  mat$rank[is.na(mat$rank)] <- 1
  cc <- unique(mat$coef)
  dd <- sort(unique(mat$date))

  bigmat <- matrix(NA, ncol=length(cc), nrow=length(dd))
  colnames(bigmat) <- tolower(cc)
  rownames(bigmat) <- dd
  for(d in dd){
    kk <- which(mat$date == d)
    cf <- mat$coef[kk]
    jj <- match(cf, cc)
    bigmat[d,jj] <- mrank <- mat$rank[kk]
  }
  
  bigmat[is.na(bigmat) | bigmat==0] <- 0
  dp <- colSums(bigmat)
  # drop <- which(dp<= 0.05*nrow(bigmat)) # less that 5%
  # if(length(drop)>0){
  #   bigmat <- bigmat[,-drop]
  # }

  bigmat[is.na(bigmat) | bigmat==0] <- 0
  cc <- colnames(bigmat)
  dim(bigmat)

  bigmat <- t(bigmat)

  sipri <- rownames(bigmat)[rownames(bigmat) %in% SIPRI] 
  gdelt <- NULL
  gdelt <- rownames(bigmat)[startsWith(rownames(bigmat),"m_")]
  gdelt <- c(gdelt, rownames(bigmat)[startsWith(rownames(bigmat),"e_")])
  gdelt <- c(gdelt, rownames(bigmat)[startsWith(rownames(bigmat),"eg_")])
  gdelt <- c(gdelt, rownames(bigmat)[startsWith(rownames(bigmat),"eq_")])
  gdelt <- c(gdelt, rownames(bigmat)[startsWith(rownames(bigmat),"egq_")])
  
  # gtrend <- NULL
  # for(tp in Topics){
  #   gtrend <- c(gtrend,grep(tp,rownames(bigmat),fixed=TRUE))
  # }
  # gtrend <- rownames(bigmat)[gtrend]
  # id <- grep(".",gtrend,fixed=TRUE)
  # if(length(id)>0){
  #   gtrend <- gtrend[-id]
  # }
  gtrend <- rownames(bigmat)[rownames(bigmat) %in% Topics]
  
  
  group <- rep("ViEWS", nrow(bigmat))
  group[match(sipri, rownames(bigmat))] <- "Sipri"
  group[match(gtrend, rownames(bigmat))] <- "GTrend"
  group[match(gdelt, rownames(bigmat))] <- "GDelt"

  annotation_row = data.frame( DataClass = factor(group) )

  rownames(annotation_row) = rownames(bigmat)
  
  # performs clustering
  row.order <- try(hclust(dist(bigmat))$order, TRUE)
  if(class(row.order)=="try-error"){
      next
  }# clustering
  dat_new <- bigmat[row.order, ] # re-order matrix accoring to clustering

  nba.m <- melt(bigmat)
  nba.m <- melt(dat_new)
  mycol <- colorRampPalette(c("white", "steelblue", "lightgreen"))(12)
  aa <- ggplot(nba.m, aes(Var2, Var1)) + 
    geom_tile(aes(fill = cut(value,breaks=c(-1e-5,0,(1:10)/10))), colour = "white") + 
  #  scale_fill_gradient(low = "white", high = "steelblue")
    scale_fill_manual(values=mycol, breaks=levels(cut(nba.m$value,breaks=c(-1e-5,0,(1:10)/10))))

  nba.m$Var2 <- as.Date(nba.m$Var2)
  bb <- ggplot(nba.m, aes(Var2, Var1)) + 
    geom_tile(aes(fill = value), colour = "gray50") + 
    scale_fill_gradient2(midpoint=0.5, low = "white", mid = "steelblue",high = "red")+
    labs(x = "", y = "Select variable", fill="Rank")+
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
  #scale_x_date(date_minor_breaks = "1 month")
    scale_x_date(date_breaks = "1 month",  
               date_labels = "%b %Y", 
               limits = as.Date(c("2017-01-01","2019-09-01")))+
    ggtitle(sprintf("%s ", country))




     try(pheatmap(bigmat, cluster_cols = FALSE,
         #  clustering_distance_rows =  "correlation",
           # color = c("white", "steelblue", "red"),
         color=cols,
         # legend = FALSE,
         annotation_row = annotation_row,
         treeheight_row = 0, treeheight_col = 0,
         fontsize = 8,
         border_color = "gray",
         main = sprintf("%s : relative importance of predictors selected by AdaENet",country),
         angle_col="90",filename = sprintf("importance-%s.pdf",country),width=12,height = 6), TRUE)

}

library(data.table)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(cluster)
library(dendextend)

countries = c(
  "DZ","AO","BJ","BW","BF",
  "BI","CI","CV","CM","CF","TD","CG",
  "CD","DJ","EG","GQ","ER","ET","GA",
  "GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","MA",
  "MZ","NA","NE","NG","RW","SN","SC",
  "SL","SO","ZA","SS","SD","SZ","TZ",
  "TG","TN","UG","ZM","ZW")

nct <- length(countries)
vars <- NULL
for(i in 1:nct){
  country <- countries[i]
  cat(".")
  load(sprintf("coef-%s.rda", country))
  if(is.null(bigcf)){
    next
  }
  vars <- c(vars, bigcf$coef) 
}
vars <- unique(vars)

# MAT, MAT2 etc contains the scores
# used later for cluster analysis

MAT <- matrix(,ncol = length(vars)+1)
colnames(MAT) <- c("country",vars)
MAT <- data.frame(MAT)
MAT$country <- ""
tMAT <- MAT
MAT <- NULL

for(i in 1:nct){
  country <- countries[i]
  cat(sprintf("\nCountry: %s\n", country))
  load(sprintf("coef-%s.rda", country))
  if(is.null(bigcf)){
    cat(sprintf("\nNo data for %s", country))
    next
  }
  
  for(step in 1:7){
    bigcf$rank[is.na(bigcf$rank) & bigcf$step==step] <- 1
    idx <- which(bigcf$step==step & !is.na(bigcf$rank))
    if(length(idx)==0){ 
      next;
    }
  }
  if(length(idx)==0){ 
    cat(sprintf("\nRank null for %s", country))
    next;
  }
  mat <- as.data.table(bigcf[idx,])
  mat2 <- mat[,.(mean=mean(rank),count=.N),by=coef] # aveage rank
  mat2[,prod:=mean*count] # the score
  mat2 <- mat2[order(prod,decreasing = TRUE),,]
  tMAT[1,-1] <- as.numeric(NA)
  tMAT[1,match(mat2$coef,vars)] <- mat2$prod
  tMAT$country <- country
  MAT <- rbind(MAT,tMAT)

 
 p <- ggplot(data=mat2[mean>0 ,,],aes(x=count,y=prod,label=coef))+
    geom_point()+
     ggtitle(country)+
     geom_text_repel(  point.padding = unit(0.35, "lines")) +
     xlab("number of times variable is selected")+
     ylab("average relative rank")

   print(p)
   pdf(file=sprintf("scatter-%s.pdf",country))
   print(p)
   dev.off()
}


rownames(MAT) <- MAT$country
MAT$country <- NULL
idx <- which(as.vector(apply(MAT,2,function(u) length(which(is.na(u))))) == nrow(MAT))
MAT <- MAT[,-idx]
MAT2 <- MAT
MAT[is.na(MAT)] <- 0



SIPRI <- c("milex_usd","milex_sharegov","import")

sipri <- colnames(MAT)[colnames(MAT) %in% SIPRI] 
gdelt <- NULL
gdelt <- colnames(MAT)[startsWith(colnames(MAT),"m_")]
gdelt <- c(gdelt, colnames(MAT)[startsWith(colnames(MAT),"e_")])
gdelt <- c(gdelt, colnames(MAT)[startsWith(colnames(MAT),"eg_")])
gdelt <- c(gdelt, colnames(MAT)[startsWith(colnames(MAT),"eq_")])
gdelt <- c(gdelt, colnames(MAT)[startsWith(colnames(MAT),"egq_")])


wdi <- colnames(MAT)[startsWith(colnames(MAT),"wdi_")]
vdem <- colnames(MAT)[startsWith(colnames(MAT),"vdem_")]
fvp <- colnames(MAT)[startsWith(colnames(MAT),"fvp_")]
acled <- colnames(MAT)[startsWith(colnames(MAT),"acled_")]
ged <- colnames(MAT)[startsWith(colnames(MAT),"ged_")]
icgcw <- colnames(MAT)[startsWith(colnames(MAT),"icgcw_")]
colnames(MAT)[-match(c(gdelt,wdi,vdem,fvp,acled,ged,icgcw),colnames(MAT))]
   
factors <- fread("factors.csv") # grouped factors look up table
factors[,source:=""] 
factors$source[factors$series %in% gdelt] <- "gdelt"
factors$source[factors$series %in% wdi] <- "wdi"
factors$source[factors$series %in% vdem] <- "vdem"
factors$source[factors$series %in% fvp] <- "fvp"
factors$source[factors$series %in% acled] <- "acled"
factors$source[factors$series %in% ged] <- "ged"
factors$source[factors$series %in% icgcw] <- "icgcw"
factors$source[factors$series %in% sipri] <- "sipri"

factors <- factors[-which(factors$source==""),,]
factors <- factors[-which(factors$factor==""),,]
factors[,code:=sprintf("%s_%s",source,factor)]
factors[, code:=stringr::str_replace_all(code, c(" " = "_", "-" = "_"))]
factors[, factor:=stringr::str_replace_all(factor, c(" " = "_", "-" = "_"))]

# MAT5: average impact by CODES
# average only on values != 0 to avoid
# bias toward zero in the calculation of the mean

codes <- sort(unique(factors$code))
ncodes <- length(codes)
MAT5 <- matrix(0,nrow(MAT),ncodes)
rownames(MAT5) <- rownames(MAT)
colnames(MAT5) <- codes
nct <- nrow(MAT)
for(i in 1:nct){
  for(j in 1:ncodes){
    mycode <- codes[j]
    idx <- which(colnames(MAT) %in% factors$series[factors$code==mycode]) 
    t <- as.numeric(MAT[i,idx])
    if(length(which(t>0))>0){
     MAT5[i,mycode] <- mean(t[t>0])
    }
  }
}
 
# MAT3: average impact by FACTORS
# average only on values != 0 to avoid
# bias toward zero in the calculation of the mean

facts <- sort(unique(factors$factor))
nfacts <- length(facts)
MAT3 <- matrix(0,nrow(MAT),nfacts)
rownames(MAT3) <- rownames(MAT)
colnames(MAT3) <- facts
nct <- nrow(MAT)
for(i in 1:nct){
  for(j in 1:nfacts){
    myfact <- facts[j]
    idx <- which(colnames(MAT) %in% factors$series[factors$factor==myfact]) 
    t <- as.numeric(MAT[i,idx])
    if(length(which(t>0))>0){
      MAT3[i,myfact] <- mean(t[t>0])
    }
  }
}


# MAT6: average impact by SOURCES
# average only on values != 0 to avoid
# bias toward zero in the calculation of the mean
sources <- c("gdelt","wdi","vdem","fvp","acled","ged","icgcw")
nsources <- length(sources)
MAT6 <- matrix(0,nrow(MAT),nsources)
rownames(MAT6) <- rownames(MAT)
colnames(MAT6) <- sources
nct <- nrow(MAT)
for(i in 1:nct){
  for(j in 1:nsources){
    mysource <- sources[j]
    idx <- which(colnames(MAT) %in% factors$series[factors$source==mysource]) 
    t <- as.numeric(MAT[i,idx])
    if(length(which(t>0))>0){
      MAT6[i,mysource] <- mean(t[t>0])
    }
  }
}

# this function is called later for performing the
# profiling analysis
 
analysis <- function(mat,binary=FALSE,nclusters=8,type=NULL, width=5, height=10){

  myMAT <- mat
  if(binary){
    myMAT[myMAT>0] <- 1
    gdist <- daisy(myMAT, metric = "gower",type=list(asymm=c(1:ncol(myMAT))))
    hmap <- heatmaply::ggheatmap(t(myMAT),
              distfun=function(u) daisy(u, metric = "gower",type=list(asymm=c(1:ncol(myMAT)))) )
  } else {
    gdist <- daisy(myMAT, metric = "gower")
    hmap <- heatmaply::ggheatmap(t(myMAT),
              distfun=function(u) daisy(u, metric = "gower"))
  }
  data <- data.frame(cmdscale(gdist))
  colnames(data) <- c("x","y")
  gowercl <- hclust(gdist)
  data$country <- rownames(myMAT)
  data$cluster <- as.character(cutree(gowercl,k = nclusters))

  dend <- gowercl %>% as.dendrogram %>%
    set("branches_k_color", k = nclusters) %>% set("branches_lwd", 0.7) %>%
    set("labels_cex", 0.6) %>% set("labels_colors", k = nclusters) %>%
    set("leaves_pch", 19) %>% set("leaves_cex", 0.5) 
  ggd <- as.ggdend(dend)

  data$clusterCol <-  ggd$labels$col[match(data$country,ggd$labels$label)]
  myval <- as.character(1:nclusters)
  mycol <- data$clusterCol[match(myval, data$cluster)]

  p1 <- ggplot(ggd, horiz = TRUE)+ggtitle(sprintf("Clustering: %s (%s)", type, ifelse(binary,"binary","means")))

  p2 <- ggplot(data=data,aes(x=x,y=y,label=country,color=cluster))+
    geom_point()+
    ggtitle(sprintf("Multidimesional scaling: %s (%s)",type, ifelse(binary,"binary","means")))+
    geom_text_repel(  point.padding = unit(0.35, "lines"),show.legend  = FALSE) +
    xlab("")+ ylab("")+
    scale_colour_manual(values = mycol,labels=myval)

  p <- gridExtra::grid.arrange(p1, p2, ncol=1)

  print(p)
 #heatmap(MAT6)
  myfile <- sprintf("clustering-%s-%s-%d.pdf",type,ifelse(binary,"binary","means"),nclusters) 
  ggpubr::ggexport(p1,p2,hmap, nrow = 3, ncol = 1,filename=myfile, width=width, height=height, res=300)

  cls <- unique(data$cluster)
  tmpb <- NULL
  tmpm <- NULL
  tmpM <- NULL
  for(cl in cls){
    ct <- data$country[data$cluster==cl]  
    tmp <- round(myMAT[ct,],3)
    if(is.null(dim(tmp))){
      tmp <- matrix(tmp, nrow=length(ct), ncol=ncol(myMAT))
      colnames(tmp) <- colnames(myMAT)
      rownames(tmp) <- ct
    }
    tmp <- data.frame(tmp)
    tmp$cluster <- cl
    tmpb <- rbind(tmp, tmpb)
  
    tmp <- round(mat[ct,],3)
    if(is.null(dim(tmp))){
      tmp <- matrix(tmp, nrow=length(ct), ncol=ncol(mat))
      colnames(tmp) <- colnames(mat)
      rownames(tmp) <- ct
    }
    tmp2 <- matrix(colMeans(tmp),nrow=1)
    colnames(tmp2) <- colnames(tmp)
    rownames(tmp2) <- sprintf("CL%s",cl)
    tmpM <- rbind(tmpM,tmp2)
  
    tmp <- data.frame(tmp)
    tmp$cluster <- cl
    tmpm <- rbind(tmp, tmpm)
  }

  print("Cluster profiles by binary values")
  print(tmpb)
  print("Cluster profiles by mean values")
  print(tmpm)
  print("Means by cluster")
  print(round(tmpM,2))
  
  fname <- sprintf("clustering-%s-%s-%d.xlsx",type,ifelse(binary,"binary","means"),nclusters)
  if(binary){
    openxlsx::write.xlsx(list(tmpb,tmpm,tmpM),rowNames=TRUE,
                       sheetName= list("binary","means","means by cluster"),
                       file=fname)
  } else {
    openxlsx::write.xlsx(list(tmpm,tmpM),rowNames=TRUE,
                         sheetName= list("means","means by cluster"),
                         file=fname)
  }
  
}

analysis(MAT5,binary=FALSE,nclusters=8,type="codes", width=9, height=12)
analysis(MAT5,binary=TRUE,nclusters=8,type="codes", width=9, height=12) # questo per paper

analysis(MAT6,binary=FALSE,nclusters=8,type="sources", width=9, height=12)
analysis(MAT6,binary=TRUE,nclusters=8,type="sources", width=9, height=12)

 
analysis(MAT3,binary=FALSE,nclusters=8,type="factors", width=9, height=12)
analysis(MAT3,binary=TRUE,nclusters=8,type="factors", width=9, height=12)


































 

