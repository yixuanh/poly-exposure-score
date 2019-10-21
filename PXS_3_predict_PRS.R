
##predict in group c 

library(data.table)
library(fst)
library(pROC)
library(ggplot2)
library(stringr)
library(scales)

envdir<-'/n/groups/patel/yixuan/PRS_ERS_DRS/XWAS/B_stepwise/'

IDlist<-read.table("/n/groups/patel/yixuan/PRS_ERS_DRS/IDs/ID_C.txt",header=TRUE)
df<-read_fst("/n/groups/patel/yixuan/PRS_ERS_DRS/processed/post_pc.fst")
df<-data.frame(df)
df<-df[which(df$f.eid %in% IDlist$FID),]
colnames(df)[1]<-'IID'

names<-sub(".*?f.","",colnames(df))
names<-gsub("\\..*","",names)

d='t2d'
signif<-fread(paste(envdir,d,'_coeffs.csv',sep='')) 
colnames(signif)[1:2]<-c('name','coeff')

sig<-sub(".*?f.","",signif$name)
sig<-gsub("\\..*","",sig)

sig<-sig[!duplicated(sig)]
keep<-which(names %in% sig)
dis<-which(colnames(df)==d)
keep<-data.frame(df[,c(1,dis,keep)]) 

keep<-keep[rowSums(keep == "Prefer not to answer")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-3")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-10")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-1")==0, , drop = FALSE]

keep<-na.omit(keep)
keep<-keep[vapply(keep, function(x) length(unique(x)) > 1, logical(1L))] 

cat<-sapply(colnames(keep), function(x) class(df[[x]]))
cat<-data.frame(cat)

listcat<-which(cat[2,]=="factor")

f = function(x) {
  temp<-data.frame(x)
  temp<-data.frame(na.omit(x))
  score=0
  for (j in 2:nrow(temp)){ 
    if (j %in% listcat){
      v<-paste(rownames(temp)[j],temp[,1][j],sep='')
      n<-which(signif$name==v)
      if (length(n)!=0){
        score=score+signif$coeff[n]
      }}
    else{
      v<-rownames(temp)[j]
      n<-which(signif$name==v)
      if (length(n)!=0){
        score=score+as.numeric(as.character((temp[,1][j])))*signif$coeff[n]
             }
    }
  }
  return(score)
}

saving<-apply(keep,1,f)
save<-data.table(keep$IID,saving)

colnames(save)=c('IID',paste(d,'_Xscore',sep=''))
write.table(save,paste(d,'.csv',sep=''),sep=',',row.names=FALSE)
