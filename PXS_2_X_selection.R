library(data.table)
library(fst)
library(stringi)
library(pROC)
library(tidyverse)

disease<-'t2d'


IDlist<-read.table("ID_B.txt",header=TRUE)
df<-read_fst("post_pc.fst")
df<-data.frame(df)

df<-df[which(df$f.eid %in% IDlist$FID),]

names<-sub(".*?f.","",colnames(df))
names<-gsub("\\..*","",names)

dis<-fread(paste(disease,'.csv',sep=''))

dis<-dis[order(dis$adjusted_pval),]

disnames<-sub(".*?f.","",dis$V1)
disnames<-gsub("\\..*","",disnames)
disnames<-disnames[!duplicated(disnames)]

keep<-which(names == disnames[1])
for (i in 2:length(disnames)){
  keep<-c(keep,which(names == disnames[i]))
}
index<-which(colnames(df)==disease)
keep<-df[,c(index,7:48,keep)]

cat<-sapply(colnames(keep), function(x) class(keep[[x]]))
cat<-data.frame(cat)


##fix factoring
listcat<-which(cat[2,]=="factor")
for (i in listcat){
  keep[,i]<-factor(keep[,i],ordered=FALSE)}

for (i in listcat){
  keep[,i]<-fct_rev(keep[,i])
}
index<-0


keep<-keep[rowSums(keep == "Prefer not to answer")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-3")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-10")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-1")==0, , drop = FALSE]

keep<-na.omit(keep)
keep<-keep[vapply(keep, function(x) length(unique(x)) > 1, logical(1L))] # those with only unique values

print(colnames(keep)[44])
store_matrix<-matrix(0,nrow=1,ncol=3)

store<-c(0,0,0)

for (i in (45:ncol(keep))){
  temp<-keep[,c(1:43,44,i)]
  k<-table(temp[,ncol(temp)])
  k<-length(which(k!=0))
  cat<-sapply(colnames(temp), function(x) class(temp[[x]]))
  if (cat[length(cat)]!='factor'){
    k=2
  }
  fit<-glm(temp[,1]~.,data=temp[,-1],family='binomial')
  pval<-data.frame(summary(fit)$coefficients)
  pvals<-pval[(nrow(pval)-(k-2)):nrow(pval),3:4]
  print(pvals)
  print(i)
  store<-rbind(store,pvals)}

save<-store[rowSums(store==0, na.rm=TRUE)<ncol(store), ]
save[,2]<-p.adjust(save[,2],method='fdr')

save<-save[save[,2]<0.05,]
sig<-save[order(save[,2]),]

##round i
for (round in 2:50){
  print(round)
  disnames<-sub(".*?f.","",row.names(sig))
  disnames<-gsub("\\..*","",disnames)
  disnames<-disnames[!duplicated(disnames)]
  disnames<-c(924,disnames) ######change 1538 if cad and ibd, 924 if af or t2d
  
  keep<-which(names == disnames[1])
  for (i in 2:length(disnames)){
    keep<-c(keep,which(names == disnames[i]))
  }
  index<-which(colnames(df)==disease)
  keep<-df[,c(index,7:48,keep)]
  
  cat<-sapply(colnames(keep), function(x) class(keep[[x]]))
  cat<-data.frame(cat)
  
  
  ##fix factoring
  listcat<-which(cat[2,]=="factor")
  for (i in listcat){
    keep[,i]<-factor(keep[,i],ordered=FALSE)}
  for (i in listcat){
    keep[,i]<-fct_rev(keep[,i])
  }
  
  keep<-keep[rowSums(keep == "Prefer not to answer")==0, , drop = FALSE]
  keep<-keep[rowSums(keep == "-3")==0, , drop = FALSE]
  keep<-keep[rowSums(keep == "-10")==0, , drop = FALSE]
  keep<-keep[rowSums(keep == "-1")==0, , drop = FALSE]
  
  keep<-na.omit(keep)
  keep<-keep[vapply(keep, function(x) length(unique(x)) > 1, logical(1L))] # those with only unique values
  
  print(colnames(keep)[44+(round-1)])
  
  store<-c(0,0,0)
  
  for (i in (45+(round-1)):ncol(keep)){
    temp<-keep[,c(1:(43+round),i)]
    k<-table(temp[,ncol(temp)])
    k<-length(which(k!=0))
    cat<-sapply(colnames(temp), function(x) class(temp[[x]]))
    if (cat[length(cat)]!='factor'){
      k=2
    }
    fit<-glm(temp[,1]~.,data=temp[,-1],family='binomial')
    pval<-data.frame(summary(fit)$coefficients)
    pvals<-pval[(nrow(pval)-(k-2)):nrow(pval),3:4]
    print(i)
    store<-rbind(store,pvals)}
  
  store<-store[rowSums(store==0, na.rm=TRUE)<ncol(store), ]
  store[,2]<-p.adjust(store[,2],method='fdr')
  
  store<-store[store[,2]<0.05,]
  sig<-rbind(sig[1:(round-1),],store[order(store[,2]),])}

k<-dis$V1[1]
k<-c(k,row.names(sig))
write.csv(paste(disease,'.tab',sep=''),sep='\t')
                
##get coefficients
dis<-k

disnames<-sub(".*?f.","",dis$x)
disnames<-gsub("\\..*","",disnames)
disnames<-disnames[!duplicated(disnames)]

keep<-which(names == disnames[1])
for (i in 2:length(disnames)){
    keep<-c(keep,which(names == disnames[i]))
}
index<-which(colnames(df)==disease)
keep<-df[,c(1,index,7:48,keep)]

cat<-sapply(colnames(keep), function(x) class(keep[[x]]))
cat<-data.frame(cat)


##fix factoring
listcat<-which(cat[2,]=="factor")
for (i in listcat){
    keep[,i]<-factor(keep[,i],ordered=FALSE)}
for (i in listcat){
    keep[,i]<-fct_rev(keep[,i])
}
 
keep<-keep[rowSums(keep == "Prefer not to answer")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-3")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-10")==0, , drop = FALSE]
keep<-keep[rowSums(keep == "-1")==0, , drop = FALSE]

keep<-na.omit(keep)
keep<-keep[vapply(keep, function(x) length(unique(x)) > 1, logical(1L))] # those with only unique values

levels(keep$f.31.0.0) <- c(levels(keep$f.31.0.0),'No')


B<-which(keep$f.eid %in% IDlist$FID)
fit<-glm(keep[B,2]~.,data=keep[B,-(1:2)],family='binomial')

coeffs<-data.frame(summary(fit)$coefficients)
coeffs<-coeffs[-(1:43),]
write.csv(coeffs,paste(disease,'_coeffs.csv',sep=''))
