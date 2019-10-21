library(data.table)
library(fst)
library(stringi)
library(pROC)
library(tidyverse)

disease='t2d'

IDlist<-read.table("ID_A.txt",header=TRUE)
df<-read_fst("post_pc.fst")
df<-data.frame(df)

df<-df[which(df$f.eid %in% IDlist$FID),]
df$f.3062.0.0<-NULL

##
cat<-sapply(colnames(df), function(x) class(df[[x]]))
cat<-data.frame(cat)

##CATEGORICAL
listcat<-which(cat[2,]=="factor")
dfcat<-df[,c(2:48,listcat)] #combine age, PC1:40 with categorical responses
dfcat$f.eid<-NULL

##NUMERICAL
listnum<-which(cat[2,]=="numeric"|cat[2,]=="integer")
dfnum<-df[,c(47,listnum)]  #combine sex with numerical responses

#make reg matrixcat
regmatrix_cat<-matrix(0,ncol=5,nrow=ncol(dfcat)*5)
colnames(regmatrix_cat)<-c("name","coeff","auc","pval","adjusted_pval")
regmatrix_num<-matrix(0,ncol=5,nrow=ncol(dfnum))
colnames(regmatrix_num)<-c("name","coeff","auc","pval","adjusted_pval")

dis_cat<-which(colnames(dfcat)==disease)
###fill in matrix
##categorical


for (i in 48:ncol(dfcat)){
  dfcat[,i]<-fct_rev(dfcat[,i])
}
index<-0

for (i in 48:ncol(dfcat)){
  stored<-cbind(dfcat[,c(dis_cat,6:47,i)]) ##change here
  stored<-na.omit(stored)
  
  stored[,ncol(stored)]<-factor(stored[,ncol(stored)],ordered=FALSE)
  remove<-which(stored[,ncol(stored)]=='Prefer not to answer')
  
  if (length(remove)!=0){
    stored<-stored[-(remove),]
  }
  
  fit<-glm(stored[,1]~.,data=stored[-1],family = "binomial")
  #get AUC
  prob=predict(fit,type=c("response"))
  stored$prob=prob
  g <- roc(stored[,1] ~ prob, data = stored)
  auc<-gsub('.*: ',"",g$auc)
  
  #get coeff and p val
  summary_fit<-summary(fit)$coefficients[44:(nrow(summary(fit)$coefficients)),]
  summary_fit<-as.data.frame(summary_fit)
  
  ##binary response
  if (ncol(summary_fit)==1){
    pval<-summary_fit[4,1]
    coeff<-summary_fit[1,1]
    regmatrix_cat[index,1]<-colnames(dfcat)[i]
    regmatrix_cat[index,2]<-coeff
    regmatrix_cat[index,4]<-pval
    regmatrix_cat[index,3]<-auc
  }
  ##non-binary response
  else {
    for (j in 1:nrow(summary_fit)){
      index<-index+1
      pval<-summary_fit[j,4]
      coeff<-summary_fit[j,1]
      regmatrix_cat[index,1]<-row.names(summary_fit)[j]
      regmatrix_cat[index,2]<-coeff
      regmatrix_cat[index,4]<-pval
      regmatrix_cat[index,3]<-auc
    }
  }
  print (i)
}

regmatrix_cat<-regmatrix_cat[regmatrix_cat[,1]!=0,]
rownames(regmatrix_cat) <- regmatrix_cat[,1]
regmatrix_cat<-regmatrix_cat[,-1]
pvals<-regmatrix_cat[,3]
regmatrix_cat[,4]<-p.adjust(pvals,"fdr")
regmatrix_cat<-data.frame(regmatrix_cat)
regmatrix_cat_keep<-regmatrix_cat[as.numeric(as.character(regmatrix_cat$adjusted_pval))<0.05,]

dis_num<-which(colnames(dfnum)==disease)
##numerical
for (i in 47:(ncol(dfnum))){
  stored<-data.frame(dfnum[,c(dis_num,1,6:46,i)])##change here
  stored<-na.omit(stored)
  remove<-which(stored[,ncol(stored)]=='-3')
  remove<-c(remove,which(stored[,ncol(stored)]=='-1'))
  
  if (length(remove)!=0){
    stored<-stored[-(remove),]
  }
  fit<-glm(stored[,1]~.,data=stored[-1],family = "binomial")
  #get AUC
  prob=predict(fit,type=c("response"))
  stored$prob=prob
  g <- roc(stored[,1] ~ prob, data = stored)
  auc<-gsub('.*: ',"",g$auc)
  
  summary_fit<-summary(fit)$coefficients[(nrow(summary(fit)$coefficients)),]
  pval<-summary_fit[4]
  coeff<-summary_fit[1]
  regmatrix_num[i-46,1]<-colnames(dfnum)[i]
  regmatrix_num[i-46,2]<-coeff
  regmatrix_num[i-46,4]<-pval
  regmatrix_num[i-46,3]<-auc
  print (i)
}

regmatrix_num<-regmatrix_num[regmatrix_num[,1]!=0,]
rownames(regmatrix_num) <- regmatrix_num[,1]
regmatrix_num<-regmatrix_num[,-1]
pvals<-regmatrix_num[,3]
regmatrix_num[,4]<-p.adjust(pvals,"fdr")
regmatrix_num<-data.frame(regmatrix_num)
regmatrix_num_keep<-regmatrix_num[as.numeric(as.character(regmatrix_num$adjusted_pval))<0.05,]

##keep all for XWAS plot
save_all<-rbind(regmatrix_cat,regmatrix_num)
rownames(save_all) <- gsub(",", "", rownames(save_all), fixed = TRUE)
write.table(save_all,paste(disease,'_all.csv',sep=''),sep=',',quote=FALSE)

##keep only those that pass threshhold
save<-rbind(regmatrix_cat_keep,regmatrix_num_keep)
rownames(save) <- gsub(",", "", rownames(save), fixed = TRUE)

write.table(save,paste(disease,'.csv',sep=''),sep=',',quote=FALSE)
