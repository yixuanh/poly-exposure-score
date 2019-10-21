
library(data.table)
library(fst)
library(stringi)

#get weights for PDS
IDlist<-read.table("ID_A.txt",header=TRUE)
demdir<-("/Dem/")

df<-read_fst("post_pc.fst")
df<-data.frame(df)

df<-df[which(df$f.eid %in% IDlist$FID),]

#generate data frames that are in the form disease, sex, age, PC1:40
af<-df[,c(2,7:48)] 
cad<-df[,c(3,7:48)]
t2d<-df[,c(5,7:48)]
ibd<-df[,c(4,7:48)]

af_fit<-glm(af[,1]~.,data=af[-1],family = "binomial")
cad_fit<-glm(cad[,1]~.,data=cad[-1],family = "binomial")
t2d_fit<-glm(t2d[,1]~.,data=t2d[-1],family = "binomial")
ibd_fit<-glm(ibd[,1]~.,data=ibd[-1],family = "binomial")

write_af<-summary(af_fit)$coefficients
write_cad<-summary(cad_fit)$coefficients
write_t2d<-summary(t2d_fit)$coefficients
write_ibd<-summary(ibd_fit)$coefficients

write.table(write_af,'Dem/af.csv',sep=',')
write.table(write_cad,'cad.csv',sep=',')
write.table(write_t2d,'Dem/t2d.csv',sep=',')
write.table(write_ibd,'Dem/ibd.csv',sep=',')

##get PDS in group C
IDlist<-read.table("ID_C.txt",header=TRUE)
df<-read_fst("post_pc.fst")
df<-data.frame(df)

df<-df[which(df$f.eid %in% IDlist$FID),]
IDlist<-df$f.eid

df<-df[,c(7:48)]
df$f.34.0.0<-as.numeric(df$f.34.0.0)

d='t2d'
betas<-fread(paste(demdir,d,'.csv',sep='')) 
colnames(betas)[1]<-'name'
betas<-betas[-1,]

df$Dscore<-0

for (i in 1:nrow(df)){
  temp<-df[i,]
  score=0
  
  for (j in c(1:42)){ 
    if (j!=41){
      score=score+temp[1,j]*betas$Estimate[j]
    }else{
      if (as.character(temp[1,j])=='Male'){
        score=score+betas$Estimate[j]
      }
    }
  }
  df$Dscore[i]<-score
}

save<-data.table(IDlist,df$Dscore)
colnames(save)=c('IID',paste(d,'_Dscore',sep=''))
write.table(save,paste(d,'_Dscore.csv',sep=''),sep=',',row.names=FALSE)  
