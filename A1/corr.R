
corr<- function(directory,treshold=0) {

df_complete<- complete(directory)
files<- list.files(directory)

df_cor<- data.frame(ID=integer(),
                    corr=numeric())

ID_above_th<- df_complete[df_complete$nobs>treshold,1]

for (i in ID_above_th)
  {
  df_filedata_temp<- read.csv(paste(directory,files[i],sep="/"))
  df_filedata_temp<- df_filedata_temp[complete.cases(df_filedata_temp),]
  df_cor_tmp<- data.frame(ID=i,corr=cor(
    df_filedata_temp$sulfate,df_filedata_temp$nitrate))
  df_cor=rbind(df_cor,df_cor_tmp)
  }
v_cor<- df_cor[,2]
v_cor
}


