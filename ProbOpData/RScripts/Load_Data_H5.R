## Load H5 data file

setwd(Input_Turbine_DATA_Explorer_CSV_Path)
h5f = H5Fopen(INPUT_PROJECT_INPUT_H5_FILE)

all_sig<-h5f$Signal_Data

H5Fclose(h5f)

col_names<-names(all_sig)
col_names<-col_names[-which(col_names=="MY_DATE_NUM")]

num_col<-length(col_names)
sort_vec<-paste0("V",seq(1,num_col))
sort_vec<-c("MY_DATE_NUM",sort_vec)

# col_names_num<-unlist(lapply(col_names,function(x)return(gsub("V","",x,fixed=TRUE))))

PROJECT_DATA_VXX<-as.data.table(all_sig[sort_vec])

# PROJECT_DATA_VXX[, MY_DATE:= as.POSIXct(MY_DATE_NUM.V1, origin="1970-01-01",tz="UTC")]
# PROJECT_DATA_VXX[, MY_DATE_NUM.V1 := NULL]
# if (nchar(PROJECT_DATA_VXX$MY_DATE_NUM[1])==12){
#   PROJECT_DATA_VXX[, MY_DATE:= as.POSIXct(MY_DATE_NUM/100, origin="1970-01-01",tz="UTC")]
#   PROJECT_DATA_VXX[, MY_DATE_NUM := NULL]
#   
# }else{
  PROJECT_DATA_VXX[, MY_DATE:= as.POSIXct(MY_DATE_NUM, origin="1970-01-01",tz="UTC")]
  PROJECT_DATA_VXX[, MY_DATE_NUM := NULL]
  
# }

cn<-c()
for (i in 1:(dim(PROJECT_DATA_VXX)[2]-1)){
  cn<-c(cn,paste0('V',i))
  if (length(which(is.infinite(PROJECT_DATA_VXX[[i]])))>0){
    PROJECT_DATA_VXX[[i]][which(is.infinite(PROJECT_DATA_VXX[[i]]))]<-NA
  }
}
colnames(PROJECT_DATA_VXX)<-c(cn,"MY_DATE")
TOTAL_OPERATION_HOURS_OF_THIS_STEP <- (dim(PROJECT_DATA_VXX)[1]/3600) * INPUT_MY_FREQUENCY
rm(all_sig)
gc()