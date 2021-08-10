# 
# # package_path<-"C:/ProbOpData_packages"
# # if (!dir.exists(package_path)){
# #   dir.create(package_path)
# # }
# pkgs<-installed.packages()
# 
# required_packages<-c("shiny","ggplot2","KernSmooth","pryr","pracma","roxygen2",
#                      "rJava","zoo","data.table","lubridate","stringi","plot3D",
#                      "gridExtra","directlabels","colorRamps","MASS","xlsx",
#                      "ash","bit64","plyr","geometry","dygraphs","dplyr","scatterplot3d","sfsmisc","xts","jsonlite",
#                      "httr","curl","zoo","doSNOW","h5","rhdf5")
# 
# repo_path<-rep("default",length(required_packages))
# repo_path[which(required_packages=="rhdf5")]<-'custom'
# check_pk<-NULL
# for (i in 1:length(required_packages)){
#   check_pk<-which(pkgs[,1]==required_packages[i])
#   
#   if (length(check_pk)==0){
#     if (repo_path[i]=='default'){
#       install.packages(required_packages[i],dependencies = TRUE)
#     }else if(repo_path[i]=='custom'){
#       if(required_packages[i]=='rhdf5'){
#         install.packages("BiocManager",dependencies = TRUE)
#         library(BiocManager)
#         BiocManager::install("rhdf5")  
#         
#       }
#     }
#   }
#   
# }
# print('Packages up-to-date')


install.packages("shiny")
install.packages("ggplot2")
install.packages("KernSmooth")
install.packages("pryr")
install.packages("pracma")
install.packages("roxygen2")
install.packages("rJava")
install.packages("zoo")
install.packages("data.table")
install.packages("lubridate")
install.packages("stringi")
install.packages("plot3D")
install.packages("gridExtra")
install.packages("directlabels")
install.packages("colorRamps")
install.packages("MASS")
install.packages("xlsx")
install.packages("ash")
install.packages("bit64")


# --- needed for rbind.fill
install.packages("plyr")

#---- needed for Ivan's ECDF_KernelEstim
install.packages("MASS")

install.packages("geometry")
install.packages("dygraphs")
install.packages("dplyr")
install.packages("scatterplot3d")

install.packages("sfsmisc")

