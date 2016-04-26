# Remove " and use ' to covert ft to inches

ft2inAps <- function(input){

for(i in 1:length(input)){
  if(grepl("'",input[i])==TRUE){
    gsub("'","",input[i])
    input[i] <- as.numeric(as.character(input[i]))
    input[i]<- input[i]*12
  }
  if(grepl("\"",input[i])==TRUE){
    gsub("\"","",input[i])
     }
}}