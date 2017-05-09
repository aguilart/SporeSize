
#Function to replace the given names of the study by accepeted names according to Schlusser website

secondTrial<-
function(p1,p2){
  temporal<-p1[match(p2[,1],p1[,2]),1]
  
  correctOnes<-temporal[which(!is.na(p1[match(p2[,1],p1[,2]),1]))]
  
  p2[p2[,1]%in%p1[,2],1]<-correctOnes
  
  return(p2)
  
}
