library('arules')
#library("parallel")
#library("foreach")
#library("doMC")
#registerDoMC(cores=2)
#data("mtcars")

#reading the data
dat<-read.csv("../swarup/Desktop/adult_umlaut.csv",encoding = "UTF-8")

#making the data into factor type
dat<-as.data.frame(sapply(dat,as.factor))
rule_calc<-function(dat,sprt,maxs){
  dat1<-as(dat,"transactions")
  tar_var<-names(dat)[ncol(dat)]
  idx<-which(itemFrequency(dat1)>=sprt | itemInfo(dat1)$variables==tar_var)  
  dat2<-dat1[,idx]
  
  #Defining the lhs and rhs of the data
  lhs1<-dat2@itemInfo$labels[dat2@itemInfo$variables!=tar_var]
  rhs1<-dat2@itemInfo$labels[dat2@itemInfo$variables==tar_var]
  
  
  #Calclulating all the item sets with support>0.001
  s1<-apriori(dat2,parameter = list(support = sprt, target = "frequent",maxlen=maxs),appearance = list(items=lhs1,default="none") )
  
  #Defining the rhs
  s2<-apriori(dat2,parameter = list(support = 0, target = "frequent",maxlen=1),appearance = list(items=rhs1,default="none") )
  
  #Generating the rules from the itemsets 
  ruleSet<-new("rules",lhs=items(s1),rhs=sample(items(s2)[1],size = length(s1),replace=TRUE),info=info(s1))
  ruleSet2<-new("rules",lhs=items(s1),rhs=sample(items(s2)[2],size = length(s1),replace=TRUE),info=info(s1))
  ruleSet<-c(ruleSet,ruleSet2)
  
  #Generating  the rule parameters
  cat("\n calculating support \n")
  i=system.time(ruleSet@quality<-data.frame(support=support(items(ruleSet),transactions = dat2)))
  print(i)
  ruleSet@quality$lhs.support<-c(s1@quality$support,s1@quality$support)
  ruleSet@quality$confidence<-ruleSet@quality$support/ruleSet@quality$lhs.support
  ruleSet@quality$confidence[is.na(ruleSet@quality$confidence)]<-0
  cat("\n calculating lift \n")
  i=system.time(ruleSet@quality$lift<-interestMeasure(ruleSet,"lift",transactions = dat2))
  print(i)
  cat("\n converting to a data frame \n")
  i=system.time(df1<-data.frame(condition=labels(lhs(ruleSet),itemSep=", "), consequence = labels(rhs(ruleSet),itemSep=", "), quality(ruleSet)))
  print(i)
  return(list(ruleSet,df1))
}
#Fomating the output
system.time(df1<-data.frame(condition=labels(lhs(ruleSet),itemSep=", "), consequence = labels(rhs(ruleSet),itemSep=", "), quality(ruleSet)))

#Stress testing the automatic rule generation for bugs
maxi<-0

#defining the start time
strt_time<-Sys.time()
for(i in 1:50){
  #generating a random number to subset the lhs
  a<-round(runif(1,1,3),0)
  print(paste0("iteration number: ",i))
  print(paste0("total/a a =:",a))
  #applying the automatic version of apriori
  system.time(s1<-apriori(dat,parameter = list(supp = runif(n = 1,min=0,max=0.2), conf = 0, target = "rules",minlen=1,maxlen=round(runif(1,2,20),0),ext=TRUE),appearance = list(rhs=rhs1,lhs=lhs1[sample(1:length(lhs1),round(length(lhs1)/a,0))],default="none")))
  #if the rule set is non empty then find the maximum number of rules
  if(length(s1)>0)
  inspect(head(s1))
  b<-length(s1)
  if(b>maxi){
    maxi<-b
  }
  print("done")
}
end_time<-Sys.time()
elapsed_time<-end_time-strt_time
print(maxi)


