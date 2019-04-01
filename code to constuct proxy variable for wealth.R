
##  code for 'Wealth Data' Analysis

D<- read.csv("C:/Users/hp/Desktop/problem_statement_wealth/h_wealth.csv")
dim(D)
#summary(D)
#601509*37


### Checking for NA's
A<-c()
for(i in 1:ncol(D))
{
  A[i]<-length(which(is.na(D[,i])))
}
A


D1<-D[,-3]    #removing variable with very high missing value
dim(D1)
#View(D1)


## To remove household with other as  one responce
length(which(D1==96))

for(i in 1:ncol(D1))
{
  Other<-which(D1[,i]==96)
  if(length(Other)>0)
  D1<-D1[-(Other[1:length(Other)]),]
  else
    D1=D1
}

dim(D1)
length(which(D1==96))



# To aggregate multiple levels into 2 level for variable having multiple levels

names(D1)

##1
water_wealthy<-c(11,12,21,31,71,72)
water_poor<-c(13,32,41,42,43,51,61,62)
Ind1W<-c()
for(i in 1:length(water_wealthy))
{
  Ind1W<-c(Ind1W,which(D1[,1]==water_wealthy[i]))
}
waterD<-D1[,1]
Ind0W<-setdiff(1:nrow(D1),Ind1W)
A<-replace(waterD,Ind1W,1)
Wat<-replace(A,Ind0W,0)


##2
toilet_wealthy<-c(11:15,21,41,44)
toilet_poor<-c(22,23,31)
Ind1T<-c()
for(i in 1:length(toilet_wealthy))
{
  Ind1T<-c(Ind1T,which(D1[,2]==toilet_wealthy[i]))
}
toiletD<-D1[,2]
Ind0T<-setdiff(1:nrow(D1),Ind1T)
A<-replace(toiletD,Ind1T,1)
Toil<-replace(A,Ind0T,0)

##3
floor_wealthy<-c(21,22,23,24,31:36)
floor_poor<-c(11,12,13)
Ind1F<-c()
for(i in 1:length(floor_wealthy))
{
  Ind1F<-c(Ind1F,which(D1[,9]==floor_wealthy[i]))
}
floorD<-D1[,9]
Ind0F<-setdiff(1:nrow(D1),Ind1F)
A<-replace(floorD,Ind1F,1)
Floor<-replace(A,Ind0F,0)


##4
wall_wealthy<-c(31:36)
wall_poor<-c(11:14,21:26)
Ind1L<-c()
for(i in 1:length(wall_wealthy))
{
  Ind1L<-c(Ind1L,which(D1[,10]==wall_wealthy[i]))
}
wallD<-D1[,10]
Ind0L<-setdiff(1:nrow(D1),Ind1L)
A<-replace(wallD,Ind1L,1)
Wall<-replace(A,Ind0L,0)


##5
roof_wealthy<-c(31:39)
roof_poor<-c(11:15,21:25)
Ind1R<-c()
for(i in 1:length(roof_wealthy))
{
  Ind1R<-c(Ind1R,which(D1[,11]==roof_wealthy[i]))
}
roofD<-D1[,11]
Ind0R<-setdiff(1:nrow(D1),Ind1R)
A<-replace(roofD,Ind1R,1)
Roof<-replace(A,Ind0R,0)


##6
fuel_wealthy<-c(1,2,95)
fuel_poor<-c(4:11)
Ind1<-c()
for(i in 1:length(fuel_wealthy))
{
  Ind1<-c(Ind1,which(D1[,13]==fuel_wealthy[i]))
}
fuelD<-D1[,13]
Ind0<-setdiff(1:nrow(D1),Ind1)
A<-replace(fuelD,Ind1,1)
Fuel<-replace(A,Ind0,0)


D2<-cbind(D1[,-c(1,2,9,10,11,13)],water=Wat,toilet=Toil,floor=Floor,wall=Wall,roof=Roof,fuel=Fuel)
dim(D2)      ## This final data we use for further analysis
#View(D2)
var<-names(D2)

##  Exploratory 

per<-matrix(nrow = 36,ncol = 2)
for(i in 1:ncol(D2))
{
  per[i,]<-c((((length(which(D2[,i]==1)))/nrow(D2))*100)
             ,(((length(which(D2[,i]==0))/nrow(D2))*100)))
}

colnames(per)=c("% of households with this amenity","% of households without this amenity")
rownames(per)<-var
Percentage<-(round(per,2))
View(Percentage)
# To export
write.table(Percentage,"C:\\Users\\hp\\Desktop\\problem_statement_wealth\\Exploratory\\Percentage data.xls",row.names=FALSE,sep = "\t")
#View(Percentage)

#### Creating proxy variable to represent the wealth of households 
var
wt<-c(1,1,5,1,5,50,rep(1,8),5,1,5,5,5,50,5,1,5,5,50,50,1,5,50,50,10,10,10,10,10,1)/415
cbind(var,round(wt,4))

sum(wt)
D3<-matrix(nrow = nrow(D2),ncol = 36)
for(i in 1:36)
{
  D3[,i]=D2[,i]*wt[i]
}
#View(D2)
##################  score
SC<-rowSums(D3)
D4<-cbind(D2,Score=SC)
#View(D4)
attach(D4)
write.table(D4,"C:\\Users\\hp\\Desktop\\problem_statement_wealth\\Data with scores.xls",row.names=FALSE,sep="\t")


########################################  boxplot of data
boxplot(SC)
Q<-quantile(SC,c(0.25,0.5,0.75))
Q
## Outlier detection 

cutoff<-Q[3]+(1.5*(Q[3]-Q[1]));cutoff

Wealthy_households<-which(SC>=cutoff)
#Wealthy_households
length(Wealthy_households)


sample_data<-sample(1:nrow(D2),577492)
coL<-rep(1,577492)
coL[which(SC[sample_data]>=cutoff)]=2
plot(1:577492,SC[sample_data],col=coL,xlab = "Households",ylab = "Score representing wealth of household",main = "Visualization of wealth of households")


################################## Exporting barplot with error for animation

library(ggplot2)

setwd("C:/Users/hp/Desktop/problem_statement_wealth/ggplotImages")

var_names<-colnames(D2)
b1<-list()
for(i in 1:length(var_names))
{
  P1<-paste(i)
  P2<-paste(P1,".jpeg",sep="")
  SC1=SC[which(D2[,i]==1)];n1=length(SC1)-1
  SC2=SC[which(D2[,i]==0)];n2=length(SC2)-1
  
  xbar=mean(SC1)
  ybar=mean(SC2)
  data <- data.frame(Households=c(paste("with",var_names[i]) ,paste("without",var_names[i])),Score=c(xbar,ybar)
                     ,sd=c(2.575829*sqrt(var(SC1)/n1),2.575829*sqrt(var(SC2)/n2)))
  b1[[i]]<-ggplot(data) +
    geom_bar( aes(x=Households, y=Score),stat="identity", fill="green", alpha=0.5) +
    geom_errorbar( aes(x=Households, ymin=Score-sd, ymax=Score+sd), width=0.4, colour="red", alpha=0.9, size=1.3) +
    coord_flip()
  ggsave(P2)
  unlink(b1[[i]])
}


## Exporting boxplot for animation (not shown in report)

setwd("C:/Users/hp/Desktop/problem_statement_wealth/Images")

var_names<-colnames(D2)
No_Positive_Cases<-c()
b1<-list()
for(i in 1:length(var_names))
{
  
  No_Positive_Cases[i]=length(which(D2[i]==1))
  P1<-paste(i)
  P2<-paste(P1,".jpeg",sep="")
  jpeg(filename=P2)
  par(mfrow=c(1,2))
  b1[[i]]<-boxplot(SC[which(D2[,i]==1)],col="green",ylab="Score for wealth of households",main=paste("households with",var_names[i]))
  b1[[i]]<-boxplot(SC[which(D2[,i]==0)],col="red",ylab="Score for wealth of households",main=paste("households without",var_names[i]))
  dev.off()
}

############   Testing equality of two mean

library(BSDA)

pval<-c()
for(i in 1:36)
{
  I<-which(D2[,i]==1)
  x<-SC[I]
  y<-SC[-I]
  Test<-z.test(x,y,sigma.x = sd(x),sigma.y = sd(y),alternative = "two.sided",conf.level = 0.99)
  pval[i]<-Test$p.value
  
}
pval
Bonferroni_p_val<-p.adjust(pval,method = "bonferroni",n=length(pval))

ptable<-cbind(colnames(D2),pval,Bonferroni_p_val)
colnames(ptable)<-c("Variable","p-value","Bonferroni adjusted p-value")
ptable

