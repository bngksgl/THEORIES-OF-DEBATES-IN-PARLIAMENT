dataset=load("C:/Users/Bengi/Documents/OKUL/OKUL-IS-ERASMUS/OKUL/Master/Multivariate Analysis/Data Essat/Dataessay.RData")
#Looking at the statistics- Starting from UKData
library(stargazer)
#getting summary statistics
summary(UKdata)
#Visualizing the dataset with x 
hist(x=UKdata$conservative_MP, xlab="Conservative MP",main="")
hist(x=UKdata$number_speeches,xlab="Number of Speeches",main="")
hist(x=UKdata$party_leader,xlab="Party Leader",main="")
hist(x=UKdata$ideological_distance,xlab="Ideological Distance",main="")


#visualizing the dataset with y
y=UKdata$number_speeches
col_ger=colnames(UKdata)
#par(mfrow=c(1, 1))
par(mfrow=c(1, 3))
i=2
maxrow=length(col_ger)+1
while(i<maxrow){
  k=col_ger[i]
  print(k)
  plot(x=UKdata[[k]], y=y,ylab="Number of Speeches",xlab = k)
  i=i+1
}
aggregate(UKdata$number_speeches, by=list(UKdata$conservative_MP), FUN=mean)
aggregate(UKdata$number_speeches, by=list(UKdata$party_leader), FUN=mean)
aggregate(UKdata$number_speeches, by=list(UKdata$ideological_distance), FUN=mean)

#As can be understood from the summary there were some missing values. 
#looking more closely to ideological distance missingness and searching for pattern.
UKdata[rowSums(is.na(UKdata)) > 0,]
hist(UKdata$ideological_distance)
#We couldnt find a specific pattern
#So we are going to handle missingness by assigning values with median, due to the fact that dataset has outliers.
UKdata$ideological_distance[is.na(UKdata$ideological_distance)] <- median(UKdata$ideological_distance, na.rm=TRUE)


#Also lets ook at outliers for ideological distance and number of speeches
#outlier values.
par(mfrow=c(1, 1))
outlier_dis <- boxplot.stats(UKdata$ideological_distance)$out
outlier_numsp <- boxplot.stats(UKdata$number_speeches)$out
boxplot(UKdata$number_speeches, main="Number of Speechs in U.K. Parlimant", boxwex=0.1)
boxplot(UKdata$ideological_distance, main="Ideological Distances in U.K. Parliament", boxwex=0.1)

#From the box plot it can be seen that there are some outlier in ideological distances
#So here we are going to try modeling by both excluding the outliers and then then keeping the outliers
#excluding these outliers
UK_data_no_outlier=subset(UKdata, ! number_speeches %in% outlier_numsp)
UK_data_no_outlier_1=subset(UKdata, ! ideological_distance %in% outlier_dis)

#function checking whether to use negative binomial or poisson
check.nb.pos<-function(modelpos,modelnb){
  L1 <- logLik(modelpos) 
  L2 <- logLik(modelnb) # Log Likelihood of model 2, unrestricted model
  LRT <- -2*L1 + 2*L2 # converges to chi^2 distribution
  checkes=LRT > qchisq(0.95, df = 1) #confidence level. If its true then negative binomial
  return(checkes)
}

# U.K. Modeling
#model1
m1_uk_base_1 <- glm(number_speeches ~ party_leader + conservative_MP,data = UK_data_no_outlier_1, family = "poisson")
library(MASS) 
m1_uk_base_nb_1 <- glm.nb(number_speeches ~ party_leader + conservative_MP,data = UK_data_no_outlier_1,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model
check.nb.pos(m1_uk_base_1,m1_uk_base_nb_1)
summary(m1_uk_base_nb_1)

m1_uk_base<- glm(number_speeches ~ party_leader + conservative_MP,data = UKdata, family = "poisson")
library(MASS) 
m1_uk_base_nb <- glm.nb(number_speeches ~ party_leader + conservative_MP,data = UKdata,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model
check.nb.pos(m1_uk_base,m1_uk_base_nb)
summary(m1_uk_base_nb)

#There is no difference in our base model lets try adding distance
#model2
m2_uk_base_1 <- glm(number_speeches ~ conservative_MP,data = UK_data_no_outlier_1, family = "poisson")
library(MASS) 
m2_uk_base_nb_1 <- glm.nb(number_speeches ~ conservative_MP,data = UK_data_no_outlier_1,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model.
check.nb.pos(m2_uk_base_1,m2_uk_base_nb_1)
summary(m2_uk_base_nb_1)


m2_uk_base <- glm(number_speeches ~ conservative_MP,data = UKdata, family = "poisson")
library(MASS) 
m2_uk_base_nb <- glm.nb(number_speeches ~ conservative_MP,data = UKdata,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model.
check.nb.pos(m2_uk_base,m2_uk_base_nb)
summary(m2_uk_base_nb)

#model 3
m3_uk_base_1 <- glm(number_speeches ~ party_leader+ideological_distance,data = UK_data_no_outlier_1, family = "poisson")
library(MASS) 
m3_uk_base_nb_1 <- glm.nb(number_speeches ~ party_leader+ideological_distance,data = UK_data_no_outlier_1,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model.
check.nb.pos(m3_uk_base_1,m3_uk_base_nb_1)
summary(m3_uk_base_nb_1)

m3_uk_base <- glm(number_speeches ~ party_leader+ideological_distance,data = UKdata, family = "poisson")
library(MASS) 
m3_uk_base_nb <- glm.nb(number_speeches ~ party_leader+ideological_distance,data = UKdata,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model.
check.nb.pos(m3_uk_base,m3_uk_base_nb)
summary(m3_uk_base_nb)

#model 4
m4_uk_base_1 <- glm(number_speeches ~ conservative_MP+party_leader+ideological_distance,data = UK_data_no_outlier_1, family = "poisson")
library(MASS) 
m4_uk_base_nb_1 <- glm.nb(number_speeches ~ conservative_MP+party_leader+ideological_distance,data = UK_data_no_outlier_1,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model.
check.nb.pos(m4_uk_base_1,m4_uk_base_nb_1)
summary(m4_uk_base_nb_1)

m4_uk_base <- glm(number_speeches ~ conservative_MP+party_leader+ideological_distance,data = UKdata, family = "poisson")
library(MASS) 
m4_uk_base_nb <- glm.nb(number_speeches ~ conservative_MP+party_leader+ideological_distance,data = UKdata,control = glm.control(maxit=100))
#Comparing negative binomial model with poisson model.
check.nb.pos(m4_uk_base,m4_uk_base_nb)
summary(m4_uk_base_nb)


stargazer(m1_uk_base_nb_1,m2_uk_base_nb_1,m3_uk_base_nb_1,m4_uk_base_nb_1) #outlier handled
stargazer(m1_uk_base_nb,m2_uk_base_nb,m3_uk_base_nb, m4_uk_base_nb) #outlier not handled


##After trying multiple models, it had been decided go with outlier treated modeling since models done
#with outlier handled datasets, results better in terms of AIC scores and every variable still found 
#significant in both techniques.
#the best AIC result achieved by UK_data_no_outlier_1. So simulations will be done using that model.



#simulating conservatives & labour party with different distance measures 
nsim <- 1000
gamma.hat_uk <- coef(m4_uk_base_nb_1)
V.hat_uk <- vcov(m4_uk_base_nb_1)
S_uk <- mvrnorm(nsim, gamma.hat_uk, V.hat_uk)
ideo_sim_uk=seq(0,5,length=100)
scenario_uk_c <- cbind(1,1,0,ideo_sim_uk)
scenario_uk_l <- cbind(1,0,0,ideo_sim_uk)
#conservative_MP+party_leader+ideological_distance
X_uk_c <- S_uk %*% t(scenario_uk_c)
X_uk_l <- S_uk %*% t(scenario_uk_l)
lambda_uk_c <- exp(X_uk_c)
lambda_uk_l <- exp(X_uk_l)
p.mean_uk_c <- apply(lambda_uk_c, 2, mean) 
p.mean_uk_l <- apply(lambda_uk_l, 2, mean) 
p.qu_uk_c <- t(apply(lambda_uk_c, 2, quantile, prob = c(0.025, 0.975)))
p.qu_uk_l <- t(apply(lambda_uk_l, 2, quantile, prob = c(0.025, 0.975)))
#conservatives
range(UK_data_no_outlier_1$number_speeches)
mean(UK_data_no_outlier_1$number_speeches)
plot(x=ideo_sim_uk, y=p.mean_uk_c, type = "l", ylim=c(0,800),
     ylab = "Number of Speeches",
     main="Conservatives in UK",
     xlab = "Ideological Distance",
     bty = "n",
     las = 1)
segments(ideo_sim_uk, p.qu_uk_c[, 1], ideo_sim_uk, p.qu_uk_c[, 2])
#labour party
plot(x=ideo_sim_uk, y=p.mean_uk_l, type = "l", ylim=c(0,800),
     ylab = "Number of Speeches",
     main="Labour Pary in UK",
     xlab = "Ideological Distance",
     bty = "n",
     las = 1)
segments(ideo_sim_uk, p.qu_uk_l[, 1], ideo_sim_uk, p.qu_uk_l[, 2])

mean(p.mean_uk_l)



#First difference between conservatives and labor part in UK
conser_party <- cbind(1, 1,0,median(UK_data_no_outlier_1$ideological_distance))
#conservative_MP+party_leader+ideological_distance
labor_party <- cbind(1, 0,0,median(UK_data_no_outlier_1$ideological_distance))
X_c <- S_uk %*% t(conser_party)
X_l <- S_uk %*% t(labor_party)
lambda_c <- exp(X_c)
lambda_l <- exp(X_l)
theta_uk <- m4_uk_base_nb_1$theta #set the variance parameter fro negative varaible.
exp.c <- sapply(lambda_c, function(x) mean(rnbinom(1000, size = theta_uk, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.l <- sapply(lambda_l, function(x) mean(rnbinom(1000, size = theta_uk, mu = x)))
exp.values_uk <- c(exp.c,exp.l)
df_uk <- data.frame(exp.values_uk)
mean(exp.c)
mean(exp.l)
df_uk$id <- c(rep("Conservative", 1000), rep("Labour", 1000))
library(ggplot2)
ggplot(df_uk, aes(x = exp.values_uk, fill = id)) + 
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  xlab("Expected Number of Speeches") +
  ylab("Density") + 
  theme_bw()

#Lets examine fist differences regarding party leaders in conservatives
conser_partyl <- cbind(1, 1,1,median(UK_data_no_outlier_1$ideological_distance))
#conservative_MP+party_leader+ideological_distance
conser_party_nl <- cbind(1, 1,0,median(UK_data_no_outlier_1$ideological_distance))
X_con_le <- S_uk %*% t(conser_partyl)
X_con_nl <- S_uk %*% t(conser_party_nl)
lambda_con_le <- exp(X_con_le)
lambda_con_nl <- exp(X_con_nl)
theta_uk <- m4_uk_base_nb_1$theta #set the variance parameter fro negative varaible.
exp.con.le <- sapply(lambda_con_le, function(x) mean(rnbinom(1000, size = theta_uk, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.con.nl <- sapply(lambda_con_nl, function(x) mean(rnbinom(1000, size = theta_uk, mu = x)))
exp.values_leader_con <- c(exp.con.le,exp.con.nl)
df_uk_con <- data.frame(exp.values_leader_con)
df_uk_con$id <- c(rep("Leader", 1000), rep("Backbenchers", 1000))
library(ggplot2)
ggplot(df_uk_con, aes(x = exp.values_leader_con, fill = id)) + 
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  ggtitle("Leader Effect in Conservative Party")+
  xlab("Expected Number of Speeches") +
  ylab("Density") + 
  theme_bw()

#Lets examine fist differences regarding party leaders in labour party
labour_partyl <- cbind(1, 0,1,median(UK_data_no_outlier_1$ideological_distance))
#conservative_MP+party_leader+ideological_distance
labour_party_nl <- cbind(1, 0,0,median(UK_data_no_outlier_1$ideological_distance))
X_lab_le <- S_uk %*% t(labour_partyl)
X_lab_nl <- S_uk %*% t(labour_party_nl)
lambda_lab_le <- exp(X_lab_le)
lambda_lab_nl <- exp(X_lab_nl)
theta_uk <- m4_uk_base_nb_1$theta #set the variance parameter fro negative varaible.
exp.lab.le <- sapply(lambda_lab_le, function(x) mean(rnbinom(1000, size = theta_uk, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.lab.nl <- sapply(lambda_lab_nl, function(x) mean(rnbinom(1000, size = theta_uk, mu = x)))
exp.values_leader_lab <- c(exp.lab.le,exp.lab.nl)
df_uk_lab <- data.frame(exp.values_leader_lab)
df_uk_lab$id <- c(rep("leader", 1000), rep("backbenc", 1000))
library(ggplot2)
ggplot(df_uk_lab, aes(x = exp.values_leader_lab, fill = id)) + 
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  ggtitle("Leader Effect in Labour Party")+
  xlab("Expected Number of Speeches") +
  ylab("Density") + 
  theme_bw()


#-----------GERMANY-----------##
#visualizing Germany
#visualizing the dataset
summary(GERdata)

hist(x=GERdata$party_leader,xlab="Party Leader",main="")
hist(x=GERdata$ideological_distance,xlab="Ideological Distance",main="")
hist(x=GERdata$committee,xlab="Committee Assignments",main="")
hist(x=GERdata$caolMPoutside,xlab="Coalition",main="")
ggplot(GERdata, aes(list_candidate))+geom_bar(stat = "count")+theme_bw()
ggplot(GERdata, aes(party_affiliation))+geom_bar(stat = "count")+theme_bw()
hist(x=GERdata$number_speeches,xlab="Number of Speeches",main="")


counts <- table(GERdata$party_affiliation)
barplot(prop.table(table(GERdata$party_affiliation)),horiz=TRUE,names.arg=c("FDP", "Alliance 90/Greens", "CDU/CSU","Die Linke","SPD"),cex.names=0.5)

my_vector=counts
names(my_vector)=c("FDP", "Greens", "CDU/CSU","Die Linke","SPD")
barplot(my_vector)

barplot(prop.table(table(GERdata$party_affiliation)),horiz=TRUE,names.arg=c("FDP", "Alliance 90/Greens", "CDU/CSU","Die Linke","SPD"),cex.names=0.5)



aggregate(GERdata$number_speeches, by=list(GERdata$ideological_distance), FUN=mean)
aggregate(GERdata$number_speeches, by=list(GERdata$party_leader), FUN=mean)
aggregate(GERdata$number_speeches, by=list(GERdata$list_candidate), FUN=mean)
aggregate(GERdata$number_speeches, by=list(GERdata$committee), FUN=mean)
aggregate(GERdata$number_speeches, by=list(GERdata$caolMPoutside), FUN=mean)
aggregate(GERdata$number_speeches, by=list(GERdata$party_affiliation), FUN=mean)


y=GERdata$number_speeches
col_ger=colnames(GERdata)
par(mfrow=c(2, 3))
i=2
maxrow=length(col_ger)+1
while(i<maxrow){
  k=col_ger[i]
  print(k)
  plot(x=GERdata[[k]], y=y,ylab="Number of Speeches",xlab = k)
  i=i+1
}
par(mfrow=c(1, 1))

GERdata$party_affiliation_coded <- as.numeric(as.factor(GERdata$party_affiliation))
GERdata$list_candidate <- as.numeric(as.factor(GERdata$list_candidate))
GERdata$list_candidate[GERdata$list_candidate==1] <- 0
GERdata$list_candidate[GERdata$list_candidate==2] <- 1

#adding new variable "total number of seats the party received in the 2005 elections
#In 2005 elections, SPD(coded 5) received 222 seats, CDU/CSU(coded 3) received 226 seats,
#FDP(coded 1) received 61 seats, Alliance 90/Greens(coded 2) received 54 seats and die linke received 51 seats.
#Germany: http://electionresources.org/de/bundestag.php?election=2005
GERdata$numberofseats[GERdata$party_affiliation_coded==5]<-222
GERdata$numberofseats[GERdata$party_affiliation_coded==3]<-226
GERdata$numberofseats[GERdata$party_affiliation_coded==1]<-61
GERdata$numberofseats[GERdata$party_affiliation_coded==2]<-51
GERdata$numberofseats[GERdata$party_affiliation_coded==4]<-54

summary(GERdata)

#handling missingess
#From summary it has seen that only ideological_distance variable has missingness. 
#Let me draw a histogram to understand ideological distance
hist(GERdata$ideological_distance)
#in the GERdata we have 209 observations including NA's.  NA's represents %5 of our dataset. 
#looking more closely to ideological distance missingness and searching for pattern.
GERdata[rowSums(is.na(GERdata)) > 0,]
#There is no random in missingness, therefore assuming missing at random.
#Since we already have small amount of data, I would choose to fill missing values with median.
#median is more robust compare to mean, since the dataset can still have outliers.
GERdata$ideological_distance[is.na(GERdata$ideological_distance)] <- median(GERdata$ideological_distance, na.rm=TRUE)
#checking whether there is missing values 
summary(GERdata$ideological_distance)

par(mfrow=c(1, 1))
#Handling outliers
boxplot(GERdata$ideological_distance, main="Ideological Distances in Germany Parliament", boxwex=0.1)
boxplot(GERdata$number_speeches, main="Number of Speechs in Germany Parlimant", boxwex=0.1)
boxplot(GERdata$committee, main="Committee in Germany Parliament", boxwex=0.1)

#Dummy coding of party variable
new_parties=dummy.code(GERdata$party_affiliation_coded)
#dropping one variable in order dummy code (dropping AllianceGreens)
colnames(new_parties)<- c("FDP","AllianceGreens","CDU/CSU","Die Linke","SPD")
keeps <- c("FDP","Die Linke","CDU/CSU","SPD")
GERdata_coded=new_parties[, keeps, drop=FALSE]
GERdata_coded <- data.frame(GERdata_coded,GERdata)
summary(GERdata_coded)

#histogram after list is changed into candidate
hist(x=GERdata_coded$list_candidate,xlab="List Candidate",main="")
#re naminf the party affiliation
GERdata_coded$part_affiliation_new[GERdata_coded$party_affiliation_coded==5] <- "CDU/CSU"
GERdata_coded$part_affiliation_new[GERdata_coded$party_affiliation_coded==3] <- "SPD"
GERdata_coded$part_affiliation_new[GERdata_coded$party_affiliation_coded==1] <- "FDP"
GERdata_coded$part_affiliation_new[GERdata_coded$party_affiliation_coded==2] <- "Greens"
GERdata_coded$part_affiliation_new[GERdata_coded$party_affiliation_coded==4] <- "DieLinke"
count=table(GERdata_coded$part_affiliation_new)
#pie chart
slices <- count
lbls <- c("CDU/CSU", "Die Linke", "FDP", "Alliance 90/Greens", "SPD")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF"),    main="")



#model1
#Variables include: party_leader + list_candidate + committee
m1_base <- glm(number_speeches ~ party_leader + list_candidate + committee,data = GERdata_coded, family = "poisson")
library(MASS) 
m1_base_nb <- glm.nb(number_speeches ~ party_leader + list_candidate + committee,data = GERdata_coded,control = glm.control(maxit=100))
check.nb.pos(m1_base,m1_base_nb)
#True so negative binomial model
summary(m1_base_nb)

#model2
#Variables include: ideological_distance+party_leader + list_candidate + committee
m2 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m2_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee,data = GERdata_coded,control = glm.control(maxit=100))
check.nb.pos(m2,m2_nb)
#TRUE-> So negative binomial model
summary(m2_nb)


#model3
#Variables include: ideological_distance+party_leader + list_candidate + committee+CDU/CSU+SPD+DIE LINKE+FDP
m3 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m3_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD,data = GERdata_coded,control = glm.control(maxit=100))
check.nb.pos(m3,m3_nb)
#TRUE-> So negative binomial model
summary(m3_nb)

#model4
#Variables include: ideological_distance+party_leader + list_candidate + committee+CDU/CSU+SPD+DIE LINKE+FDP+MPOutsideCoalition
m4 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m4_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside,data = GERdata_coded,control = glm.control(maxit=100))
check.nb.pos(m4,m4_nb)
#True so negative binomial model
summary(m4_nb)

#model5
#Variables include: ideological_distance+party_leader + list_candidate + committee+SPD+CDU.CSU+MPOutsideCoalition (only including SPD and CDU.CSU)
m5 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ SPD+CDU.CSU+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m5_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ SPD+CDU.CSU+caolMPoutside,data = GERdata_coded,control = glm.control(maxit=100))
check.nb.pos(m5,m5_nb)
#True so negative binomail model
summary(m5_nb)

#model6
#Variables include: ideological_distance+party_leader + list_candidate + committee+SPD+MPOutsideCoalition (only including SPD)
#Building 
m6 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ SPD+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m6_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ SPD+caolMPoutside,data = GERdata_coded, control = glm.control(maxit=100))
check.nb.pos(m6,m6_nb)
#True so negative binomial model
summary(m6_nb)


#model7
#Variables include: ideological_distance+party_leader + list_candidate + committee+CDU.CSU+MPOutsideCoalition (only including CDU/CSU)
m7 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ CDU.CSU+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m7_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ CDU.CSU+caolMPoutside,data = GERdata_coded, control = glm.control(maxit=100))
check.nb.pos(m7,m7_nb)
#True so negative binomial model
summary(m7_nb)

#model8
#Variables include: ideological_distance+party_leader + list_candidate + committee+FDP+MPOutsideCoalition (only including CDU/CSU)
m8 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m8_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+caolMPoutside,data = GERdata_coded, control = glm.control(maxit=100))
check.nb.pos(m8,m8_nb)
#True so negative binomial model
summary(m8_nb)

#model9
#Variables include: ideological_distance+party_leader + list_candidate + committee+Die.Linke+MPOutsideCoalition (only including CDU/CSU)
m9 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ Die.Linke+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m9_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ Die.Linke+caolMPoutside,data = GERdata_coded, control = glm.control(maxit=100))
check.nb.pos(m9,m9_nb)
#True so negative binomial model
summary(m9_nb)

stargazer(m1_base_nb,m2_nb,m3_nb,m4_nb,m5_nb,m6_nb,m7_nb,m8_nb,m9_nb,float.env="sidewaystable")

#----Numberofseatsvariableadded
#model10
#Variables include: party_leader+ committee+numberofseats
m10 <- glm(number_speeches ~ party_leader+ committee+numberofseats+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m10_nb <- glm.nb(number_speeches ~ party_leader+ committee+numberofseats+caolMPoutside,data = GERdata_coded, control = glm.control(maxit=100))
check.nb.pos(m10,m10_nb)
#True so negative binomial model
summary(m10_nb)

#model11
#Variables include: ideological_distance+party_leader+ committee+numberofseats+caolMPoutside
#Building 
m11 <- glm(number_speeches ~ ideological_distance+party_leader+ committee+numberofseats+caolMPoutside,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m11_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader+ committee+numberofseats+caolMPoutside,data = GERdata_coded, control = glm.control(maxit=100))
check.nb.pos(m11,m11_nb)
#True so negative binomial model
summary(m11_nb)
#ideological distance was significant

#model12
#Variables include: number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutsidE+N
m12 <- glm(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside+numberofseats,data = GERdata_coded, family = "poisson")
# Negative Binomial Model
m12_nb <- glm.nb(number_speeches ~ ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside+numberofseats,data = GERdata_coded, control = glm.control(maxit=100))
check.nb.pos(m12,m12_nb)
#True so negative binomial model
summary(m12_nb)
stargazer(m10_nb,m11_nb,m12_nb,float.env="sidewaystable")


#-------------Quantity of Interest-------#
#simulation model4
#For SPD
nsim <- 1000
gamma.hat <- coef(m4_nb)

V.hat <- vcov(m4_nb)
dim(V.hat)
S <- mvrnorm(nsim, gamma.hat, V.hat)
dim(S)
ideo_sim=seq(0,1,length=100)
length(ideo_sim)
sc_spd <- cbind(1, ideo_sim, 0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,0,0,1,1) #spd
sc_cdu <- cbind(1, ideo_sim, 0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,0,1,0,1) #cdu.csu
sc_linke <- cbind(1, ideo_sim, 0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,1,0,0,1) #dielinke
sc_fdp <- cbind(1, ideo_sim, 0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 1,0,0,0,1) #fdp
sc_gre <- cbind(1, ideo_sim, 0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,0,0,0,1) #greens

#ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside

X_spd <- S %*% t(sc_spd)
X_cdu <- S %*% t(sc_cdu)
X_linke <- S %*% t(sc_linke)
X_fdp <- S %*% t(sc_fdp)
X_gre <- S %*% t(sc_gre)
lambda_spd <- exp(X_spd)
lambda_cdu <- exp(X_cdu)
lambda_linke <- exp(X_linke)
lambda_fdp <- exp(X_fdp)
lambda_gre <- exp(X_gre)
mean_spd <- apply(lambda_spd, 2, mean) 
p_spd <- t(apply(lambda_spd, 2, quantile, prob = c(0.025, 0.975)))
mean_cdu <- apply(lambda_cdu, 2, mean) 
p_cdu <- t(apply(lambda_cdu, 2, quantile, prob = c(0.025, 0.975)))
mean_linke <- apply(lambda_linke, 2, mean) 
p_linke <- t(apply(lambda_linke, 2, quantile, prob = c(0.025, 0.975)))
mean_fdp <- apply(lambda_fdp, 2, mean) 
p_fdp <- t(apply(lambda_fdp, 2, quantile, prob = c(0.025, 0.975)))
mean_gre <- apply(lambda_gre, 2, mean) 
p_gre <- t(apply(lambda_gre, 2, quantile, prob = c(0.025, 0.975)))

#spd
plot(x=ideo_sim, y=mean_spd, type = "l", ylim=c(0,60),
     ylab = "Number of Speechs in SPD",
     xlab = "Distance",
     bty = "n",
     las = 1)
segments(ideo_sim, p_spd[, 1], ideo_sim, p_spd[, 2])
#cdu
plot(x=ideo_sim, y=mean_cdu, type = "l", ylim=c(0,60),
     ylab = "Number of Speechs in CDU",
     xlab = "Distance",
     bty = "n",
     las = 1)
segments(ideo_sim, p_cdu[, 1], ideo_sim, p_cdu[, 2])
#linke
plot(x=ideo_sim, y=mean_linke, type = "l", ylim=c(0,60),
     ylab = "Number of Speechs in Linke",
     xlab = "Distance",
     bty = "n",
     las = 1)
segments(ideo_sim, p_linke[, 1], ideo_sim, p_linke[, 2])
#FDP
plot(x=ideo_sim, y=mean_fdp, type = "l", ylim=c(0,60),
     ylab = "Number of Speechs in FDP",
     xlab = "Distance",
     bty = "n",
     las = 1)
segments(ideo_sim, p_fdp[, 1], ideo_sim, p_fdp[, 2])
#Greens
plot(x=ideo_sim, y=mean_gre, type = "l", ylim=c(0,60),
     ylab = "Number of Speechs in Greens",
     xlab = "Distance",
     bty = "n",
     las = 1)
segments(ideo_sim, p_gre[, 1], ideo_sim, p_gre[, 2])

#First differences based on parties
sce_par_SPD <- cbind(1, median(GERdata_coded$ideological_distance),0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,0,0,1,1) #spd
sce_par_FDP <- cbind(1, median(GERdata_coded$ideological_distance),0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 1,0,0,0,1) #fdp
sce_par_DieLinke <- cbind(1, median(GERdata_coded$ideological_distance),0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,1,0,0,1) #die linke
sce_par_CDUCSU <- cbind(1, median(GERdata_coded$ideological_distance),0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,0,1,0,1) #cdu.csu
sce_par_Greens <- cbind(1, median(GERdata_coded$ideological_distance),0,median(GERdata_coded$list_candidate), median(GERdata_coded$committee), 0,0,0,0,1) #greens
#ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside
Xpar_SPD <- S %*% t(sce_par_SPD)
Xpar_FDP <- S %*% t(sce_par_FDP)
Xpar_Linke <- S %*% t(sce_par_DieLinke)
Xpar_CDU <- S %*% t(sce_par_CDUCSU)
Xpar_Gre <- S %*% t(sce_par_Greens)
lambda_p_SPD <- exp(Xpar_SPD)
lambda_p_FDP <- exp(Xpar_FDP)
lambda_p_Linke <- exp(Xpar_Linke)
lambda_p_CDU <- exp(Xpar_CDU)
lambda_p_Gre <- exp(Xpar_Gre)
theta <- m4_nb$theta #set the variance paraemter fro negative varaible.
exp.SPD <- sapply(lambda_p_SPD, function(x) mean(rnbinom(1000, size = theta, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.FDP <- sapply(lambda_p_FDP, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.DieLinke <- sapply(lambda_p_Linke, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.CDUCSU <- sapply(lambda_p_CDU, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.sce_Greens <- sapply(lambda_p_Gre, function(x) mean(rnbinom(1000, size = theta, mu = x)))

exp.values <- c(exp.SPD, exp.FDP, exp.DieLinke,exp.CDUCSU,exp.sce_Greens)
fd_parties <- data.frame(exp.values)

fd_parties$id <- c(rep("SPD", 1000), rep("FDP", 1000), rep("DieLinke", 1000),rep("CDUSCU",1000),rep("Greens",1000))
library(ggplot2)
ggplot(fd_parties, aes(x = exp.values, fill = id)) + #we only have one variable. fill=coloring.
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  xlab("Expected Number of Speechs") +
  ylab("Density") + 
  theme_bw()

#First differences based on list and district
#first difference of SPD
spd_list <- cbind(1, median(GERdata_coded$ideological_distance),0,1, median(GERdata_coded$committee), 0,0,0,1,1) #spd
spd_dist <- cbind(1, median(GERdata_coded$ideological_distance),0,0, median(GERdata_coded$committee), 0,0,0,1,1) #spd
XSPD_list <- S %*% t(spd_list)
XSPD_dist <- S %*% t(spd_dist)
lambda_list_SPD <- exp(XSPD_list)
lambda_dist_SPD <- exp(XSPD_dist)
exp.SPD_list <- sapply(lambda_list_SPD, function(x) mean(rnbinom(1000, size = theta, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.SPD_dist <- sapply(lambda_dist_SPD, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.values_SPD <- c(exp.SPD_list, exp.SPD_dist)
fd_SPD <- data.frame(exp.values_SPD)
fd_SPD$id <- c(rep("SPD_list", 1000), rep("SPD_dist", 1000))
library(ggplot2)
ggplot(fd_SPD, aes(x = exp.values_SPD, fill = id)) + #we only have one variable. fill=coloring.
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  xlab("Expected Number of Speechs in SPD") +
  ylab("Density") + 
  theme_bw()

#First Diffrence of CDU.CSU
CDU_list <- cbind(1, median(GERdata_coded$ideological_distance),0,1, median(GERdata_coded$committee), 0,0,1,0,1) #cdu
CDU_dist <- cbind(1, median(GERdata_coded$ideological_distance),0,0, median(GERdata_coded$committee), 0,0,1,0,1) #cdu
#ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside
XCDU_list <- S %*% t(CDU_list)
XCDU_dist <- S %*% t(CDU_dist)
lambda_list_CDU <- exp(XCDU_list)
lambda_dist_CDU <- exp(XCDU_dist)
exp.CDU_list <- sapply(lambda_list_CDU, function(x) mean(rnbinom(1000, size = theta, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.CDU_dist <- sapply(lambda_dist_CDU, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.values_CDU <- c(exp.CDU_list, exp.CDU_dist)
fd_CDU <- data.frame(exp.values_CDU)
fd_CDU$id <- c(rep("CDU_list", 1000), rep("CDU_dist", 1000))
library(ggplot2)
ggplot(fd_CDU, aes(x = exp.values_CDU, fill = id)) + #we only have one variable. fill=coloring.
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  xlab("Expected Number of Speechs") +
  ylab("Density") + 
  theme_bw()

#First difference of DLinke
DLinke_list <- cbind(1, median(GERdata_coded$ideological_distance),0,1, median(GERdata_coded$committee), 0,1,0,0,1) #DLinke
DLinke_dist <- cbind(1, median(GERdata_coded$ideological_distance),0,0, median(GERdata_coded$committee), 0,1,0,0,1) #DLinke
#ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+DLinke.CSU+SPD+caolMPoutside
XDLinke_list <- S %*% t(DLinke_list)
XDLinke_dist <- S %*% t(DLinke_dist)
lambda_list_DLinke <- exp(XDLinke_list)
lambda_dist_DLinke <- exp(XDLinke_dist)
exp.DLinke_list <- sapply(lambda_list_DLinke, function(x) mean(rnbinom(1000, size = theta, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.DLinke_dist <- sapply(lambda_dist_DLinke, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.values_DLinke <- c(exp.DLinke_list, exp.DLinke_dist)
fd_DLinke <- data.frame(exp.values_DLinke)
fd_DLinke$id <- c(rep("DLinke_list", 1000), rep("DLinke_dist", 1000))
library(ggplot2)
ggplot(fd_DLinke, aes(x = exp.values_DLinke, fill = id)) + #we only have one variable. fill=coloring.
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  xlab("Expected Number of Speechs") +
  ylab("Density") + 
  theme_bw()


#First difference of FDP
FDP_list <- cbind(1, median(GERdata_coded$ideological_distance),0,1, median(GERdata_coded$committee), 1,0,0,0,1) #FDP
FDP_dist <- cbind(1, median(GERdata_coded$ideological_distance),0,0, median(GERdata_coded$committee), 1,0,0,0,1) #FDP
#ideological_distance+party_leader + list_candidate + committee+ FDP+Die.Linke+CDU.CSU+SPD+caolMPoutside
XFDP_list <- S %*% t(FDP_list)
XFDP_dist <- S %*% t(FDP_dist)
lambda_list_FDP <- exp(XFDP_list)
lambda_dist_FDP <- exp(XFDP_dist)
exp.FDP_list <- sapply(lambda_list_FDP, function(x) mean(rnbinom(1000, size = theta, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.FDP_dist <- sapply(lambda_dist_FDP, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.values_FDP <- c(exp.FDP_list, exp.FDP_dist)
fd_FDP <- data.frame(exp.values_FDP)
fd_FDP$id <- c(rep("FDP_list", 1000), rep("FDP_dist", 1000))
library(ggplot2)
ggplot(fd_FDP, aes(x = exp.values_FDP, fill = id)) + #we only have one variable. fill=coloring.
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  xlab("Expected Number of Speechs") +
  ylab("Density") + 
  theme_bw()

#First difference in Greens
GRE_list <- cbind(1, median(GERdata_coded$ideological_distance),0,1, median(GERdata_coded$committee), 0,0,0,0,1) #GRE
GRE_dist <- cbind(1, median(GERdata_coded$ideological_distance),0,0, median(GERdata_coded$committee), 0,0,0,0,1) #GRE
#ideological_distance+party_leader + list_candidate + committee+ GRE+Die.Linke+CDU.CSU+SPD+caolMPoutside
XGRE_list <- S %*% t(GRE_list)
XGRE_dist <- S %*% t(GRE_dist)
lambda_list_GRE <- exp(XGRE_list)
lambda_dist_GRE <- exp(XGRE_dist)
exp.GRE_list <- sapply(lambda_list_GRE, function(x) mean(rnbinom(1000, size = theta, mu = x))) #takes lambda one and plugs them into negative binominal distrbiution
exp.GRE_dist <- sapply(lambda_dist_GRE, function(x) mean(rnbinom(1000, size = theta, mu = x)))
exp.values_GRE <- c(exp.GRE_list, exp.GRE_dist)
fd_GRE <- data.frame(exp.values_GRE)
fd_GRE$id <- c(rep("GRE_list", 1000), rep("GRE_dist", 1000))
library(ggplot2)
ggplot(fd_GRE, aes(x = exp.values_GRE, fill = id)) + #we only have one variable. fill=coloring.
  geom_density(alpha = 0.4) + #layer for returning density layers. Transaparancy of the color
  guides(fill = guide_legend(title = "")) + #guide is the legend
  xlab("Expected Number of Speechs") +
  ylab("Density") + 
  theme_bw()

#### Model 11: Where my variables are okey-> ideological distance
nsim <- 1000
gamma.hat_m11 <- coef(m11_nb)
V.hat_m11 <- vcov(m11_nb)
S_m11 <- mvrnorm(nsim, gamma.hat_m11, V.hat_m11)

range(GERdata_coded$numberofseats)

seat_sim=seq(0,300,length=100)
sc_seat <- cbind(1, median(GERdata_coded$ideological_distance), 0,median(GERdata_coded$committee), seat_sim,1) #spd
#ideological_distance+party_leader+ committee+numberofseats+caolMPoutside
X_seat <- S_m11 %*% t(sc_seat)
lambda_seat <- exp(X_seat)
mean_seat <- apply(lambda_seat, 2, mean) 
p_seat <- t(apply(lambda_seat, 2, quantile, prob = c(0.025, 0.975)))
#seat
plot(x=seat_sim, y=mean_seat, type = "l", ylim=c(0,60),
     ylab = "Number of Speechs in the Parlaiment",
     xlab = "SeatShare",
     bty = "n",
     las = 1)
segments(seat_sim, p_seat[, 1], seat_sim, p_seat[, 2])

citation(package='ggplot2')
citation(package='stargazer')
citation(package='MASS')


