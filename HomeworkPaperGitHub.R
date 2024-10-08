#Load Libraries
library(foreign)
library(naniar)
library(missRanger)
library(corrplot)
library(RColorBrewer)
library(stats)
library(jtools)
library(dplyr)
library(plyr)
library(mltools)
library(stats)
library(ggplot2)
library(data.table)
library(ranger)
library(ggplot2)
library(ranger)
library(dplyr)
library(dbarts)
library(rpart)
library(rpart.plot)
library(Rcpp)
library(latex2exp)

#set random seed
set.seed(123)

#Use one thread
setDTthreads(1)

#Load Data
BCG<-as.data.frame(read.spss("/your_directory/bcgirlm7.sav"))
BSG<-as.data.frame(read.spss("/your_directory/bsgirlm7.sav"))
BST<-as.data.frame(read.spss("/your_directory/bstirlm7.sav"))
BTM<-as.data.frame(read.spss("/your_directory/btmirlm7.sav"))
BTS<-as.data.frame(read.spss("/your_directory/btsirlm7.sav"))

#Each row is a student matched to a teacher.
#Some students are matched to >1 teacher.
#Some students have no teacher and don't appear.
BST_BTM<-merge(BST, BTM, by="IDTEALIN")

order1<-as.numeric(as.character(BST_BTM$IDSTUD))
order2<-as.numeric(as.character(BST_BTM$BTBM14))
order2<-ifelse(is.na(order2), 0, order2)*-1

BST_BTM<-BST_BTM[order(order1, order2),]

support_teachers<-c()

for(i in 2:(length(BST_BTM$IDSTUD)))
{
  if(BST_BTM$IDSTUD[i] == BST_BTM$IDSTUD[i-1])
  {
    support_teachers<-c(support_teachers, i)
  }
}

BST_BTM<-BST_BTM[-support_teachers,]

#Each row now has the context data added to it.
BSG_BST_BTM<-merge(BSG, BST_BTM, by="IDSTUD", all.x=T)



#Each row now has school data too.
BSG_BST_BTM_BCG<-merge(BSG_BST_BTM, BCG, by="IDSCHOOL")




BST_BTS<-merge(BST, BTS, by="IDTEALIN")

order1<-as.numeric(as.character(BST_BTS$IDSTUD))
order2<-as.numeric(as.character(BST_BTS$BTBS14))
order2<-ifelse(is.na(order2), 0, order2)*-1

BST_BTS<-BST_BTS[order(order1, order2),]

support_teachers<-c()

for(i in 2:(length(BST_BTS$IDSTUD)))
{
  if(BST_BTS$IDSTUD[i] == BST_BTS$IDSTUD[i-1])
  {
    support_teachers<-c(support_teachers, i)
  }
}

BST_BTS<-BST_BTS[-support_teachers,]

BSG_BST_BTS<-merge(BSG, BST_BTS, by="IDSTUD", all.x=T)

BSG_BST_BTS_BCG<-merge(BSG_BST_BTS, BCG, by="IDSCHOOL")




#Select Columns
maths_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
              "BSBG08A", "BSBG08B",
              "BSBG09A", "BSDGEDUP", "BSBGHER",
              "BSBGSSB", "BSBGSB", "BSBGICM",
              "BSBGSCM", "BSBGSVM",
              
              "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
              "BSBG11A", "BSBG11B", "BSBG10",
              
              "BTBG01", "BTBG02", "BTBG03", "BTBG10",
              "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDMMME",
              
              "BCBGDAS", "BCBGEAS", "BCBGMRS",
              "BCDGSBC",
              
              "BTBM19CA", "BTBM19CB", "BTBM19CC", "BTBM19CD", "BTBM19CE", "BTBM14")


maths_other<-c("BSMMAT01.x", "BSMMAT02.x", "BSMMAT03.x", 
               "BSMMAT04.x", "BSMMAT05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")


maths_treatment<-c("BSBM26AA", "BSBM26BA")


science_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
                "BSBG08A", "BSBG08B",
                "BSBG09A", "BSDGEDUP", "BSBGHER",
                "BSBGSSB", "BSBGSB", "BSBGICS",
                "BSBGSCS", "BSBGSVS",
                
                "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
                "BSBG11A", "BSBG11B", "BSBG10",
                
                "BTBG01", "BTBG02", "BTBG03", "BTBG10",
                "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDSMSE",
                
                "BCBGDAS", "BCBGEAS", "BCBGSRS",
                "BCDGSBC",
                
                "BTBS18CA", "BTBS18CB", "BTBS18CC", "BTBS18CD", "BTBS18CE", "BTBS14")

science_other<-c("BSSSCI01.x", "BSSSCI02.x", "BSSSCI03.x",
                 "BSSSCI04.x", "BSSSCI05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")

science_treatment<-c("BSBS26AB", "BSBS26BB")



XYM<-BSG_BST_BTM_BCG[,c(maths_vars, maths_other, maths_treatment)]
XYS<-BSG_BST_BTS_BCG[,c(science_vars, science_other, science_treatment)]


#Function for converting to numeric
mymap<-function(x)
{
  x<-as.numeric(as.character(x))
  
  return(x)
}

#Categories to be mapped to numeric values
from<-c("Girl","Boy","Omitted or invalid",
        "Always","Almost always","Sometimes","Never",
        "None or very few (0–10 books)",                         
        "Enough to fill one shelf (11–25 books)",                
        "Enough to fill one bookcase (26–100 books)",            
        "Enough to fill two bookcases (101–200 books)",          
        "Enough to fill three or more bookcases (more than 200)",
        "Finish <Lower secondary education—ISCED Level 2>",                             
        "Finish <Upper secondary education—ISCED Level 3>",                             
        "Finish <Post-secondary, non-tertiary education—ISCED Level 4>",                
        "Finish <Short-cycle tertiary education—ISCED Level 5>",                        
        "Finish <Bachelor’s or equivalent level—ISCED Level 6>",                        
        "Finish <Postgraduate degree: Master’s—ISCED Level 7 or Doctor —ISCED Level 8>",
        "University or Higher",                      
        "Post-secondary but not University",         
        "Upper Secondary",                           
        "Lower Secondary",                           
        "Some Primary, Lower Secondary or No School",
        "Don't Know",
        "Yes","No","I don't know", "Not applicable",
        "Every day","Almost every day","Sometimes","Never",
        "Female","Male",
        "Under 25","25–29","30–39","40–49","50–59","60 or more",
        "Major in Mathematics and Mathematics Education",       
        "Major in Mathematics but not in Mathematics Education",
        "Major in Mathematics Education but not in Mathematics",
        "All Other Majors",                                     
        "No Formal Education Beyond Upper Secondary",
        "More Affluent",                               
        "Neither More Affluent nor More Disadvantaged",
        "More Disadvantaged",
        "Major in Science and Science Education",       
        "Major in Science but not in Science Education",
        "Major in Science Education but not in Science",
        "Once a week","Once every two weeks","Once a month","Once every two month","Never or almost never",
        "Every day", "3 or 4 times a week", "1 or 2 times a week", "Less than once a week", "Never",
        "My teacher never gives me homework in…", "1–15 minutes", "16–30 minutes", "31–60 minutes", "61–90 minutes", "More than 90 minutes",
        "Always or almost always", "Sometimes", "Never or almost never", "Logically not applicable")


to=c(0, 1, NA,
     3, 2, 1, 0,
     5, 20, 50, 150, 200,
     2, 3, 4, 5, 6, 8,
     5, 4, 3, 2, 1, 0,
     1, 0, NA, NA,
     5, 2, 1, 0,
     0, 1,
     25, 29, 39, 49, 59, 66,
     2, 1, 1, 0, 0,
     3, 2, 1,
     2, 1, 1,
     4,3,2,1,0,
     5, 3.5, 1.5, 0.5, 0,
     0, 10, 20, 45, 75, 100,
     2, 1, 0, 0)



#Function for performing the conversion
mymap2<-function(x)
{
  x<-mapvalues(x,
               from=from,
               to=to)
  
  x<-as.numeric(as.character(x))
  
  return(x)
}

XYM<-mutate_at(XYM, c(2:9, 16:25, 27:28, 34, 38:43, 53, 54), mymap2)
XYS<-mutate_at(XYS, c(2:9, 16:25, 27:28, 34, 38:43, 53, 54), mymap2)

XYM<-mutate_at(XYM, c(1, 10:15, 26, 29:33, 35:37, 44:52), mymap)
XYS<-mutate_at(XYS, c(1, 10:15, 26, 29:33, 35:37, 44:52), mymap)


XY<-merge(XYM, XYS, by="IDSTUD")


###########
#Run this code before imputing missing data to get plots
#Producing the plot with the raw data prior to imputation
X<-as.matrix(one_hot(as.data.table(XY[,-c(1, 46:66, 70:79, 89:90, 92, 99:107)])))

X<-X[,!colnames(X) %in% c("BTBM19CB", "BTBM19CC", "BTBM19CD", "BTBM19CE",
                          "BTBS18CB", "BTBS18CC", "BTBS18CD", "BTBS18CE")]


DurationM<-XY$BSBM26BA
DurationS<-XY$BSBS26BB

FrequencyM<-XY$BSBM26AA
FrequencyS<-XY$BSBS26AB


table_df1 <- as.data.frame(table(FrequencyM, DurationM))
colnames(table_df1) <- c("Frequency", "Duration", "Count")
table_df2 <- as.data.frame(table(FrequencyS, DurationS))
colnames(table_df2) <- c("Frequency", "Duration", "Count")
table_df<-rbind(table_df1, table_df2)
table_df$Subject<-c(rep("Mathematics", 30), rep("Science", 30))
table_df$Subject<-as.factor(table_df$Subject)


# Create the ggplot with geom_tile
ggplot(table_df, aes(x = Frequency, y = Duration, fill = Count)) +
  geom_tile(width=0.95, height=0.95) +
  geom_text(aes(label = Count), vjust = 1) +  # Add text labels
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Color gradient
  labs(title = "Tile Plot of Homework Frequency and Duration") +
  theme_minimal() +
  scale_x_discrete(name = "Frequency", labels = c("Never", "Less Than Once a Week", "1 or 2 Times a Week", "3 or 4 Times a Week", "Every Day")) +  # Customize x-axis labels
  scale_y_discrete(name = "Duration", labels = c("0 Minutes", "1-15 Minutes", "16-30 Minutes", "31-60 Minutes", "61-90 Minutes", "More Than 90 Minutes")) +   # Customize y-axis labels
  facet_wrap(~Subject)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid=element_blank())+
  theme(text = element_text(size = 16))

# Create the scatterplot in the paper
ggplot(data=XY, mapping=aes(x=BSMMAT01.x, y=BSSSCI01.x)) +
  geom_abline(slope=1, intercept=0, col="blue") +
  geom_point(size=0.5) +
  xlab("Mathematics Achievement") +
  ylab("Science Achievement") +
  ggtitle(TeX("Student Achievement in Mathematics and Science")) +
  scale_x_continuous(breaks=c(1:100)*100)  +
  scale_y_continuous(breaks=c(1:100)*100)  +
  theme(text = element_text(size = 16))
############
############
############


XY<-missRanger(XY, num.threads=6, num.trees=50, pmm.k = 3)


X<-as.matrix(one_hot(as.data.table(XY[,-c(1, 46:66, 70:79, 89:90, 92, 99:107)])))

X<-X[,!colnames(X) %in% c("BTBM19CB", "BTBM19CC", "BTBM19CD", "BTBM19CE",
                          "BTBS18CB", "BTBS18CC", "BTBS18CD", "BTBS18CE")]


DurationM<-XY$BSBM26BA
DurationS<-XY$BSBS26BB

FrequencyM<-XY$BSBM26AA
FrequencyS<-XY$BSBS26AB

#Create the treatment indicators
Z1<-ifelse(FrequencyM==3.5, 1, 0)
Z2<-ifelse(FrequencyS==3.5, 1, 0)

Z3<-ifelse(FrequencyM==5, 1, 0)
Z4<-ifelse(FrequencyS==5, 1, 0)

Z5<-ifelse(DurationM==20, 1, 0)
Z6<-ifelse(DurationS==20, 1, 0)

Z7<-ifelse(DurationM>=45, 1, 0)
Z8<-ifelse(DurationS>=45, 1, 0)

#Get propensity scores
p_mod<-bart(x.train = X, y.train = Z1, k=3)
p<-colMeans(pnorm(p_mod$yhat.train))
p_mod2<-bart(x.train = X, y.train = Z2, k=3)
p2<-colMeans(pnorm(p_mod2$yhat.train))
p_mod3<-bart(x.train = X, y.train = Z3, k=3)
p3<-colMeans(pnorm(p_mod3$yhat.train))
p_mod4<-bart(x.train = X, y.train = Z4, k=3)
p4<-colMeans(pnorm(p_mod4$yhat.train))
p_mod5<-bart(x.train = X, y.train = Z5, k=3)
p5<-colMeans(pnorm(p_mod5$yhat.train))
p_mod6<-bart(x.train = X, y.train = Z6, k=3)
p6<-colMeans(pnorm(p_mod6$yhat.train))
p_mod7<-bart(x.train = X, y.train = Z7, k=3)
p7<-colMeans(pnorm(p_mod7$yhat.train))
p_mod8<-bart(x.train = X, y.train = Z8, k=3)
p8<-colMeans(pnorm(p_mod8$yhat.train))


Z1<-cbind(Z1, Z2)
Z2<-cbind(Z3, Z4)
Z3<-cbind(Z5, Z6)
Z4<-cbind(Z7, Z8)




#Set number of trees and iterations
n_tree_mu<-50
n_tree_tau<-20
n_iter<-5000
n_burn<-3000


#Change 01 to 02, 03, 04, and 05 for different chains.
Y<-cbind(XY$BSMMAT01.x, XY$BSSSCI01.x)

X1<-cbind(X, p, p2, p3, p4, p5, p6, p7, p8)

sourceCpp(file = "~/your_directory/HomeworkModelGitHub.cpp")

group_id<-as.integer(as.factor(XY$IDCLASS.x.x)) - 1

group_id_test<-rep(group_id, 16)

my_mod <- fast_bart(X1, 
                    Y, 
                    Z1,
                    Z2,
                    Z3,
                    Z4,
                    X,
                    X1[1:3,],
                    X[1:3,],
                    0.95, 
                    2, 
                    0.25, 
                    3, 
                    diag((1)^2/n_tree_mu, 2), 
                    diag((0.3)^2/n_tree_tau, 2), 
                    1, 
                    diag(1, 2), 
                    n_iter, 
                    n_tree_mu, 
                    n_tree_tau, 
                    1, 
                    group_id,
                    group_id_test,
                    diag(0.1, 2),
                    matrix(0, nrow=2, ncol=1),
                    diag(0.01, 2),
                    1,
                    n_burn,
                    2)

#save results
save(my_mod, file = "ModelResultsChain1.RData", compress = "xz")
