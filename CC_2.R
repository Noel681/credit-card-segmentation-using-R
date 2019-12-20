rm(list = ls())

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)


#1.Loading data 

orginal_data = read.csv("C:/Users/Hp/Desktop/BLESSED 2/credit-card-data.csv",stringsAsFactors = FALSE)

h = orginal_data

str(h)

summary(h)

h = h[,c(-1)]

k = h


################################Feature_Extraction#######################


h$Monthly_Avg_PURCHASES <- h$PURCHASES/(h$PURCHASES_FREQUENCY*h$TENURE)
h$Monthly_CASH_ADVANCE <- h$CASH_ADVANCE/(h$CASH_ADVANCE_FREQUENCY*h$TENURE)
h$LIMIT_USAGE <- h$BALANCE/h$CREDIT_LIMIT
h$MIN_PAYMENTS_RATIO <- h$PAYMENTS/h$MINIMUM_PAYMENTS



Numerical_Variabless <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","Monthly_Avg_PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE",
                          "Monthly_CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY",
                          "CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","LIMIT_USAGE","PAYMENTS","MINIMUM_PAYMENTS","MIN_PAYMENTS_RATIO","PRC_FULL_PAYMENT",
                          "TENURE")


################################Outlier_Analysis#######################################
# Identifying Outliers
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s
  LC <- m-2*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

Outliers<-t(data.frame(apply(h[Numerical_Variabless], 2, mystats)))


# Outlier Treatment
h$BALANCE[h$BALANCE>5727.53]<-5727.53
h$BALANCE_FREQUENCY[h$BALANCE_FREQUENCY>1.3510787]<-1.3510787
h$PURCHASES[h$PURCHASES>5276.46]<-5276.46
h$Monthly_Avg_PURCHASES[h$Monthly_Avg_PURCHASES>800.03] <- 800.03
h$ONEOFF_PURCHASES[h$ONEOFF_PURCHASES>3912.2173709]<-3912.2173709
h$INSTALLMENTS_PURCHASES[h$INSTALLMENTS_PURCHASES>2219.7438751]<-2219.7438751
h$CASH_ADVANCE[h$CASH_ADVANCE>5173.1911125]<-5173.1911125
h$Monthly_CASH_ADVANCE[h$Monthly_CASH_ADVANCE>2558.53] <- 2558.53
h$PURCHASES_FREQUENCY[h$PURCHASES_FREQUENCY>1.2930919]<-1.2930919
h$ONEOFF_PURCHASES_FREQUENCY[h$ONEOFF_PURCHASES_FREQUENCY>0.7991299]<-0.7991299
h$PURCHASES_INSTALLMENTS_FREQUENCY[h$PURCHASES_INSTALLMENTS_FREQUENCY>1.1593329]<-1.1593329
h$CASH_ADVANCE_FREQUENCY[h$CASH_ADVANCE_FREQUENCY>0.535387]<-0.535387
h$CASH_ADVANCE_TRX[h$CASH_ADVANCE_TRX>16.8981202]<-16.8981202
h$PURCHASES_TRX[h$PURCHASES_TRX>64.4251306]<-64.4251306
h$CREDIT_LIMIT[h$CREDIT_LIMIT>11772.09]<-11772.09
h$LIMIT_USAGE[h$LIMIT_USAGE>1.1683] <- 1.1683
h$PAYMENTS[h$PAYMENTS>7523.26]<-7523.26
h$MINIMUM_PAYMENTS[h$MINIMUM_PAYMENTS>5609.1065423]<-5609.1065423
h$MIN_PAYMENTS_RATIO[h$MIN_PAYMENTS_RATIO>249.9239] <- 249.9239
h$PRC_FULL_PAYMENT[h$PRC_FULL_PAYMENT>0.738713]<-0.738713
h$TENURE[h$TENURE>14.19398]<-14.19398
str(h)


####Missing_value_Analysis########

missing_val = data.frame(apply(h,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(h)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

missing_values = data.frame(apply(h,2,function(x){sum(is.na(x))}))


##Imputuing with mean####3

h$MINIMUM_PAYMENTS[which(is.na(h$MINIMUM_PAYMENTS))] <- 721.9256368
h$CREDIT_LIMIT[which(is.na(h$CREDIT_LIMIT))] <- 4343.62
h$Monthly_Avg_PURCHASES[which(is.na(h$Monthly_Avg_PURCHASES))] <-184.8991609
h$Monthly_CASH_ADVANCE[which(is.na(h$Monthly_CASH_ADVANCE))] <- 717.7235629
h$LIMIT_USAGE[which(is.na(h$LIMIT_USAGE))] <-0.3889264
h$MIN_PAYMENTS_RATIO[which(is.na(h$MIN_PAYMENTS_RATIO))]  <- 9.3500701



checking_missing_values = data.frame(apply(h,2,function(x){sum(is.na(x))}))

#####################Feature_Selection##################3

ggcorr(h, 
       label = T, 
       label_size = 3,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "royalblue",
       layout.exp = 5,
       low = "dodgerblue", 
       mid = "gray95", 
       high = "red2",
       name = "Correlation")


####################Applying_PCA#######################

#Principal Component Analysis (PCA)
#Before determine the cluster of Customers, 
#we need to conduct PCA to reduce the dimensionality but maintain information as much as possible. 
#Therefore, we need to determine which PC

scaled.credit = scale(h)

credit_pca = prcomp(scaled.credit)


credit_pca

####Looking for variance###


summary(credit_pca)

#After looking the summary, we will decide to reduce the dimensionality when the Cumulative Proportion reach 90%, 
#which is PC10. The reason why we would like to take the 90% proportion is because that number 
#is considerably able to describe the overall customer information.

####individual data plot#####
fviz_pca_biplot(credit_pca, 
                axes = c(1:2), 
                col.var = "orange",
                col.ind = "royalblue",
                labelsize = 3) +
  theme_igray() +
  labs(title = "Biplot of PC1 and PC2")

####Outliers#####

fviz_pca_biplot(credit_pca, 
                axes = c(1:2), 
                col.var = "orange",
                col.ind = "red",
                labelsize = 3,
                select.ind = list(contrib = 5)) +
  theme_igray() +
  labs(title = "Outlier of PC1 and PC2")

h[c("502","551","1257","1605","2160"), ]

####Dimensionality Reduction####

credit_new <- credit_pca$x[,1:10]


###################K-means_Clustering#################

#After defining which dimensions that are going to used in Clustering, 
#now we will use K-Means to determine how many Clusters do we need to divide Customers, 
#which may represents their profile and hopefully we can determine what kind of tretment should be given to them.

fviz_nbclust(credit_new, 
             kmeans, 
             method = "wss",
             linecolor = "green4") +
  geom_vline(xintercept = c(4,7), linetype = 2, col = "red") +
  theme_igray()

######Clusters#########

for(i in 4:7){
  set.seed(289)
  model <- kmeans(credit_new, i)
  print(paste("WSS of K",i, "=",model$tot.withinss))
  print(paste("BSS Proportion of K",i, "=", model$betweenss/model$totss))
  print(paste("Cluster Size of K",i, "="))
  print(paste(model$size))
  print(fviz_cluster(model, h, palette = "Set1") +
          theme_igray())
}


credit_km = kmeans(credit_new, 6)

h$CLUSTER = credit_km$cluster

calinhara(credit_new,credit_km ,cn=max(6))
 