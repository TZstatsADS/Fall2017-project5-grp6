set.seed(1680) # for reproducibility
# install.packages("Rtsne")

library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization


# Load data 
Speed_Dating_Data <- read.csv("~/Desktop/project5/speed-dating-experiment/Speed Dating Data_bkp.csv")


# Check NA for each column 
na_count <-sapply(Speed_Dating_Data, function(y) sum(length(which(is.na(y)))))
na_count

# Field id, race, date , go out, career/career id,field/field id are categorical data 
# based on NA and variable explanation, delete expnum & match 
dim(Speed_Dating_Data)
drops <- c("match","expnum")
Speed_Dating_Data<-Speed_Dating_Data[ , !(names(Speed_Dating_Data) %in% drops)]
dim(Speed_Dating_Data)

Speed_Dating_Data[is.na(Speed_Dating_Data$age),]
# For iid = 58, 59, 136,339,340,346, most of data are NA, delete those 
# 512,129, will use average age later 
drop_age<- c(58, 59, 136,339,340,346)
Speed_Dating_Data<-Speed_Dating_Data[which(!(Speed_Dating_Data$iid %in% drop_age)),] 
dim(Speed_Dating_Data)

# For iid = 28, most of data are NA, delete those 
Speed_Dating_Data[is.na(Speed_Dating_Data$imprace),]
Speed_Dating_Data<-Speed_Dating_Data[Speed_Dating_Data$iid !=28,] 
dim(Speed_Dating_Data)

# iid=512 missing exphappy 
unique(Speed_Dating_Data[is.na(Speed_Dating_Data$exphappy),]$iid)
# iid= 413 missing date 
unique(Speed_Dating_Data[is.na(Speed_Dating_Data$date),]$iid)
table(Speed_Dating_Data$exphappy)
table(Speed_Dating_Data$date)

# randomly assign random number to missing date and exphappy 
Speed_Dating_Data[is.na(Speed_Dating_Data$exphappy),]$exphappy<-sample(1:10,1)
Speed_Dating_Data[is.na(Speed_Dating_Data$date),]$date<-sample(1:7,1)

#field = opeation research, field_cd=NA, the field code is 5
Speed_Dating_Data[is.na(Speed_Dating_Data$field_cd),]$field
Speed_Dating_Data[is.na(Speed_Dating_Data$field_cd),]$field_cd<-5

# career_c NA
unique(Speed_Dating_Data[is.na(Speed_Dating_Data$career_c),]$career)
Speed_Dating_Data[(Speed_Dating_Data$career=="lawyer"
                   &is.na(Speed_Dating_Data$career_c)),]$career_c<-1
Speed_Dating_Data[(Speed_Dating_Data$career=="law"
                   &is.na(Speed_Dating_Data$career_c)),]$career_c<-1
Speed_Dating_Data[(Speed_Dating_Data$career=="Economist"
                   &is.na(Speed_Dating_Data$career_c)),]$career_c<-7
Speed_Dating_Data[(Speed_Dating_Data$career=="tech professional"
                   &is.na(Speed_Dating_Data$career_c)),]$career_c<-10

# delete repulicate rows
df<-unique(Speed_Dating_Data)
dim(df)[1]==length(unique(Speed_Dating_Data$iid))

# age missing values 
df[which(is.na(df$age)),]$age<-mean(df$age,na.rm = TRUE)

na_count_df <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count_df

df[is.na(df$career_c),]$career_c<-10

# delete career &field
dim(df)
drops_df <- c("field","career")
df<-df[ , !(names(df) %in% drops_df)]
dim(df)


# Field  gender,race, date , go out, career/career id,field/field id are categorical data 
cols <- c("gender", "field_cd", "race", "date","go_out", "career_c")
df[cols] <- lapply(df[cols], factor)
df$age<-as.integer(df$age)
df$income<-as.numeric(df$income)
sapply(df, class)

# write.csv(df,file="~/Desktop/project5/SpeedDating.csv")

# Distance Calculation 
gower_dist <- daisy(df[,-1],metric = "gower",type = list(logratio = 3))
gower_dist
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Output most similar pair
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# Calculate silhouette width for many k using PAM
sil_width <- c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)
plot(1:10, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
lines(1:10, sil_width)

# Cluster Interpretation
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_results <- df %>%
  dplyr::select(-iid) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
df[pam_fit$medoids, ]

# Visualization
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = df$iid)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# tsne_data %>%
#   filter(X > 15 & X < 25,
#          Y > -15 & Y < -10) %>%
#   left_join(df, by = "name") %>%
#   collect %>%
#   .[["name"]]
# 


