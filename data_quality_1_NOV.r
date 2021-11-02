
#Data Quality (last edit on 28 OCT -2021)

#Packages

install.packages('Hmisc')
install.packages('naniar')
install.packages('VIM')
install.packages('finalfit')
install.pacakges('vtable')
install.packages("tidyr")  
install.packages("gtsummary")
install.packages("psych")

#install.packages('fBasics')

############################################

#libraries

Sys.setlocale("LC_CTYPE","arabic")
library(dplyr)
library(Hmisc)
library(naniar)
library(VIM)
library(finalfit)
library(tidyr)
library(dplyr)
library(mice)
library(vtable)
library(tibble)
library(plyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(reshape2)
library(corrplot)
library(ggcorrplot)
library(psych)
#library(fBasics)
############################################

#data

data=read.csv("C:/Users/admin/Desktop/data.csv",encoding = 'utf-8')

############################################

#Exploring the data

#dataset shape
dim(data)

#number of records
nrow(data) 

#number of columns
length(data) 

#statistical summary of the data
summary(data)
describe(data)
st(data)
summary1_df<-data %>% tbl_summary()  
summary1_df

#columns data type
df_type<-as.data.frame(sapply(data, class))
colnames(df_type)[which(names(df_type) == "sapply(data, class)")] <- "type"
df_type1<-rownames_to_column(df_type, var="column")
df_type1

pct_miss(data) # percentage of missing value in the data.
n_miss(data) # number of missing values in the 
n_complete(data) # without missing value


############################################

#duplication

#number of duplicated rows
nrow(data[duplicated(data), ])

#number of unique rows
nrow( data[!duplicated(data), ]) 

############################################

#missing values 

#not null values counts
summary_counts<-as.data.frame(colSums(!is.na(data)))
View(summary_counts)

#null values
summary_counts_null<-as.data.frame(colSums(is.na(data)))
summary_counts_null

#unique_values
summary_un<-data %>%as.data.frame()%>% summarise_all(funs(n_distinct(.)))
summary_un1<-as.data.frame(t(summary_un))
View(t(summary_un))
sapply(data, n_distinct)

############################################

data_values_counts <- function (df){
  #merging(null, not null, distinct)
  data_values_counts<-cbind(summary_counts_null,summary_counts,summary_un1)
  colnames(data_values_counts)[which(names(data_values_counts) == "V1")] <- "distinct values"
  colnames(data_values_counts)[which(names(data_values_counts) == "colSums(!is.na(data))")] <- "non null values"
  colnames(data_values_counts)[which(names(data_values_counts) == "colSums(is.na(data))")] <- " null values"
  #names(dimnames(data_values_counts)) <- c("columns")
  data_values_counts1<-rownames_to_column(data_values_counts, var="column")
  data_values_counts1
}

data_values_summary<-data_values_counts(data)
data_values_summary

############################################

#min,max,quantile 

min_max_quan<-function(df){
  #for each column
  lis<-list()
  lis1<-list()
  lis11<-list()
  lis_<-list()
  q1<-list()
  q3<-list()
  iqr<-list()
  Cols <-  which(sapply(data, is.numeric))
  for(j in Cols){
    #print(colnames(data[j]))
    v1<-colnames(data[j])
    #print (min(data[j], na.rm = TRUE))
    #print (max(data[j], na.rm = TRUE))
    v<-min(data[j], na.rm = TRUE)
    lis1<-c(lis1,v1)
    lis<-c(lis,v)
    
    v2<-max(data[j], na.rm = TRUE)
    # lis11<-c(lis11,v1)
    lis_<-c(lis_,v2)
    
    Q1 <- quantile(data[j], .25,na.rm = TRUE)
    q1<-c(q1,Q1) 
    #print (Q1)
    Q3 <- quantile(data[j], .75,na.rm = TRUE)
    #print (Q3)
    q3<-c(q3,Q3)
    x_num <- as.numeric(unlist(data[j]))
    
    iq <- IQR(x_num,na.rm = TRUE)
    iqr<-c(iqr,iq)
    #print(iq)
    #print(as.data.frame(t(min(data[j]))))
    #list.append(lis,v,after=1)
  }
  miss<-as.data.frame(miss_var_summary(data))
  colnames(miss)[which(names(miss) == "variable")] <- "column"
  
  
  min_<-cbind(lis1,lis)
  colnames(min_)<-c('column','min')
  
  max_<-cbind(lis1,lis_)
  colnames(max_)<-c('column','max')
  
  q1_<-cbind(lis1,q1)
  colnames(q1_)<-c('column','q1')
  
  q3_<-cbind(lis1,q3)
  colnames(q3_)<-c('column','q3')
  
  iqr_<-cbind(lis1,iqr)
  colnames(iqr_)<-c('column','iqr')
  
  all_<-join_all(list(as.data.frame(df_type1),as.data.frame(data_values_counts1),
                      as.data.frame(miss),as.data.frame(min_)
                      ,as.data.frame(max_),as.data.frame(q1_),as.data.frame(q3_),
                      as.data.frame(iqr_)), by = 'column', type = 'full')
}

data_values_summary1<-min_max_quan(data)
View(data_values_summary1)

############################################

#missing_pattern 

#missing summary and visualization
gg_miss_var(data)
gg_miss_upset(data)

#missing pattern
res<-summary(aggr(data, sortVar=TRUE))#$combinations
matrixplot(data, sortby = 2)
vis_miss(data, sort_miss = TRUE) 
vis_miss(data, cluster =  TRUE) 
data %>% missing_plot()


data1=data[ , colSums(is.na(data)) != 0]
gg_miss_fct(data1,fct=AGE)  #you can change fct to the column that you wnat to check

md.pattern(data, plot = TRUE, rotate.names = TRUE)
#aggr(data, prop = F, numbers = T)
#aggr(data, prop = T, numbers = T)
#scattmatrixMiss(data, interactive = F, highlight = c("AGE"))

#Now, looking at the relationship between the presence of missing values in each variable and the observed values in other variables:

data1_<-data %>% dplyr::select(where(is.numeric))
View(cor(data1_, y, use="pairwise.complete.obs"))
df_described<-describeBy(data1)
View(as.data.frame(df_described))
############################################

#Correlation

## Create data frame indicating missingness by 1
x<- as.data.frame(abs(is.na(data)))
## Select columns with some (but not all) missing values
y <- x[,sapply(x, sd) > 0]

## Create a correlation matrix: Variables missing together have high correlation
cor(y)


corrplot(cor(y), type="upper", order="hclust")

ggcorrplot(cor(y),hc.order = FALSE, lab = TRUE , lab_size = 3, pch = 4,
           pch.col = "black",pch.cex = 5, tl.cex = 10, tl.col = "black", 
           tl.srt = 45,digits = 2)

############################################
#Values Distribution


data_long <- data1_ %>% pivot_longer(colnames(data1_)) %>% as.data.frame()
head(data_long)

# Draw numeric columns as histogram

options(repr.plot.width=15, repr.plot.height=8)

ggp1 <- ggplot(data_long, aes(x = value)) +    
  geom_histogram(fill='skyblue') + 
  facet_wrap(~ name, scales = "free")
ggp1+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#boxplot(data1)

options(scipen=10000)
ggp2<-ggplot(stack(data1), aes(x = ind, y = values)) +
  geom_boxplot() +
  coord_flip()
ggp2+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#######################################
#final outlier flag
Cols <-  which(sapply(data, is.numeric))
for(j in Cols){
  print(j)
  print(colnames(data[j]))
  x_num <- as.numeric(unlist(data[j]))
  iqr<-IQR(x_num,na.rm = TRUE)
  q1<-as.numeric(quantile(x_num,0.25,na.rm = TRUE))
  q3<-as.numeric(quantile(x_num,0.75,na.rm = TRUE))
  mild_low<-q1-(1.5*iqr)
  mild_high<-q3+(1.5*iqr)
  new_variable<-x_num[x_num>mild_low & x_num<mild_high]
  data$outlier = 0
  data[which(x_num %in% new_variable),"outlier"] <- 1
  colnames(data)[which(names(data) == "outlier")] <- paste0("outlier",colnames(data[j]))

}

View(data)

#############################
#number of outliers in each column

out_totla<-function(df){
  df <-data %>% dplyr:: select(starts_with("outlier"))
  
  #data[grepl('outlier', colnames(data))]
  for(j in colnames(df)){
    print(df$j)
    x<-df%>%group_by(df[j])%>%dplyr::summarise(n=n())
    print(x)
    print(typeof(x))
    #return(x)
  }
}

outlierts_<-out_totla(data)


##################################
#logistic Regression

for( i in 1:ncol(data1)){
  print(i)
  data1[i] <- ifelse(is.na(data1[i]), 1, 0)
  
}

model <- glm(AGE ~.,data=data1)
summary(model)
##################################
#column length
for(j in colnames(data)){
  print(data[[j]])
  data$len<-nchar(as.character(data[[j]]))
  print(data$len)
  colnames(data)[which(names(data) == "len")] <- paste0("len",j)
}
