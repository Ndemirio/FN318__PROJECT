#Opening and Loading the data to a variable name 'pension_data'
pension_data<-read.csv('C:\\Users\\HP\\Desktop\\FN318\\pension_fund_data_tanzania.csv')

#Exploring the data by using some in built functions
dim(pension_data) #this returns the dimensions of the data i.e rows and columns
str(pension_data) #this returns the internal structure of the data variable i.e chr,int,num
summary(pension_data) #provides the summary statistics for the numeric data
View(pension_data)  #displays the data in spreadsheet style form 


#DATA CLEANING PROCESS
#1. Handling Missing values and duplicates
any(is.na(pension_data)) #a logical test to check if there is any missing values
anyDuplicated(pension_data) # checks if there is any duplicates

#checks which column has missing values and returns their name
na_in_column<-sapply(pension_data, function(x)anyNA(x))
print(names(pension_data)[na_in_column])

#updates the pension_data by omitting the rows containing the missing variables
pension_data2<-na.omit(pension_data) #omits the entire rows containing 'NA' in the data set
omitted_dataset<-pension_data[!complete.cases(pension_data),]

#2.Inspecting important variables to handle Outliers and check distribution'
im_col<-c('age','service_years','pensionable_earnings','fund_balance','salary')

# Create a boxplot for chosen numeric column
for (colb in im_col) {
  boxplot(pension_data2[[colb]], main = paste("Boxplot of", colb),
          xlab = colb,horizontal =T)
}

# Create a Histogram for chosen numeric column
library(ggplot2)
for (plot in c('age','service_years')){
  p<-ggplot(pension_data2, aes_string(x = plot)) + 
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black")+ labs(title=paste('Histogram of',plot), x='age of members',y='')
  print(p)
}

for (colh in c('salary','pensionable_earnings')){
  hist(pension_data2[[colh]], main= paste('Histogram of', colh), xlab= colh)
}

#inpecting the age and sex distribution by use of population pyramids
ggplot(pension_data2, aes(x = age, fill = gender)) +
  geom_bar(data = subset(pension_data2, gender == "Female"), aes(y=-..count..), stat = "count", width = 5) +
  geom_bar(data = subset(pension_data2, gender == "Male"), aes(y=..count..), stat = "count", width = -5) +
  scale_fill_manual(values = c("Female" = "grey", "Male" = "darkgreen")) +
  labs(title = "Population Pyramids for members in the fund", x = "Age", y = "Count") +
  coord_flip()+ theme_minimal()

# Create a pie chart for the 'gender' column
par(cex=1, mar=c(0.5,0.5,0.5,0.5))
gender_counts <- table(pension_data2$gender)
pie(gender_counts, labels = paste0(names(gender_counts), " (", round(gender_counts / sum(gender_counts) * 100, 1), "%)"), main = "Gender Distribution")

#Create a pie chart for the pension plan type
par(cex=1, mar=c(0.5,0.5,0.5,0.5))
type_counts <- table(pension_data2$pension_plan_type)
pie(type_counts, labels = paste(names(type_counts), "(", round(type_counts / sum(type_counts) * 100, 1), "%)", sep = ""), main = "Pension Plan Type Distribution")

#inspecting some important variables for pension calculations
mean(pension_data2$inflation_rate)
mean(pension_data2$average_salary_increase)
mean(pension_data2$interest_rate)

#separating active members from the inactive/retired members
active_members<-pension_data2[as.integer((pension_data2$retirement_age)-(pension_data2$age))>0,]
retired_members<-pension_data2[as.double((pension_data2$retirement_age)-(pension_data2$age))<=0,]

#trimming the data to remain with important columns for computations
trim_active_members<-subset(active_members,select=c('member_id','age','service_years','pensionable_earnings','retirement_age','salary',))
trim_retired_members<-subset(retired_members, select=c('member_id','age','service_years','pensionable_earnings','retirement_age','salary'))

#opening an excel file containiing important prob and annuity factors for computations
library(readxl)      #Loading important packages for accessing the excel file
life_table<-read_xlsx('C:\\Users\\HP\\Desktop\\FN318\\annuity_lifetable.xlsx')   #opening the excel file 

#Present value for benefits accrued due to past services for active members
for (a in seq_len(nrow(trim_active_members))) {
  indiv_factor <- numeric()  # Initialize empty vector for each age
  indiv_benefit <- numeric()  # Initialize empty vector for each age
  age_in_loop <- trim_active_members$age[a]  # Keep track of age without modifying original 'age' column
  for (l in seq_len(nrow(life_table))) {
    if (trim_active_members$age[a] == life_table$age[l]) {
      p <- 2
      inc=0.5
      while (p <= 11 & (age_in_loop+inc <= trim_active_members$retirement_age[a])) {
        prob_ann_factor <- function() {
          prob_ann_fact <- as.double(life_table[l, p] * life_table$ax[p - 1] * (((1 + 0.09) / (1 + 0.10))^(p - 1.5)))
          return(prob_ann_fact)
        }
        indiv_factor <- c(indiv_factor, prob_ann_factor())
        age_in_loop <- age_in_loop + 1  # Increment age by 1
        p <- p + 1
      }
      
      indiv_benefit <- append(indiv_benefit, as.double((1 / 48 * trim_active_members$pensionable_earnings[a] * sum(indiv_factor))))
      trim_active_members[a, "indiv_benefit"] <- indiv_benefit
    }
  }
}

                                                                                                                                          
#Present value for benefits accrued due to past services for pensioners
for (a in seq_len(nrow(trim_retired_members))) {
  retired_benefit <- numeric()  # Initialize empty vector for each age
  age_in_loop <- trim_retired_members$age[a]  # Keep track of age without modifying original 'age' column
  for (l in seq_len(nrow(life_table))) {
    if (trim_retired_members$age[a] == life_table$age[l]) {
      retired_benefit <- append(retired_benefit, as.double((trim_retired_members$pensionable_earnings[a]*life_table$ax[l])))
      trim_retired_members[a, "retired_benefit"] <- retired_benefit
    }
  }
}

Total_Expected_Benefit<-sum(trim_active_members$indiv_benefit)+sum(trim_retired_members$retired_benefit)
print(Total_Expected_Benefit)

#Projecting Fund Balance
fund_balance_data <- pension_data2[, c("member_id", "fund_balance")]      # Select relevant columns for fund balance projection
interest_rate <- 0.10
years <- 10

# Function to calculate future value
project_balance <- function(current_balance, rate, n_years) {
  return(current_balance * (1 + rate) ^ n_years)
}

#calculating projected fund balance
fund_balance_data$projected_fund_balance <- sapply(fund_balance_data$fund_balance, project_balance, rate = interest_rate, n_years = years)


#PERFORMING CLUSTER ANALYSIS

# Load required packages
library(cluster)
library(tidyverse)

#Preparing Data for cluster analysis
pcluster<-subset(pension_data2,select=c('member_id','salary','loan_balance'))

#scaling the data to find the optimal number of clusters
pcluster$standardized_salary<-scale(pcluster$salary)
pcluster$standardized_loanbalance<-scale(pcluster$loan_balance)

# Perform K-means clustering
set.seed(0)  # For reproducibility
kmeans_result <- kmeans(pcluster[, c("standardized_salary", "standardized_loanbalance")], centers = 4, nstart = 20)

# Add cluster labels to the data
pcluster$cluster <- kmeans_result$cluster

# Create the ggplot
p <- ggplot(pcluster, aes(x = standardized_salary, y = standardized_loanbalance, color = as.factor(cluster))) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_viridis_d(begin = 0.1, end = 0.9, name = "Cluster", guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(title = "Cluster Analysis of Salary and Loan Balance",
       x = "Standardized Salary",
       y = "Standardized Loan Balance") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))  # Center the plot title

# Print the plot
print(p)

# Plot the results
cluster_colors<-c('red','blue','green','purple')
plot(pcluster$standardized_salary, pcluster$standardized_loanbalance, col = cluster_colors[kmeans_result$cluster], pch = 20, xlab = "Standardized Salary", ylab = "Standardized Loan Balance", main = "Cluster Analysis of Salary and Loan Balance")
legend("topright", legend = unique(pcluster$cluster), col = unique(kmeans_result$cluster), pch = 20)

# Summarize cluster characteristics
library(dplyr)                      #Loading important package
library(magrittr)                   #Loading important package 
cluster_summary <- pcluster %>% 
  group_by(cluster) %>% 
  summarise(
    salary_mean = mean(salary),
    salary_sd = sd(salary),
    salary_min = min(salary),
    salary_max = max(salary),
    loan_balance_mean = mean(loan_balance),
    loan_balance_sd = sd(loan_balance),
    loan_balance_min = min(loan_balance),
    loan_balance_max = max(loan_balance)
  )
print(cluster_summary)

# Subsetting first 1000 rows for testing the code fragment to calculate indiv_benefit and retired_benefit for pensioners
test_data <- head(pension_data2, 100000)
write.csv(test_data, file='test_data.csv')
trim_retired_members2<-head(trim_retired_members,1000)
observed_Data<-read.csv('C:\\Users\\HP\\Desktop\\FN318\\benefit_active_members.csv')

#writing the output into a file
write.csv(trim_active_members,file='benefit_active_members.csv')
write.csv(trim_retired_members,file='benefit_pensionsers.csv')
write.csv(fund_balance_data, file="projected_fund_balance.csv")
write.csv(cluster_summary, file='cluster summary.csv')
  

