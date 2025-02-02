library(readr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(VIM)

Attrition <- read_csv("/Users/wendiwang/Desktop/university/is\ 445/Week1/Employee_Attrition.csv")
na_count <- colSums(is.na(Attrition))/nrow(Attrition)
round(na_count, 2)
print(na_count) # All 0.04991449
Attrition <- na.omit(Attrition)

avg <- ggplot(Attrition, aes(x = average_montly_hours)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white",
                 bins = 50) + 
  labs(title = "Average Montly Hours of Employees",
       x = "average_montly_hours")
ggsave("Uni-Quant-Average_Montly_Hours_of_Employees.png", plot=avg)

satis <- ggplot(Attrition, aes(x = satisfaction_level)) +
  geom_density() + 
  labs(title = "Satisfaction Level of Employees")
ggsave("Uni-Quant-Satisfaction_Level_of_Employees.png", plot=satis)

dept_count <- table(Attrition$dept)
percent <- paste(names(dept_count), round(100 * dept_count / sum(dept_count), 2), "%")
png("Uni-Cate-Working_Department_Distribution.png")
pie(dept_count, 
    main = "Working Department Distribution", 
    labels = percent,
    col = rainbow(length(dept_count)))
dev.off()

salary_count <- table(Attrition$salary)
percent <- paste(names(salary_count), round(100 * salary_count / sum(salary_count), 2), "%")
png("Uni-Cate-Salary_Distribution.png")
pie(salary_count, 
    main = "Salary Distribution", 
    labels = percent,
    col = rainbow(length(salary_count)))
dev.off()

cat_cat <- ggplot(Attrition, aes(x = dept, fill = salary)) + 
  geom_bar(position = "stack", width=0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Bi-Cat-Cat-Relationship Between Working Department and Salary.png", cat_cat)

quant_cat <- ggplot(Attrition, aes(x = satisfaction_level, fill = salary)) +
  geom_density(alpha = 0.4) +
  labs(title = "Satisfaction Level Distribution by Salary")
ggsave("Bi-Quant-Cat-Satisfaction_Level_Distribution_by_Salary.png", plot=quant_cat)

quant_cat <- ggplot(Attrition, aes(x = satisfaction_level, fill = dept)) +
  geom_density(alpha = 0.4) +
  labs(title = "Satisfaction Level Distribution by Department")
ggsave("Bi-Quant-Cat-Satisfaction_Level_Distribution_by_Department.png", plot=quant_cat)

quant_cat <- ggplot(Attrition, aes(x = time_spend_company, fill = salary)) +
  geom_density(alpha = 0.4) +
  labs(title = "Time Spent Company by Salary")
ggsave("Bi-Quant-Cat-Time_Spent_Company_by_Salary.png", plot=quant_cat)
