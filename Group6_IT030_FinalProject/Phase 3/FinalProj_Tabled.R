library(readr)

file_path <- "C:/Users/Joren Hanz/Documents/cleaned_DA_FinalProj.csv"

DataAnalytics_table <- read_csv(file_path)

print("Structure of the DataAnalytics_table:")
str(DataAnalytics_table)


print(DataAnalytics_table)

View(DataAnalytics_table)