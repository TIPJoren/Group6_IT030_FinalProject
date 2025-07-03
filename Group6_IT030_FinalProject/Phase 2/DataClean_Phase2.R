library(dplyr)

DataAnalytics <- read.csv("C:/Users/Joren Hanz/Documents/DataAnalytics_FinalProj.csv")

# Remove rows with 'Unknown' in the Attack.Source column
cleaned_dataset <- DataAnalytics %>%
  filter(Attack.Source != "Unknown")

# Function to replace "Man-in-the-Middle" with "MITM" in Attack.Type column
replace_mitm <- function(data) {
  data %>%
    mutate(Attack.Type = ifelse(Attack.Type == "Man-in-the-Middle", "MITM", Attack.Type))
}

# Apply the function to replace Man-in-the-Middle with MITM
cleaned_dataset <- replace_mitm(cleaned_dataset)

# --- SAVE THE CLEANED DATA (OPTIONAL) ---
# If you want to save the cleaned data back to a new CSV file
write.csv(cleaned_dataset, "cleaned_DA_FinalProj.csv", row.names = FALSE)