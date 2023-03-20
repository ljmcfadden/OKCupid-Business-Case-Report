# Author: Lucas McFadden
# Date: Mar-13-2023
# Purpose: OKCupid Case Analysis

# Libs
library(dplyr)
library(ggplot2)
library(officer)
library(ggthemes)
library(flextable)
library(dplyr)
library(rbokeh)
library(plotly)
library(ggrepel)

## Bring in data
profiles <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/profiles.csv')


## Clean data
# Extract first word to just get the sign
profiles$sign_clean <- sub("(^[A-Za-z]+).*", "\\1", profiles$sign) 
profiles <- profiles[, !colnames(profiles) %in% c("sign")] # Remove old sign column
profiles <- subset(profiles, !(age > 69)) # Removes outlier ages
profiles <- subset(profiles, !(height < 55)) # Removes outlier heights
profiles <- subset(profiles, !(status == "unknown")) # Removes irrelevant data

# Delete useless columns
profiles$essay0 <- NULL # Irrelevant to analysis
profiles$last_online <- NULL # Irrelevant to analysis
profiles$location <- NULL # Normally valuable but all users are located in bay area

# Replace NA's with blanks
profiles$body_type <- gsub('NA', "", profiles$body_type)
profiles$diet <- gsub('NA', "", profiles$diet)
profiles$drinks <- gsub('NA', "", profiles$drinks)
profiles$diet <- gsub('NA', "", profiles$diet)
profiles$drugs <- gsub('NA', "", profiles$drugs)
profiles$education <- gsub('NA', "", profiles$education)
profiles$ethnicity <- gsub('NA', "", profiles$ethnicity)
profiles$height <- gsub('NA', "", profiles$height)
profiles$income <- gsub('NA', "", profiles$income)
profiles$job <- gsub('NA', "", profiles$job)
profiles$offspring <- gsub('NA', "", profiles$offspring)
profiles$pets <- gsub('NA', "", profiles$pets)
profiles$religion <- gsub('NA', "", profiles$religion)
profiles$smokes <- gsub('NA', "", profiles$smokes)
profiles$speaks <- gsub('NA', "", profiles$speaks)
profiles$sign_clean <- gsub('NA', "", profiles$sign_clean)



#View updated data
head(profiles)

## Insight 1 - Data Visualization
# Bar chart to see Age Distribution
plot_ly(data = profiles, x = ~age, type = "histogram", histnorm = "count",
        marker = list(color = "black", line = list(color = "red", width = 1))) %>%
  layout(title = "Age Distribution", xaxis = list(title = "Age", range = c(17, 70), dtick = 5), yaxis = list(title = "Count")) %>%
  add_annotations(x = 50, y = 4000, text = "Hover over the bars for details", showarrow = FALSE, font = list(size = 16))

# Dataframe creation for distribution of drink prefs
plotDF <- data.frame(table(profiles$sex,  profiles$status))

# Stacked plot
ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity")


# Calculate the percentage of each variable in the status column
drinks_pct <- prop.table(table(profiles$drinks)) * 100

# Plot percentages as bar chart
barplot(drinks_pct, main = "Drinks Variable Percentages",
        xlab = "Drinks", ylab = "Percentage",
        col = "red", horiz = FALSE, las = 2)






## Insight 2 - Data Visualization
# Create a data frame with the counts of each body type
body_type_counts <- as.data.frame(table(profiles$body_type))

# Remove missing values
body_type_counts <- body_type_counts[!is.na(body_type_counts$Var1), ]

# Create a cluster chart using ggplot2
ggplot(body_type_counts, aes(x = Freq, y = reorder(Var1, -Freq))) +
  geom_point(color = "red", size = 3) +
  geom_text_repel(aes(label = Var1), size = 3) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(body_type_counts$Freq) * 1.1)) +
  labs(title = "Body Type Cluster Chart", x = "Count", y = "Body Type") +
  theme_bw()


# Count of each body type
body_type_count <- table(profiles$body_type)

# Creation of dataframe
body_type_df <- data.frame(body_type = names(body_type_count),
                           count = as.numeric(body_type_count))

# Put the results in descending order
body_type_df <- body_type_df[order(body_type_df$count, decreasing = TRUE), ]

# Choosing the top 5
top_5_body_type_df <- head(body_type_df, 5)

# Display results as a bar chart
ggplot(data = top_5_body_type_df, aes(x = body_type, y = count)) + 
  geom_bar(stat = "identity", fill = "red") + 
  xlab("Body Type") + 
  ylab("Count") +
  ggtitle("Top 5 Body Types")


## Insight 3 - Data Visualization
# Create a data frame with counts of signs for single users
df <- data.frame(table(subset(profiles, status == "single")$status, subset(profiles, status == "single")$sign_clean))


# Create the grouped bar chart
ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Status", y = "Count", fill = "Sign Clean") +
  theme_minimal()



## Insight 4 - Data Visualization
# Create a new column "status_grouped" that segments the "status" column
profiles <- profiles %>%
  mutate(status_grouped = case_when(
    status %in% c("single", "available") ~ "Seeking a partner",
    status %in% c("married", "seeing someone") ~ "Seeking a friend",
    TRUE ~ "Other"
  ))

# Create a summary table of the segmented data
status_summary <- profiles %>%
  group_by(status_grouped) %>%
  summarise(count = n(), .groups = "keep")

# Plot the results using a bar chart
ggplot(status_summary, aes(x = status_grouped, y = count, fill = status_grouped)) +
  geom_col() +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x = "Status", y = "Count", title = "Relationship Status") +
  theme_minimal()