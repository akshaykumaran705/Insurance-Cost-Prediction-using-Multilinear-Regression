# Load necessary libraries
library(ggplot2)

# Load the dataset
df <- read.csv("/Users/akshaykumaran/Downloads/insurance.csv")
head(df)

# EDA - Summary and structure
summary(df)
str(df)

# EDA - Check for missing data
lapply(df, function(x) {length(which(is.na(x)))})

# Basic charts for EDA

# Distribution of Age
ggplot(df, aes(age)) + 
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
  ggtitle("Age Distribution")

# Distribution of BMI
ggplot(df, aes(bmi)) + 
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") + 
  ggtitle("BMI Distribution")

# Distribution of Charges
ggplot(df, aes(charges)) + 
  geom_histogram(binwidth = 1000, fill = "cyan", color = "black") + 
  ggtitle("Charges Distribution")

# Scatterplot of Age vs Charges
ggplot(df, aes(x = age, y = charges)) + 
  geom_point(color = "blue", alpha = 0.5) + 
  ggtitle("Age vs Charges")

# Scatterplot of smoker vs Charges
ggplot(df, aes(x = factor(smoker), y = charges, fill = factor(smoker))) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Smoker vs Charges") +
  xlab("Smoker Status") +
  ylab("Charges") +
  scale_x_discrete(labels = c("0" = "Non-Smoker", "1" = "Smoker")) +
  scale_fill_manual(values = c("skyblue", "lightcoral")) +
  theme_minimal()

ggplot(df, aes(x = factor(sex), y = charges, fill = factor(sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Sex vs Charges") +
  xlab("Gender") +
  ylab("Charges") +
  scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_fill_manual(values = c("skyblue", "lightcoral")) +
  theme_minimal()

# Scatterplot of BMI vs Charges
ggplot(df, aes(x = bmi, y = charges)) + 
  geom_point(color = "green", alpha = 0.5) + 
  ggtitle("BMI vs Charges")

g <- ggplot(fr, aes(x="", y=Freq, fill = factor(Var1))) + 
  geom_bar(width=1, stat="identity") + 
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("region Distribution", subtitle = "region of customer (US)") + 
  labs(x="region", y="count") +
  coord_polar(theta = "y", start=1)  + 
  theme(legend.position="bottom") + 
  guides(fill=guide_legend(title="")) + 
  scale_fill_manual(values = c("#58508d", "#bc5090", "#ff6361", "#ffa600"))
g

gg <- ggplot(df, aes(x=bmi, y=charges)) + 
  # geom_point(aes(col=bmi))
  geom_jitter(colour="#2e4057", alpha=0.3) + 
  ggtitle("bmi vs charges") + 
  theme(legend.position="None")

# Encode categorical features (sex, smoker, region)
encode <- function(x) {
  as.numeric(factor(x)) - 1  # Encoding as 0 and 1 (factor levels start from 1, so subtract 1)
}
df$sex <- encode(df$sex)
df$smoker <- encode(df$smoker)
df$region <- encode(df$region)
head(df$sex)

# Fit Linear Regression Model
n_train <- round(0.7 * nrow(df))  # Changed variable to `df` instead of `encoded_df`
train_idx <- sample(1:nrow(df), n_train)

train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

lm_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train_df)

# Print the model summary
summary(lm_model)

# Make predictions using the fitted model
predictions <- predict(lm_model, test_df)

# Scatterplot of Predicted Charges vs Actual Charges
ggplot(test_df, aes(x = predictions, y = charges)) + 
  geom_point(color = "red", alpha = 0.5) + 
  ggtitle("Predicted vs Actual Charges") + 
  xlab("Predicted Charges") + 
  ylab("Actual Charges")

# Add predicted charges to the test dataset
test_df$predicted_charges <- predictions

# Print the final table with actual and predicted charges

# Perform ANOVA
anova_result <- anova(lm_model)

# Display the ANOVA table
print(anova_result)

# Sample prediction for specific values
samp <- data.frame(age = 23,
                   sex = 1, 
                   bmi = 23.845,
                   children = 0,
                   smoker = 1, 
                   region = 3)

# Predict charges for sample
print(paste0("Predicted Charges: ", round(predict(lm_model, samp), 2)))
final_table <- test_df[, c("age", "sex", "bmi", "children", "smoker", "region", "charges", "predicted_charges")]
print(head(final_table))



