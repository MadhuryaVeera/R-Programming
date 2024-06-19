# Load the dataset
data(mtcars)

# View the structure of the dataset
str(mtcars)

# Summary statistics
summary(mtcars)

# Descriptive statistics for specific variables
mean_mpg <- mean(mtcars$mpg)
sd_hp <- sd(mtcars$hp)

# Histogram of Miles per Gallon (mpg)
hist(mtcars$mpg, main = "Histogram of Miles per Gallon (mpg)", xlab = "Miles per Gallon",col="blue")

# Boxplot of Miles per Gallon (mpg) by number of cylinders
boxplot(mpg ~ cyl, data = mtcars, main = "Boxplot of Miles per Gallon (mpg) by number of cylinders", xlab = "Number of Cylinders", ylab = "Miles per Gallon",col="salmon")

# Scatterplot of Miles per Gallon (mpg) vs. Horsepower (hp)
plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per Gallon", main = "Scatterplot of Miles per Gallon (mpg) vs. Horsepower (hp)",col="green")

# Correlation plot
correlation_matrix <- cor(mtcars)
corrplot::corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, main = "Correlation Plot")

# Pairwise scatterplot matrix
pairs(mtcars[, c("mpg", "disp", "hp", "wt")], main = "Pairwise Scatterplot Matrix",col="blue")

# Barplot of number of cylinders
barplot(table(mtcars$cyl), main = "Barplot of Number of Cylinders", xlab = "Number of Cylinders", ylab = "Frequency",col="orange")

# Pie chart of transmission types
pie(table(mtcars$am), main = "Pie Chart of Transmission Types", labels = c("Automatic", "Manual"), col = c("skyblue", "salmon"))

# Hypothesis testing (e.g., t-test)
# Is there a significant difference in miles per gallon (mpg) between cars with 4 and 6 cylinders?
t_test_result <- t.test(mtcars$mpg ~ mtcars$cyl)
print(t_test_result)

# Regression analysis
# Predicting miles per gallon (mpg) based on horsepower (hp)
model <- lm(mpg ~ hp, data = mtcars)
summary(model)

# Visualize the regression line
plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per Gallon", main = "Regression Analysis")
abline(model, col = "red")

