#1.import data
read_file <- read.csv("C:/Users/admin/Documents/Zalo Received Files/Intel_CPUs.csv")
# 2. Chuyển về đúng định dạng cho dữ liệu 
read_file <- data.frame(lapply(read_file, function(x) ifelse(x %in% c("", "N/A"), NA, x)))
# Kiểm tra cấu trúc của dữ liệu
str(read_file)
#loss data
# Vẽ biểu đồ
#kiểm tra sơ bộ 6 hàng đầu tiên trong bảng
head(read_file)

# Kiểm tra giá trị bị mất trong mỗi cột
freq.na <- function(x) sum(is.na(x))
missing_values <- sapply(read_file, freq.na)

# Tính toán % giá trị bị mất
percentage_missing <- (missing_values / nrow(read_file)) * 100

# xem giá trị mising
print(percentage_missing)


# Set the margin to allow for longer y-axis labels
par(mar = c(5, 10, 4, 2) + 1.5)

# Plot the percentage missing values with rotated column names
barplot(percentage_missing, names.arg = colnames(read_file), col = "skyblue", main = "Percentage of Missing Values by Column", ylab = "Percentage Missing", las = 2, cex.names = 0.7)

# Plot the percentage missing values with rotated column names and horizontal orientation
barplot(percentage_missing, names.arg = colnames(read_file), col = "skyblue", main = "Percentage of Missing Values by Column", xlab = "Percentage Missing", horiz = TRUE,las = 1, cex.names = 0.7)


#2.2. loc bien
data = data.frame(name =read_file$Product_Collection, 
                  lithography= read_file$Lithography,
                  number_cores = read_file$nb_of_Cores, 
                  number_threads= read_file$nb_of_Threads ,
                  cache =read_file$Cache,
                  power = read_file$TDP,
                  Pfrequency = read_file$Processor_Base_Frequency,
                  instruction_set = read_file$Instruction_Set,
                  bandwidth = read_file$Max_Memory_Bandwidth
)
# chuyen ve dung dinh dang
data <- data.frame(lapply(data, function(x) ifelse(x == "", NA, x)))

#3.Lam sach du lieu
#3.1 kiem tra du lieu khuyet
anyNA(data)

#3.2 Lập bảng thống kê dữ liệu khuyết
install.packages("questionr")
library(questionr)
head(freq.na(data))

data$lithography <- as.numeric(gsub("[^0-9]", "", data$lithography))
data$power <- as.numeric(gsub("[^0-9]", "", data$power))
data$Pfrequency <- as.numeric(gsub("[^0-9]", "", data$Pfrequency))
data$bandwidth <- as.numeric(gsub("[^0-9]", "", data$bandwidth))
data$instruction_set <- as.numeric(gsub("[^0-9]", "", data$instruction_set))

data$cache <- sapply(data$cache, function(x) {
  if (grepl("MB", x)) {
    as.numeric(gsub("[^0-9]", "", x)) * 1024
  } else {
    as.numeric(gsub("[^0-9]", "", x))
  }
})

library(dplyr)

# Thay thế giá trị bị thiếu trong cột "lithography" bằng giá trị trung bình
cleaned_data <- data %>%
  mutate(lithography = ifelse(is.na(lithography), ceiling(mean(lithography, na.rm = TRUE)), lithography)) %>%
  mutate(cache = ifelse(is.na(cache), ceiling(mean(cache, na.rm = TRUE)), cache)) %>%
  mutate(power = ifelse(is.na(power), ceiling(mean(power, na.rm = TRUE)), power)) %>%
  mutate(bandwidth = ifelse(is.na(bandwidth), ceiling(mean(bandwidth, na.rm = TRUE)), bandwidth)) %>%
  mutate(number_threads = ifelse(is.na(number_threads), ceiling(mean(number_threads, na.rm = TRUE)), number_threads)) %>%
  mutate(Pfrequency = ifelse(is.na(Pfrequency), ceiling(mean(Pfrequency, na.rm = TRUE)), Pfrequency)) %>%
  mutate(instruction_set = ifelse(is.na(instruction_set), ceiling(mean(instruction_set, na.rm = TRUE)), instruction_set))


#3.3 Tính trung vị (median)
median_lithography <- median(cleaned_data$lithography)
mean_lithography <- mean(cleaned_data$lithography)
min_lithography <- min(cleaned_data$lithography)
max_lithography <- max(cleaned_data$lithography)

median_cores <- median(cleaned_data$number_cores)
mean_cores <- mean(cleaned_data$number_cores)
min_cores <- min(cleaned_data$number_cores)
max_cores <- max(cleaned_data$number_cores)

median_threads <- median(cleaned_data$number_threads)
mean_threads <- mean(cleaned_data$number_threads)
min_threads <- min(cleaned_data$number_threads)
max_threads <- max(cleaned_data$number_threads)

median_cache <- median(cleaned_data$cache)
mean_cache <- mean(cleaned_data$cache)
min_cache <- min(cleaned_data$cache)
max_cache <- max(cleaned_data$cache)

median_power <- median(cleaned_data$power)
mean_power <- mean(cleaned_data$power)
min_power <- min(cleaned_data$power)
max_power <- max(cleaned_data$power)

median_Pfrequency <- median(cleaned_data$Pfrequency)
mean_Pfrequency <- mean(cleaned_data$Pfrequency)
min_Pfrequency <- min(cleaned_data$Pfrequency)
max_Pfrequency <- max(cleaned_data$Pfrequency)

median_instruction_set <- median(cleaned_data$instruction_set)
mean_instruction_set <- mean(cleaned_data$instruction_set)
min_instruction_set <- min(cleaned_data$instruction_set)
max_instruction_set <- max(cleaned_data$instruction_set)

median_bandwidth <- median(cleaned_data$bandwidth)
mean_bandwidth <- mean(cleaned_data$bandwidth)
min_bandwidth <- min(cleaned_data$bandwidth)
max_bandwidth <- max(cleaned_data$bandwidth)

result_table <- data.frame(
  Statistic = c("Median","Mean", "Minimum", "Maximum"),
  Lithography = c(median_lithography,mean_lithography, min_lithography, max_lithography),
  Cores = c(median_cores,mean_cores, min_cores, max_cores),
  Threads = c(median_threads,mean_threads, min_threads, max_threads),
  Cache = c(median_cache,mean_cache, min_cache, max_cache),
  Power = c(median_power,mean_power, min_power, max_power),
  Instruction_set = c(median_instruction_set,mean_instruction_set, min_instruction_set, max_instruction_set),
  Pfrequency = c(median_Pfrequency,mean_Pfrequency, min_Pfrequency, max_Pfrequency),
  Bandwidth = c(median_bandwidth,mean_bandwidth, min_bandwidth, max_bandwidth)
)
#3.4 Thống Kê Tả 
summary(cleaned_data)
# Scatter plots (so sánh tương quan tỉ lệ một chỉ số với power)
plot(cleaned_data$power, cleaned_data$lithography)
plot(cleaned_data$power, cleaned_data$number_cores)
plot(cleaned_data$power, cleaned_data$cache)
plot(cleaned_data$power, cleaned_data$Pfrequency)
plot(cleaned_data$power, cleaned_data$bandwidth)
# histogram
hist(cleaned_data$lithography)
hist(cleaned_data$number_cores)
hist(cleaned_data$number_threads)
hist(cleaned_data$cache)
hist(cleaned_data$power)
hist(cleaned_data$Pfrequency)
hist(cleaned_data$instruction_set)
hist(cleaned_data$bandwidth)
# boxplot
boxplot(cleaned_data$lithography)
title("Boxplot of lithography")
boxplot(cleaned_data$number_cores)
title("Boxplot of number_cores")
boxplot(cleaned_data$number_cores)
title("Boxplot of number_cores")
boxplot(cleaned_data$cache)
title("Boxplot of cache")
boxplot(cleaned_data$power)
title("Boxplot of power")
boxplot(cleaned_data$Pfrequency)
title("Boxplot of Pfrequency")
boxplot(cleaned_data$instruction_set)
title("Boxplot of instruction_set")
boxplot(cleaned_data$bandwidth)
title("Boxplot of bandwidth")
#4 thống kê suy luận
#4.1 Giả định
# Tạo mô hình hồi quy tuyến tính
model <- lm(power ~ lithography + number_cores + number_threads + cache + Pfrequency + instruction_set + bandwidth, data=cleaned_data)
# Lấy phần dư từ mô hình
residuals <- residuals(model)

# Thực hiện phép kiểm định Shapiro-Wilk cho phần dư
shapiro_test_result <- shapiro.test(residuals)

# In kết quả
print(shapiro_test_result)
# Kiểm tra tính tuyến tính
summary(model)
#predicted_values <- predict(model)
#Kiểm tra đa cộng tuyến 
install.packages("car")
library(car)
vif_values <- vif(model)
print(vif_values)
#4.2 Hồi quy tuyến tính bội 
#4.2.1 Tách dữ liệu
#Chia dữ liệu thành tập huấn luyện và tập kiểm thử:
set.seed(123)  # Đặt seed để có thể tái tạo kết quả

sample <- sample(c(TRUE, FALSE), nrow(cleaned_data), replace = TRUE, prob = c(0.8, 0.2))
train <- cleaned_data[sample, ]
test <- cleaned_data[!sample, ]

#4.2.2 Lắp mô hình
#4.2.3 Hồi quy từng bước
stepwise_model <- step(model, direction = "both")
summary(stepwise_model,  direction = "both")
coefficients <- coef(stepwise_model)
cat("power =", coefficients[1], "+", coefficients[2], "* lithography +", 
    coefficients[3], "* number_cores +",coefficients[4], "* number_threads +", 
    coefficients[5], "* Pfrequency +", coefficients[6], "* instruction_set")
#4.2.5 Dự đoán mô hình 
predicted_values <- predict(stepwise_model)
comparison <- data.frame(Predicted = predicted_values, Original = cleaned_data)
pred1 <- predict(stepwise_model, newdata = test)
pred <- data.frame(predict(stepwise_model, newdata = test))
compare <- cbind(test$power,pred)
colnames(compare) <- c("test_set","prediction")
head(compare,15)
MSE <- mean((test$power - pred1)^2)
SSE <- sum((test$power - pred)^2)
plot(test$power, pred1, main="Scatter Plot with y=x Line", xlab="X", ylab="Y")
abline(a=0, b=1, col="red")
SST <- sum((test$power - mean(test$power))^2)
cat((SST - SSE )/SST)
#mở rộng  
model_poly2 <- lm(power ~ poly(lithography, 2) + poly(number_cores, 2) + poly(number_threads, 2) 
            + poly(cache, 2) + poly(Pfrequency, 2) + poly(instruction_set, 2) + 
              poly(bandwidth, 2), data=cleaned_data)

summary(model_poly2)
coef(model_poly2)
pred2 <- predict(model_poly2, newdata = test)
MSE2 <- mean((test$power - pred2)^2)

model_poly3 <- lm(power ~ poly(lithography, 3) + poly(number_cores, 3) + poly(number_threads, 3) 
                  + poly(cache, 3) + poly(Pfrequency, 3) + 
                    poly(bandwidth, 3), data=cleaned_data)

summary(model_poly3)
coef(model_poly3)
pred3 <- predict(model_poly3, newdata = test)
MSE3 <- mean((test$power - pred3)^2)
model_poly4 <- lm(power ~ poly(lithography, 4) + poly(number_cores, 4) 
                  + poly(number_threads, 4) + poly(cache, 4) 
                  + poly(Pfrequency, 4) + poly(bandwidth, 4), data=cleaned_data)

summary(model_poly4)
pred4 <- predict(model_poly4, newdata = test)
pred_fi<- data.frame(predict(model_poly4, newdata = test))
compare_fi <- cbind(test$power,pred_fi)
colnames(compare_fi) <- c("test_set","prediction")
head(compare_fi,15)
plot(test$power, pred4, main="Scatter Plot with y=x Line", xlab="X", ylab="Y")
abline(a=0, b=1, col="red")
MSE4 <- mean((test$power - pred4)^2) 




coefficients_fi <- coef(model_poly4)
cat("power =", 
    coefficients_fi[1], "+",
    coefficients_fi[2], "*lithography +",
    coefficients_fi[3], "*lithography^2 +",
    coefficients_fi[4], "*lithography^3 \n+",
    coefficients_fi[5], "*lithography^4+",
    coefficients_fi[6], "*number_cores +",
    coefficients_fi[7], "*number_cores^2 \n+",
    coefficients_fi[8], "*number_cores^3 +",
    coefficients_fi[9], "*number_cores^4 +",
    coefficients_fi[10], "*number_threads \n+",
    coefficients_fi[11], "*number_threads^2 +",
    coefficients_fi[12], "*number_threads^3 +",
    coefficients_fi[13], "*number_threads^4 \n+",
    coefficients_fi[14], "*cache +",
    coefficients_fi[15], "*cache^2 +",
    coefficients_fi[16], "*cache^3 +",
    coefficients_fi[17], "*cache^4 \n+",
    coefficients_fi[18], "*Pfrequency +",
    coefficients_fi[19], "*Pfrequency^2 +",
    coefficients_fi[20], "*Pfrequency^3 \n+",
    coefficients_fi[21], "*Pfrequency^4 +",
    coefficients_fi[22], "*bandwidth +",
    coefficients_fi[23], "*bandwidth^2 \n+ ",
    coefficients_fi[24], "*bandwidth^3 + ",
    coefficients_fi[25], "*bandwidth^4")

