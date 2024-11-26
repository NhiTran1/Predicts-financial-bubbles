library(psymonitor)
library(readxl)

# Đọc dữ liệu từ file CSV
data <- read_excel("C:/Users/binhn/Downloads/ML.xlsx")# Kiểm tra và xử lý giá trị thiếu
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")  # Adjust the format as needed
data$log_Close <- log(data$Close)
y        <- data$log_Close
obs      <- length(y)
swindow0 <- floor(obs * (0.01 + 1.8 / sqrt(obs))) # set minimal window size
IC       <- 1  # use BIC to select the number of lags
adflag   <- 6  # set the maximum nuber of lags to 6
yr       <- 2  
Tb       <- 12*yr + swindow0 - 1  # Set the control sample size
nboot    <- 99  # set the number of replications for the bootstrap

bsadf          <- PSY(y, swindow0 = swindow0, IC = IC,
                      adflag = adflag)  # estimate the PSY test statistics sequence

quantilesBsadf <- cvPSYwmboot(y, swindow0 = swindow0, IC = IC,
                              adflag = adflag, Tb = Tb, nboot = 99,
                              nCores = 2) # simulate critical values via wild bootstrap. Note that the number of cores is arbitrarily set to 2.
dim          <- obs - swindow0 + 1 
monitorDates <- data$Date[swindow0:obs]
quantile95   <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)
ind95        <- (bsadf > t(quantile95[2, ])) * 1
print(ind95)
# Tìm các chỉ số có giá trị bằng 1
crisis_indices <- which(ind95 == 1)

# Liên kết các chỉ số này với các ngày trong monitorDates
crisis_dates <- monitorDates[crisis_indices]

# In ra các ngày khủng hoảng
print(crisis_dates)

