#https://github.com/twitter/AnomalyDetection
library(sqldf)
## https://github.com/pablo14/anomaly_detection_post/blob/master/anomaly_detection.R
library(ggplot2)
#install.packages("devtools")
#devtools::install_github("petermeissner/wikipediatrend")
#devtools::install_github("twitter/AnomalyDetection")
#install.packages("Rcpp")
library(AnomalyDetection)

list.files()
setwd("/Users/a149174/UST_GPS/seis_763/r/seis_763_machine_learning/anomaly_detection/ydata-labeled-time-series-anomalies-v1_0/A2Benchmark")

# adding up files into one df
data_a2benchmark <- data.frame()
for (f in list.files()){
    data <- read.csv(f, head=F, sep=',', skip = 1)
    #?read.csv
    data_a2benchmark <- rbind(data_a2benchmark, data)
}
col_headings <- c('ts', 'value', 'anomaly')
names(data_a2benchmark) <- col_headings

head(data_a2benchmark)
dim(data_a2benchmark)
summary(data_a2benchmark)

## Convert date variable
## http://stackoverflow.com/questions/27408131/convert-unix-timestamp-into-datetime-in-r
data_a2benchmark$ts <- as.POSIXlt(as.numeric(as.character(data_a2benchmark$ts)),origin="1970-01-01",tz="GMT")
head(data_a2benchmark)
## Plotting benchmark data
ggplot(data_a2benchmark, aes(x=ts, y=value, color=value)) + geom_line()

## Keep only desiered variables (date & page views) for modeling
data_trimmed=data_a2benchmark[,c(1,2)]
unique(data_trimmed$ts)

#help("AnomalyDetectionTs")
# % of all outcomes which are "Nothing Found".  To determine if adding value

# subsetting
# data_trimmed <- subset(data_trimmed, value > 0)
vec <- AnomalyDetectionVec(data_trimmed[,2], max_anoms=0.02, period=14200, direction='both', only_last=FALSE, plot=TRUE)
vec$plot
vec$anoms
#help("AnomalyDetectionVec")
## Calculate deviation percentage from the expected value 
#vec$anoms$perc_diff=round(100*(vec$anoms$expected_value-vec$anoms$anoms)/vec$anoms$expected_value)

## anomalies table
anomaly_table=vec$anoms
names(anomaly_table) <- c("table_index", "anoms")
head(anomaly_table)
tail(anomaly_table)

# subsetting by variables
benchmark_data <- data_a2benchmark[c('value', 'anomaly')]
benchmark_data$col_id <- seq.int(nrow(benchmark_data))
tail(benchmark_data,100)

nrow(benchmark_data)
# 142100

# anomalies in training data
training_anomalies <- sqldf(paste("select count(b.col_id) from benchmark_data b where anomaly == 1"))
training_anomalies

# non anomailies in training data
training_non_anomalies <- sqldf(paste("select count(b.col_id) from benchmark_data b where anomaly == 0"))
training_non_anomalies

# anomalies predicted in model
predicted_anomalies <- sqldf(paste("select count(table_index) from anomaly_table"))
predicted_anomalies
# anomalies matched with model
tp <- sqldf(paste("select count(b.col_id) from anomaly_table a join benchmark_data b on a.table_index=b.col_id where anomaly == 1"))
tp
tp_rate <- tp/training_anomalies
tp_rate

# not detected anomailes
fn <- training_anomalies - tp
fn
fn_rate <- fn/training_anomalies
fn_rate

fp <- (predicted_anomalies - tp)
fp
fp_rate <- fp/training_non_anomalies
fp_rate

tn_rate <- 1-fp_rate
tn <- tn_rate * training_non_anomalies
tn

balanced_accurancy <- (tp_rate + tn_rate)/2
balanced_accurancy

prevelance <- training_anomalies/nrow(benchmark_data)
prevelance















########################### end - below is notes on github example code ###########

#res = AnomalyDetectionTs(raw_data[1:100,], max_anoms=0.02, direction='both', plot=TRUE)
#res$plot


class(testing_outcome[,2])
class(raw_data[,2])


head(data_trimmed[1:10,1:2])
summary(data_trimmed)
class(data_trimmed$value)
class(data_trimmed$ts)
class(raw_data$count)
class(raw_data$timestamp)
dim(data_trimmed)
dim(raw_data)
#AnomalyDetectionVec(data_trimmed[,2], max_anoms=0.02, period=1440, direction='both', only_last=FALSE, plot=TRUE)
## Apply anomaly detection
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot

test_data <- data_trimmed[1:5,1:2]
test_data[1,]$ts <- "2014-11-23 07:01:00"
test_data[2,]$ts <- "2014-11-23 08:01:00"
test_data[3,]$ts <- "2014-11-23 09:01:00"
test_data[4,]$ts <- "2014-11-23 10:01:00"
test_data[5,]$ts <- "2014-11-23 11:01:00"
test_data[1:5,]
test_data <- raw_data[1:3500,]
data_anomaly = AnomalyDetectionTs(test_data, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
data_anomaly$plot

jpeg("03_fifa_wikipedia_term_page_views_anomaly_detection.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
## Plot original data + anomalies points
data_anomaly$plot
dev.off()


## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms


data(raw_data)
head(raw_data)
raw_data[1:10,]
class(raw_data)
dim(raw_data)
unique(raw_data$timestamp)
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot
