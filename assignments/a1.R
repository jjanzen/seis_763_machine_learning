print("Josh Janzen")
print("SEIS-763-02")
print("Assign #1")

getwd()
setwd("/Users/a149174/UST_GPS/seis_763/assignments")
library(jpeg)

# 1: read in image
parrots <- readJPEG("Parrots.jpg")

# 2: size of image
print(dim(parrots))

# 3: 1st plane maximum
first_plane <- 0.000
for (d in 1:length(parrots[,1,3])){
    if ((parrots[d,1,3]) > first_plane ){
        first_plane <- parrots[1,d,3]
    }
}
print(first_plane)

# 4: 2nd plane minimum
second_plane <- 1.000
for (d in 1:length(parrots[1,,3])){
    if ((parrots[1,d,3]) < second_plane ){
        second_plane <- parrots[1,d,3]
    }
}
print(second_plane)

# 5: 3rd plane greater than 0.5882
third_plane <- 0.5882
count_greater <- 0
for (d in 1:length(parrots[1,1,])){
    if ((parrots[1,1,d]) > third_plane ){
        count_greater = count_greater + 1
    }
}
print(count_greater)
