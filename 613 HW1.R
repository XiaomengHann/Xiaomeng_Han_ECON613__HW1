# exercise 1
# input data
# calculate the number of students
length(na.omit(datstu$V1)) #omit the NAs

# calculate the number of schools
schoolcodenum <- as.vector(as.matrix(datstu[2:340824,5:10])) # turn to the vector
length(unique(na.omit(schoolcodenum))) # omit the NAs and delete the replications, then calculate the length

# calculate the number of programs
pgm <- as.vector(as.matrix(datstu[2:340824,11:16])) # turn to the vector
length(unique(na.omit(pgm))) # omit the NAs and delete the replications, then calculate the length

# calculate the number of choices
schoolcode2 <- as.vector(as.matrix(datstu[2:340824,5:10]))
pgm2 <- as.vector(as.matrix(datstu[2:340824,11:16]))
combine <- cbind(schoolcode2,pgm2) # combine the two vectors
choice <- unique(combine) # delete the replications
choice[choice==""] = NA # save the empty data as NAs
choice <- na.omit(choice) # delete the NAs
nrow(choice) # calculate the number of rows

# calculate the number of missing test scores
sum(is.na(datstu$V2))

# apply to the same school
# repeat each student's number 6 times and turn to the vector1, also make the schoolcodes be vector2, combine the two vectors
sameschool <- cbind(rep(1:nrow(datstu), each=6), as.vector(as.matrix(datstu[2:340824,5:10]))) 
sameschool <- na.omit(sameschool)
sameschool <- unique(sameschool) # omit the NAs and delete the replications
sameschoolfactor <- factor(sameschool[,2]) # use function of factor to specify the application to the same school
sameschoolfactor2 <- as.data.frame(table(sameschoolfactor)) # table the number

# apply to less than 6 choices
# almost the same code as Q6
sameschool2 <- cbind(rep(1:nrow(datstu), each=6), as.vector(as.matrix(datstu[2:340824,5:10])))
sum(is.na(sameschool2)) # sum up and calculate the number of NAs

# exercise 2
datsss2 <- datsss[,2:6]
datsss2 <- datsss2[!duplicated(datsss2$schoolcode),] # delete the repeated rows based on schoolcode
# merge the dataframe of schoolcodes, programs, district locations. longitude and latitude of districts
colnames(choice)=c("schoolcode","program")
dataset1 <- merge(choice, datsss2, by.choice=schoolcode, by.datsss2=schoolcode, all.x=FALSE,sort=TRUE )
dataset1 <- unique(na.omit(dataset1))
# take out the data of test score, schoolcode and rankplace
testscoreall <- as.data.frame(datstu[2:340824, c(2, 5:16, 18)])
# get the data of scores of the admitted students
testscore1 <- subset(testscoreall, testscoreall[,14] == 1, select = c(V2,V5,V11))
testscore2 <- subset(testscoreall, testscoreall[,14] == 2, select = c(V2,V6,V12))
testscore3 <- subset(testscoreall, testscoreall[,14] == 3, select = c(V2,V7,V13))
testscore4 <- subset(testscoreall, testscoreall[,14] == 4, select = c(V2,V8,V14))
testscore5 <- subset(testscoreall, testscoreall[,14] == 5, select = c(V2,V9,V15))
testscore6 <- subset(testscoreall, testscoreall[,14] == 6, select = c(V2,V10,V16))
# rename variables and combine the data
colnames(testscore1)=c("testscore","schoolcode","program")
colnames(testscore2)=c("testscore","schoolcode","program")
colnames(testscore3)=c("testscore","schoolcode","program")
colnames(testscore4)=c("testscore","schoolcode","program")
colnames(testscore5)=c("testscore","schoolcode","program")
colnames(testscore6)=c("testscore","schoolcode","program")
lowscore <- rbind(testscore1, testscore2, testscore3, testscore4, testscore5, testscore6)
# change the type of variables to factor
program<-as.factor(lowscore$program)
# calculate the lowest score and the average of scores, rename the dataset
minscore <- aggregate.data.frame(x = testscore, by = list(schoolcode, program), FUN = "min")
meanscore <- aggregate.data.frame(x = testscore, by = list(schoolcode, program), FUN = "mean")
colnames(minscore)=c("schoolcode","program","lowestscore")
colnames(meanscore)=c("schoolcode","program","averagescore")
# calculate the number of admitted students
sameschooladmit <- as.data.frame(table(lowscore[,2:3]))
colnames(sameschooladmit)=c("schoolcode","program","frequency")
sameschooladmit <- unique(na.omit(sameschooladmit))
# merge all the data
dataset1 <- merge(dataset1, minscore, by.dataset1=c(1,2), by.minscore=c(1,2), all.x=FALSE,sort=TRUE )
dataset1 <- merge(dataset1, meanscore,  by.dataset1=c(1,2), by.meanscore=c(1,2), all.x=FALSE,sort=TRUE )
dataset1 <- merge(dataset1, sameschooladmit, by.dataset1=c(1,2), by.sameschooladmit=c(1,2), all=FALSE,sort=TRUE )
dataset1 <- unique(na.omit(dataset1))

# exercise 3
# turn out the data of the coordinates of the district of the school (students apply to), the coordinates of the junior high school and longitude and latitude of them
jdistrict <- unique(datjss[,2:4])
sdistrict <- unique(datsss[,4:6])
# make a dataset of the district choices
jdistrict2 <- unique(na.omit(as.vector(datjss[,2])))
sdistrict2 <- unique(na.omit(as.vector(datsss2[,3])))
alldistrict <- cbind(rep(jdistrict2, each=139), sdistrict2)
colnames(alldistrict)=c("jssdistrict","sssdistrict")
# merge all the data
alldistrict <- merge(alldistrict, jdistrict,  by=c("jssdistrict"), all.x=TRUE,sort=TRUE )
alldistrict <- merge(alldistrict, sdistrict,  by=c("sssdistrict"), all.x=TRUE,sort=TRUE )
alldistrict <- na.omit(alldistrict)
# calculate the distances
distance <- as.vector(sqrt((69.172*(alldistrict$ssslong-alldistrict$point_x)*cos(alldistrict$point_y/57.3))^2+(69.172*(alldistrict$ssslat-alldistrict$point_y)^2)))
alldistrict <- cbind(alldistrict, distance)
colnames(alldistrict)=c("sssdistrict","jssdistrict","jsslong","jsslat","ssslong","ssslat","distance")

# exercise 4
# Report the average and sd of the variables for each ranked choice
# take out the data of the student number, test score, schoolcode and program according to students' rankplaces, these are the students who are admitted
testscore11 <- as.data.frame(datstu[2:340824,c(1,2,5,11,17)])
testscore11[testscore11==""] = NA
testscore11 <- na.omit(testscore11)                             
testscore22 <- as.data.frame(datstu[2:340824,c(1,2,6,12,17)])
testscore22[testscore22==""] = NA
testscore22 <- na.omit(testscore22)
testscore33 <- as.data.frame(datstu[2:340824,c(1,2,7,13,17)])
testscore33[testscore33==""] = NA
testscore33 <- na.omit(testscore33)
testscore44 <- as.data.frame(datstu[2:340824,c(1,2,8,14,17)])
testscore44[testscore44==""] = NA
testscore44 <- na.omit(testscore44)
testscore55 <- as.data.frame(datstu[2:340824,c(1,2,9,15,17)])
testscore55[testscore55==""] = NA
testscore55 <- na.omit(testscore55)
testscore66 <- as.data.frame(datstu[2:340824,c(1,2,10,16,17)])
testscore66[testscore66==""] = NA
testscore66 <- na.omit(testscore66)
# rename the names of variables
colnames(testscore11)=c("x","score","schoolcode","program","jssdistrict")
colnames(testscore22)=c("x","score","schoolcode","program","jssdistrict")
colnames(testscore33)=c("x","score","schoolcode","program","jssdistrict")
colnames(testscore44)=c("x","score","schoolcode","program","jssdistrict")
colnames(testscore55)=c("x","score","schoolcode","program","jssdistrict")
colnames(testscore66)=c("x","score","schoolcode","program","jssdistrict")
datsss3 <- unique(as.data.frame(datsss[,3:4]))
# merge the basic dataset with the data of sssdistrict
testscore11 <- merge(testscore11, datsss3, by.testscore11=c(3), by.datsss3=c(1), all.x=TRUE,sort=TRUE )
# merge the dataset with the data of min score
testscore11 <- merge(testscore11, minscore, by.testscore11=c(1,4), by.minscore=c(1,2), all.x=TRUE,sort=TRUE )
# merge the dataset with the data of mean score
testscore11 <- merge(testscore11, meanscore, by.testscore11=c(1,2), by.meanscore=c(1,2), all.x=TRUE,sort=TRUE )
# merge the dataset with the data of distance
testscore11 <- merge(testscore11, alldistrict, by.testscore11=c(1,2), by.alldistrict=c(5,6), all.x=TRUE,sort=TRUE )
testscore11 <- na.omit(testscore11)
# calculate and print the mean and sd of these three variables
cutoffmean11 <- mean(as.vector(testscore11$lowestscore))
cutoffsd11 <- sd(as.vector(testscore11$lowestscore))
qualitymean11 <-mean(as.vector(testscore11$averagescore))
qualitysd11 <- sd(as.vector(testscore11$averagescore))
distancemean11 <- mean(as.vector(testscore11$distance))
distancesd11 <- sd(as.vector(testscore11$distance))
print(cutoffmean11)
print(cutoffsd11)
print(qualitymean11)
print(qualitysd11)
print(distancemean11)
print(distancesd11)
# turn the results into two vectors
rank1 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result1 <- as.vector(c("318.7323","52.42778","339.3761","47.33106","20.87923","28.53967"))
# repeat the process above 6 times
testscore22 <- merge(testscore22, datsss3, by.testscore22=c(3), by.datsss3=c(1), all.x=TRUE,sort=TRUE )
testscore22 <- merge(testscore22, minscore, by.testscore22=c(1,4), by.minscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore22 <- merge(testscore22, meanscore, by.testscore22=c(1,2), by.meanscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore22 <- merge(testscore22, alldistrict, by.testscore22=c(5,6), by.alldistrict=c(1,2), all.x=TRUE,sort=TRUE )
testscore22 <- na.omit(testscore22)
cutoffmean22 <- mean(as.vector(testscore22$lowestscore))
cutoffsd22 <- sd(as.vector(testscore22$lowestscore))
qualitymean22 <-mean(as.vector(testscore22$averagescore))
qualitysd22 <- sd(as.vector(testscore22$averagescore))
distancemean22 <- mean(as.vector(testscore22$distance))
distancesd22 <- sd(as.vector(testscore22$distance))
print(cutoffmean22)
print(cutoffsd22)
print(qualitymean22)
print(qualitysd22)
print(distancemean22)
print(distancesd22)
rank2 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result2 <- as.vector(c("301.0319","48.99849","322.5332","43.31476","19.96643","27.21018"))
testscore33 <- merge(testscore33, datsss3, by.testscore33=c(3), by.datsss3=c(1), all.x=TRUE,sort=TRUE )
testscore33 <- merge(testscore33, minscore, by.testscore33=c(1,4), by.minscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore33 <- merge(testscore33, meanscore, by.testscore33=c(1,2), by.meanscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore33 <- merge(testscore33, alldistrict, by.testscore33=c(5,6), by.alldistrict=c(1,2), all.x=TRUE,sort=TRUE )
testscore33 <- na.omit(testscore33)
cutoffmean33 <- mean(as.vector(testscore33$lowestscore))
cutoffsd33 <- sd(as.vector(testscore33$lowestscore))
qualitymean33 <-mean(as.vector(testscore33$averagescore))
qualitysd33 <- sd(as.vector(testscore33$averagescore))
distancemean33 <- mean(as.vector(testscore33$distance))
distancesd33 <- sd(as.vector(testscore33$distance))
print(cutoffmean33)
print(cutoffsd33)
print(qualitymean33)
print(qualitysd33)
print(distancemean33)
print(distancesd33)
rank3 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result3 <- as.vector(c("288.5268","47.1945","311.2268","41.14239","18.57147","25.69437"))
testscore44 <- merge(testscore44, datsss3, by.testscore44=c(3), by.datsss3=c(1), all.x=TRUE,sort=TRUE )
testscore44 <- merge(testscore44, minscore, by.testscore44=c(1,4), by.minscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore44 <- merge(testscore44, meanscore, by.testscore44=c(1,2), by.meanscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore44 <- merge(testscore44, alldistrict, by.testscore44=c(5,6), by.alldistrict=c(1,2), all.x=TRUE,sort=TRUE )
testscore44 <- na.omit(testscore44)
cutoffmean44 <- mean(as.vector(testscore44$lowestscore))
cutoffsd44 <- sd(as.vector(testscore44$lowestscore))
qualitymean44 <-mean(as.vector(testscore44$averagescore))
qualitysd44 <- sd(as.vector(testscore44$averagescore))
distancemean44 <- mean(as.vector(testscore44$distance))
distancesd44 <- sd(as.vector(testscore44$distance))
print(cutoffmean44)
print(cutoffsd44)
print(qualitymean44)
print(qualitysd44)
print(distancemean44)
print(distancesd44)
rank4 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result4 <- as.vector(c("275.1899","45.96456","299.6867","39.62323","16.08562","24.3666"))
testscore55 <- merge(testscore55, datsss3, by.testscore55=c(3), by.datsss3=c(1), all.x=TRUE,sort=TRUE )
testscore55 <- merge(testscore55, minscore, by.testscore55=c(1,4), by.minscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore55 <- merge(testscore55, meanscore, by.testscore55=c(1,2), by.meanscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore55 <- merge(testscore55, alldistrict, by.testscore55=c(5,6), by.alldistrict=c(1,2), all.x=TRUE,sort=TRUE )
testscore55 <- na.omit(testscore55)
cutoffmean55 <- mean(as.vector(testscore55$lowestscore))
cutoffsd55 <- sd(as.vector(testscore55$lowestscore))
qualitymean55 <-mean(as.vector(testscore55$averagescore))
qualitysd55 <- sd(as.vector(testscore55$averagescore))
distancemean55 <- mean(as.vector(testscore55$distance))
distancesd55 <- sd(as.vector(testscore55$distance))
print(cutoffmean55)
print(cutoffsd55)
print(qualitymean55)
print(qualitysd55)
print(distancemean55)
print(distancesd55)
rank5 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result5 <- as.vector(c("256.6208","32.13175","284.3778","25.88133","19.68102","20.77518"))
testscore66 <- merge(testscore66, datsss3, by.testscore66=c(3), by.datsss3=c(1), all.x=TRUE,sort=TRUE )
testscore66 <- merge(testscore66, minscore, by.testscore66=c(1,4), by.minscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore66 <- merge(testscore66, meanscore, by.testscore66=c(1,2), by.meanscore=c(1,2), all.x=TRUE,sort=TRUE )
testscore66 <- merge(testscore66, alldistrict, by.testscore66=c(5,6), by.alldistrict=c(1,2), all.x=TRUE,sort=TRUE )
testscore66 <- na.omit(testscore66)
cutoffmean66 <- mean(as.vector(testscore66$lowestscore))
cutoffsd66 <- sd(as.vector(testscore66$lowestscore))
qualitymean66 <-mean(as.vector(testscore66$averagescore))
qualitysd66 <- sd(as.vector(testscore66$averagescore))
distancemean66 <- mean(as.vector(testscore66$distance))
distancesd66 <- sd(as.vector(testscore66$distance))
print(cutoffmean66)
print(cutoffsd66)
print(qualitymean66)
print(qualitysd66)
print(distancemean66)
print(distancesd66)
rank6 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result6 <- as.vector(c("251.5665","31.83102","279.8642","25.75431","19.80237","20.61514"))
# report the whole data of 6 results
Report <- as.data.frame(cbind(rank1,result1,rank2,result2,rank3,result3,rank4,result4,rank5,result5,rank6,result6))


# Redo the same table, differentiating by student test score quantiles
tall <- as.data.frame(datstu[2:340824, c(1:2, 5:16, 17:18)])
# get the data of scores of the admitted students
t1 <- subset(tall, tall[,16] == 1, select = c(V1,V2,V5,V11,V17))
t2 <- subset(tall, tall[,16] == 2, select = c(V1,V2,V6,V12,V17))
t3 <- subset(tall, tall[,16] == 3, select = c(V1,V2,V7,V13,V17))
t4 <- subset(tall, tall[,16] == 4, select = c(V1,V2,V8,V14,V17))
t5 <- subset(tall, tall[,16] == 5, select = c(V1,V2,V9,V15,V17))
t6 <- subset(tall, tall[,16] == 6, select = c(V1,V2,V10,V16,V17))
# rename variables and combine the data
colnames(t1)=c("X","testscore","schoolcode","program","jssdistrict")
colnames(t2)=c("X","testscore","schoolcode","program","jssdistrict")
colnames(t3)=c("X","testscore","schoolcode","program","jssdistrict")
colnames(t4)=c("X","testscore","schoolcode","program","jssdistrict")
colnames(t5)=c("X","testscore","schoolcode","program","jssdistrict")
colnames(t6)=c("X","testscore","schoolcode","program","jssdistrict")
lscore <- rbind(t1, t2, t3, t4, t5, t6)
# merge the dataset with the data of sssdistrict, jssdistrict, min score and mean score
lscore <- merge(lscore, dataset1, by=c("schoolcode","program"), all.x=TRUE,sort=TRUE )
# merge the dataset with the data of distance
lscore <- merge(lscore, alldistrict, by=c("sssdistrict","jssdistrict"), all.x=TRUE,sort=TRUE )
# take out the necessary data
lscore <- lscore[,c(1:6,10,11,17)]
# order the data according to test score
lscore <- lscore[order(lscore$testscore),]
# seperate the data into 4 quantiles
q_1 <- na.omit(as.data.frame(lscore[1:34806,]))
q_2 <- na.omit(as.data.frame(lscore[34807:69612,]))
q_3 <- na.omit(as.data.frame(lscore[69613:104418,]))
q_4 <- na.omit(as.data.frame(lscore[104419:139224,]))
# calculate and print the mean and sd of the 3 variables
cutoffmean_q1 <- mean(as.vector(q_1$lowestscore))
cutoffsd_q1 <- sd(as.vector(q_1$lowestscore))
qualitymean_q1 <-mean(as.vector(q_1$averagescore))
qualitysd_q1 <- sd(as.vector(q_1$averagescore))
distancemean_q1 <- mean(as.vector(q_1$distance))
distancesd_q1 <- sd(as.vector(q_1$distance))
print(cutoffmean_q1)
print(cutoffsd_q1)
print(qualitymean_q1)
print(qualitysd_q1)
print(distancemean_q1)
print(distancesd_q1)
# turn the results as two vectors
rank11 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result11 <- as.vector(c("217.2587","14.70495","251.94","12.73083","14.60612","22.96826"))
# repeat the process above 4 times
cutoffmean_q2 <- mean(as.vector(q_2$lowestscore))
cutoffsd_q2 <- sd(as.vector(q_2$lowestscore))
qualitymean_q2 <-mean(as.vector(q_2$averagescore))
qualitysd_q2 <- sd(as.vector(q_2$averagescore))
distancemean_q2 <- mean(as.vector(q_2$distance))
distancesd_q2 <- sd(as.vector(q_2$distance))
print(cutoffmean_q2)
print(cutoffsd_q2)
print(qualitymean_q2)
print(qualitysd_q2)
print(distancemean_q2)
print(distancesd_q2)
rank22 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result22 <- as.vector(c("243.0132","23.7312","273.0892","17.69718","16.32984","25.29695"))
cutoffmean_q3 <- mean(as.vector(q_3$lowestscore))
cutoffsd_q3 <- sd(as.vector(q_3$lowestscore))
qualitymean_q3 <-mean(as.vector(q_3$averagescore))
qualitysd_q3 <- sd(as.vector(q_3$averagescore))
distancemean_q3 <- mean(as.vector(q_3$distance))
distancesd_q3 <- sd(as.vector(q_3$distance))
print(cutoffmean_q3)
print(cutoffsd_q3)
print(qualitymean_q3)
print(qualitysd_q3)
print(distancemean_q3)
print(distancesd_q3)
rank33 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result33 <- as.vector(c("276.7182","28.80193","302.3892","22.7742","18.07317","25.49426"))
cutoffmean_q4 <- mean(as.vector(q_4$lowestscore))
cutoffsd_q4 <- sd(as.vector(q_4$lowestscore))
qualitymean_q4 <-mean(as.vector(q_4$averagescore))
qualitysd_q4 <- sd(as.vector(q_4$averagescore))
distancemean_q4 <- mean(as.vector(q_4$distance))
distancesd_q4 <- sd(as.vector(q_4$distance))
print(cutoffmean_q4)
print(cutoffsd_q4)
print(qualitymean_q4)
print(qualitysd_q4)
print(distancemean_q4)
print(distancesd_q4)
rank44 <- as.vector(c("cutoff_mean","cutoff_sd","quality_mean","quality_sd","distance_mean","distance_sd"))
result44 <- as.vector(c("338.2554","39.39301","358.2653","35.32207","23.91324","29.9198"))
# report the whole data of 4 results
Report2 <- as.data.frame(cbind(rank11,result11,rank22,result22,rank33,result33,rank44,result44))


# exercise 5
# rank the dataset in exercise 2 by cutoff
cutoff_rank <- dataset1[order(dataset1$lowestscore),]
# seperate the dataset into 10 deciles
c_1 <- as.data.frame(cutoff_rank[1:230,])
c_2 <- as.data.frame(cutoff_rank[231:460,])
c_3 <- as.data.frame(cutoff_rank[461:690,])
c_4 <- as.data.frame(cutoff_rank[691:920,])
c_5 <- as.data.frame(cutoff_rank[921:1150,])
c_6 <- as.data.frame(cutoff_rank[1151:1380,])
c_7 <- as.data.frame(cutoff_rank[1381:1610,])
c_8 <- as.data.frame(cutoff_rank[1611:1840,])
c_9 <- as.data.frame(cutoff_rank[1841:2070,])
c_10 <- as.data.frame(cutoff_rank[2071:2300,])
# match the program number of these 10 datasets
program_num1 <- data.frame(program_num=1)
program_num1 <- as.vector(program_num1[rep(1:nrow(program_num1), each=230),])
c_1 <- cbind(c_1, program_num1)
program_num2 <- data.frame(program_num=2)
program_num2 <- as.vector(program_num2[rep(1:nrow(program_num2), each=230),])
c_2 <- cbind(c_2, program_num2)
program_num3 <- data.frame(program_num=3)
program_num3 <- as.vector(program_num3[rep(1:nrow(program_num3), each=230),])
c_3 <- cbind(c_3, program_num3)
program_num4 <- data.frame(program_num=4)
program_num4 <- as.vector(program_num4[rep(1:nrow(program_num4), each=230),])
c_4 <- cbind(c_4, program_num4)
program_num5 <- data.frame(program_num=5)
program_num5 <- as.vector(program_num5[rep(1:nrow(program_num5), each=230),])
c_5 <- cbind(c_5, program_num5)
program_num6 <- data.frame(program_num=6)
program_num6 <- as.vector(program_num6[rep(1:nrow(program_num6), each=230),])
c_6 <- cbind(c_6, program_num6)
program_num7 <- data.frame(program_num=7)
program_num7 <- as.vector(program_num7[rep(1:nrow(program_num7), each=230),])
c_7 <- cbind(c_7, program_num7)
program_num8 <- data.frame(program_num=8)
program_num8 <- as.vector(program_num8[rep(1:nrow(program_num8), each=230),])
c_8 <- cbind(c_8, program_num8)
program_num9 <- data.frame(program_num=9)
program_num9 <- as.vector(program_num9[rep(1:nrow(program_num9), each=230),])
c_9 <- cbind(c_9, program_num9)
program_num10 <- data.frame(program_num=10)
program_num10 <- as.vector(program_num10[rep(1:nrow(program_num10), each=230),])
c_10 <- cbind(c_10, program_num10)
# rename these 10 datasets
colnames(c_1)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_2)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_3)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_4)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_5)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_6)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_7)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_8)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_9)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(c_10)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
# combine these 10 datasets
c_all <- rbind(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9,c_10)
# take out the data of admitted students from datstu, omit the empty data
p_1 <- as.data.frame(datstu[2:340824,c(1,5,11)])
p_1[p_1==""] = NA
p_1 <- na.omit(p_1)                             
p_2 <- as.data.frame(datstu[2:340824,c(1,6,12)])
p_2[p_2==""] = NA
p_2 <- na.omit(p_2)
p_3 <- as.data.frame(datstu[2:340824,c(1,7,13)])
p_3[p_3==""] = NA
p_3 <- na.omit(p_3)
p_4 <- as.data.frame(datstu[2:340824,c(1,8,14)])
p_4[p_4==""] = NA
p_4 <- na.omit(p_4)
p_5 <- as.data.frame(datstu[2:340824,c(1,9,15)])
p_5[p_5==""] = NA
p_5 <- na.omit(p_5)
p_6 <- as.data.frame(datstu[2:340824,c(1,10,16)])
p_6[p_6==""] = NA
p_6 <- na.omit(p_6)
# rename these 6 datasets
colnames(p_1)=c("student","schoolcode","program")
colnames(p_2)=c("student","schoolcode","program")
colnames(p_3)=c("student","schoolcode","program")
colnames(p_4)=c("student","schoolcode","program")
colnames(p_5)=c("student","schoolcode","program")
colnames(p_6)=c("student","schoolcode","program")
# combine these 6 datasets and rank them by students' ids
sameschool3 <- rbind(p_1,p_2,p_3,p_4,p_5,p_6)
sameschool3 <- sameschool3[order(sameschool3$student),]
sameschool4 <- merge(sameschool3, c_all, by=c("schoolcode","program"), all.x=TRUE,sort=TRUE )
# table the frequency of the number of applications for each students
application_num <- as.data.frame(table(sameschool4[,3]))
colnames(application_num)=c("student","application_num")


# redo by test scores(quantile), repeat the same process
quality_rank <- dataset1[order(dataset1$averagescore),]
qua_1 <- as.data.frame(quality_rank[1:575,])
qua_2 <- as.data.frame(quality_rank[576:1150,])
qua_3 <- as.data.frame(quality_rank[1151:1725,])
qua_4 <- as.data.frame(quality_rank[1726:2300,])
program2_num1 <- data.frame(program_num=1)
program2_num1 <- as.vector(program2_num1[rep(1:nrow(program2_num1), each=575),])
qua_1 <- cbind(qua_1, program2_num1)
program2_num2 <- data.frame(program_num=2)
program2_num2 <- as.vector(program2_num2[rep(1:nrow(program2_num2), each=575),])
qua_2 <- cbind(qua_2, program2_num2)
program2_num3 <- data.frame(program_num=3)
program2_num3 <- as.vector(program2_num3[rep(1:nrow(program2_num3), each=575),])
qua_3 <- cbind(qua_3, program2_num3)
program2_num4 <- data.frame(program_num=4)
program2_num4 <- as.vector(program2_num4[rep(1:nrow(program2_num4), each=575),])
qua_4 <- cbind(qua_4, program2_num4)
colnames(qua_1)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(qua_2)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(qua_3)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
colnames(qua_4)=c("schoolcode","program","schoolname","sssdistrict","ssslong","ssslat","lowestscore","averagescore","frequency","program_num")
qua_all <- rbind(qua_1,qua_2,qua_3,qua_4)
sameschool5 <- merge(sameschool3, qua_all, by=c("schoolcode","program"), all.x=TRUE,sort=TRUE )
application_num2 <- as.data.frame(table(sameschool5[,3]))
colnames(application_num2)=c("student","application_num")

write.csv(choice, file = "choice.csv")
write.csv(sameschoolfactor2, file = "sameschoolfactor2")
write.csv(lowscore, file = "lowscore.csv")
write.csv(minscore, file = "minscore.csv")
write.csv(meanscore, file = "meanscore.csv")
write.csv(dataset1, file = "dataset1.csv")
write.csv(alldistrict, file = "alldistrict.csv")
write.csv(Report, file = "Report.csv")
write.csv(Report2, file = "Report2.csv")
write.csv(sameschool4, file = "sameschool4.csv")
write.csv(application_num, file = "application_num.csv")
write.csv(sameschool5, file = "sameschool5.csv")
write.csv(application_num2, file = "application_num2.csv")