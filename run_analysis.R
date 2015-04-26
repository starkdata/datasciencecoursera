#last updated 4.26.15
require(plyr)

uci.path <- "UCI\ HAR\ Dataset"

#load and read files
  x.train.f <- paste(uci.path, "/train/X_train.txt", sep = "")
  x.test.f <- paste(uci.path, "/test/X_test.txt", sep = "")
  y.train.f <- paste(uci.path, "/train/y_train.txt", sep = "")
  y.test.f <- paste(uci.path, "/test/y_test.txt", sep = "")
  subject.train.f <- paste(uci.path, "/train/subject_train.txt", sep = "")
  subject.test.f <- paste(uci.path, "/test/subject_test.txt", sep = "")
  feature.f <- paste(uci.path, "/features.txt", sep = "")
  activity.labels.f <- paste(uci.path, "/activity_labels.txt", sep = "")

  x.train.d <- read.table(x.train.f)
  x.test.d <- read.table(x.test.f)
  y.train.d <- read.table(y.train.f)
  y.test.d <- read.table(y.test.f)
  subject.train.d <- read.table(subject.train.f)
  subject.test.d <- read.table(subject.test.f)
  features.d <- read.table(feature.f, colClasses = c("character"))
  activity.labels.d <- read.table(activity.labels.f, col.names = c("activityId", "activity"))

# Bind train and test data to create a single dataset
  training.sensor.merged <- cbind(cbind(x.train.d, subject.train.d), y.train.d)
  test.sensor.merged <- cbind(cbind(x.test.d, subject.test.d), y.test.d)
  sensor.data.merged <- rbind(training.sensor.merged, test.sensor.merged)

# Label sensor columns
  sensor.labels <- rbind(rbind(features, c(562, "subject")), c(563, "activityId"))[,2]
  names(sensor.data.merged) <- sensor.labels

#Get mean and standard deviation for each measure
  sens.mean.and.std <- sensor.data.merged[,grepl("mean|std|subject|activityId", names(sensor.data.merged))]

  sens.mean.and.std <- join(sens.mean.and.std, activity.labels.d, by = "activityId", match = "first")
  sens.mean.and.std <- sens.mean.and.std[,-1]

# Label the data
  names(sens.mean.and.std) <- gsub('GyroJerk',"AngAccel",names(sens.mean.and.std))
  names(sens.mean.and.std) <- gsub('Gyro',"Gyroscope",names(sens.mean.and.std))
  names(sens.mean.and.std) <- gsub('\\(|\\)',"",names(sens.mean.and.std), perl = TRUE)
  names(sens.mean.and.std) <- make.names(names(sens.mean.and.std))
  names(sens.mean.and.std) <- gsub('Acc',"Accel",names(sens.mean.and.std))
  names(sens.mean.and.std) <- gsub('Mag',"Magnitude",names(sens.mean.and.std))
  names(sens.mean.and.std) <- gsub('^t',"time",names(sens.mean.and.std))
  names(sens.mean.and.std) <- gsub('^f',"frequency",names(sens.mean.and.std))

#Create tidy dataset with average only of each activity
  sensor.avg = ddply(sens.mean.and.std, c("subject","activity"), numcolwise(mean))
  write.table(sensor.avg, file = "coursera.proj.tidy.data.txt", row.name = FALSE)
  print("file creation complete")
