
#R SCRIPT TO IMPLEMENT LINEAR REGRESSION ON WINE DATASET

#SET THE WORKING DIRECTORY FOR R
setwd("C:\\Users\\dell.hsk\\Desktop\\SGX_Repository\\KP-Tech\\R\\Data\\Practice Datasets")
getwd()

#WE WILL USE R DATASET FOR ANALYSIS
library("MASS")
Cars93Data <- Cars93
attach(Cars93Data)

#DATASET AT FIRST SIGHT - HOW DOES IT LOOK?
dim(Cars93Data)       #NUMBER OF ROWS/COLUMNS IN DATASET
str(Cars93Data)       #DATASET LAYOUT OR STRUCTURE
summary(Cars93Data)   #DATASET SUMMARY
head(Cars93Data)      #STARTING FEW RECORDS

#TREAT NA'S
Cars93Data.v1 <- na.omit(Cars93Data)
dim(Cars93Data.v1)

#LETS REMOVE VARIABLES WHICH DOESN'T HAVE DIRECT RELATION WITH CAR MILEAGE
Variables <- setdiff(colnames(Cars93Data.v1), c("MPG.highway", "Model", "AirBags", "DriveTrain", "Man.trans.avail", "Origin", "Make", "Luggage.room", "Rear.seat.room", "Turn.circle", "Manufacturer"))
Variables

#SPLIT QUANTITATIVE & CATEGORICAL VARIABLES IN TWO VECTORS
NumVars <- Variables[sapply(Cars93Data.v1[,Variables], class) %in% c("numeric", "integer")]
NumVars

CatVars <- Variables[sapply(Cars93Data.v1[,Variables], class) %in% c("factor")]
CatVars

#LETS ALSO SET THE ROW NAMES FOR OUR DATASET
rownames(Cars93Data.v1) <- Cars93Data.v1$Make
rownames(Cars93Data.v1)

#CORRELATION MATRIX FOR NUMERIC VARIABLES
round(cor(Cars93Data.v1[,NumVars]),3)

#VISUALIZATION OF DATA
attach(Cars93Data.v1) #ATTACH DATASET TO R SESSION

#HISTOGRAM ON MPG
hist(MPG.city, main = "Histogram for Gas Mileage", probability = T, xlim = c(15, 50), xlab = "MPG", ylim = c(0, 0.13), breaks = 12, las = 1, col = 4, cex.main = 1, cex.lab = 0.8, cex.axis = 0.8)
lines(density(MPG.city), col = 2, lwd = 2)
text(x = 40, y = 0.01, label = "Outliers?", adj = 0, col = 2)

#STACKED BAR CHART - CAR TYPE VS. ORIGIN
CatTab <- table(Origin, Type) #CONTIGENCY/FREQUENCY TABLE
barplot(CatTab, main = "CarType by Origin", cex.main = 1, ylim = c(0, 25), xlab = "Type of Car", cex.lab = 1, las = 1, legend.text = c("USA", "Non-USA"), col = c(4, 2))
box()

#CREATE NEW VARIABLE 'TYPE1', WITHOUT CATEGORY 'VAN'
Cars93Data.v1$Type1 <- factor(Cars93Data.v1$Type)
levels(Cars93Data.v1$Type1)

#SPLIT SCREEN AND PLOT TWO HISTOGRAMS
par(mfrow = c(1,2))
plot(EngineSize[Origin == "USA"], MPG.city[Origin == "USA"], main = "Scatterplot", xlab = "Engine Size", , xlim = c(0, 6), ylab = "MPG", ylim = c(15, 50), las = 1, col = 4, cex.lab = 0.9, cex.axis = 0.8, pch = "U", font.lab = 3)
points(EngineSize[Origin == "non-USA"], MPG.city[Origin == "non-USA"], col = 2, pch = "N")
legend(x = 4.5, y = 50, legend = c("USA", "Non-USA"), fill = c(4, 2), cex = 0.5, bty = "n")
plot(Horsepower[Origin == "USA"], MPG.city[Origin == "USA"], main = "Scatterplot", xlab = "Horse Power", ylab = "MPG", xlim = c(50, 300), ylim = c(15, 50), las = 1, col = 4, cex.lab = 0.9, cex.axis = 0.8, pch = "U", font.lab = 3)
points(Horsepower[Origin == "non-USA"], MPG.city[Origin == "non-USA"], col = 2, pch = "N")
legend(x = 250, y = 50, legend = c("USA", "Non-USA"), fill = c(4, 2), cex = 0.5, bty = "n")

#BOXPLOTS
boxplot(MPG.city ~ Cylinders, main = "MPG vs. Cylinders Boxplot", xlab = "Number of Cylinders", ylab = "MPG", las = 1, col = "purple", cex.main = 1, cex.lab = 0.9, cex.axis = 0.8, col.label = 2)
text(x = 2.1, y = 44, labels = "Outlier?", col = 2, font = 2)
text(x = 3, y = 22, labels = "1 Obs?", col = 2, font = 2)
text(x = 6.1, y = 17, labels = "No Data!", col = 2, font = 2)
stripchart(MPG.city ~ Cylinders, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = "red", cex = 0.7)

#ORGANIZE LEVELS FOR CYLINDERS
Cars93Data.v1$Cylinders <- factor(Cars93Data.v1$Cylinders)
summary(Cars93Data.v1$Cylinders)

#SPLIT THE DATA IN TRAINING SET AND TEST SET
Cars93Data.v1$Sampling <- runif(dim(Cars93Data.v1)[1])
TrainSet <- subset(Cars93Data.v1, Cars93Data.v1$Sampling > 0.1)
dim(TrainSet)
TestSet <- subset(Cars93Data.v1, Cars93Data.v1$Sampling < 0.1)
dim(TestSet)

#BUILD THE FORMULA
Predictors <- paste(c("EngineSize", "Horsepower", "Weight", "Origin", "Cylinders", "Fuel.tank.capacity"), collapse = "+")
Formula <- paste("MPG.city", Predictors, sep = "~")
Formula

#BUILD THE MODEL
Model <- lm(Formula, data = TrainSet)
Model
attributes(Model)
summary(Model)

#APPLYING BACKWARD SELECTION 
step(Model, direction = "backward")

#MAKE PREDICTIONS ON TEST SET
TestSet$MPG.PRED <- predict(Model, newdata = TestSet)
TestSet[,c("MPG.city", "MPG.PRED")]
