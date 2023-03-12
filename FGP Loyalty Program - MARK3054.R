## MARK3054 Final Report Code

# Import Packages 
if (!"pscl" %in% installed.packages()) install.packages("pscl")
library(pscl)
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
library(ggplot2)
if (!"psych" %in% installed.packages()) install.packages("psych")
library(psych)
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
library(tidyverse)
if (!"scales" %in% installed.packages()) install.packages("scales")
library(scales)
if (!"eeptools" %in% installed.packages()) install.packages("eeptools")
library(eeptools)
if (!"zoo" %in% installed.packages()) install.packages("zoo")
library(zoo)
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")
library(plotrix)

# Import Data Sets
purchase <- read.csv("FGP Customer Loyalty Program_RData_purchase.csv",
                     head=TRUE, sep=",")
customer <- read.csv("FGP Customer Loyalty Program_RData_customer.csv",
                     head=TRUE, sep=",")
redeem <- read.csv("FGP Customer Loyalty Program_RData_redeem.csv",
                   head=TRUE, sep=",")

# Create a column called 'Age' in numbers for customer data
AgeData <- data.frame(as.numeric(as.character(customer$BirthYear)))
colnames(AgeData) <- c("Age")
AgeData <- na.aggregate(AgeData)

for (i in 1:AgeData$Age) {
  AgeData$Age[i] = 2021 - AgeData$Age[i]
}
customer <- cbind(customer, AgeData$Age)

# Create Merged Data Sets
redeem.merged <- merge(customer, redeem)
customer.merged <- merge(customer, purchase)


# ANALYZE SALES DATA

# Descriptive Statistics

# Plot the Total Sales by Merchant Member
SalesTotal.Plt <- purchase %>% group_by(SalesFirm) %>% summarise(TotalPoints = sum(SalesAmt)) 
SalesTotal.Plt

SalesPlot <- SalesTotal.Plt %>% ggplot(aes(x=SalesFirm, y=TotalPoints)) + geom_col(aes(fill=SalesFirm)) 
SalesPlot <- SalesPlot + scale_y_continuous(labels=comma)
print(SalesPlot + labs(title="Comparing the Total Sales of each Sales Firm",
                          y="Total Sales", x = "Sales Firm"))

# Plotting the Average Sales per Transaction by Firm
SalesAvg.Plt <- purchase %>% group_by(SalesFirm) %>% summarise(AvgSales = mean(SalesAmt)) 
SalesAvg.Plt

SalesAvgPlot <- SalesAvg.Plt %>% ggplot(aes(x=SalesFirm, y=TotalPoints)) + geom_col(aes(fill=SalesFirm)) 
SalesAvgPlot <- SalesAvgPlot + scale_y_continuous(labels=comma)
print(SalesAvgPlot + labs(title="Comparing the Average Transaction Amount of each Sales Firm",
                       y="Total Sales", x = "Sales Firm"))


# What factors influence Sales? What factors can increase the average transaction spend? ie. increase sales, or get consumers to spend more
Sales.lm <- lm(SalesAmt ~ factor(OwnCar) + factor(OwnCreditCard) + factor(HomeCity) + factor(Race) + Sat_Program + NetPromoter + Sat_Petrol + SalesFirm + Sat_FastFood + Sat_Grocery + (Sat_Grocery * Sat_Grocery), data= customer.merged)
summary(Sales.lm)

# From this regression output, we see that when we control 4 factors such as car ownership, Credit Card Ownership
# Race and HomeCity, Sat_Program and NetPromoter have a positive and significant impact. That is, for a unit increase 
# in Sat_Program, the average spend will increase by $0.81, holding all other factors constant and a unit increase in 
# NetPromoter will result in a $0.35 increase in average transaction, holding all other factors constant. Thus, we recognise,
# that to increase average spend, we must improve customer satisfaction. All Factors in the model are significant at the 5% level,
# except for Credit Card Ownership, which has been left in the model as a control. The Adjusted R-Aquared of the model is 0.17,
# which signifies that 17.29% of the variation in Sales Amount is accounted for by the model. This is quite low, which signifIes that there
# may be external factors which have not been included in the model or that have not been included in the data set which contributes
# to the variance in Sales Amount.

# From the above regression, we see that the satisfaction of the loyalty program is significant and influential in influencing sales.
# Hence, it is significant for firms to look to improve customer satisfaction. First, lets have a look at the customer satisfaction 
# information that has been provided in the data set.

# Chart of Customer Satisfaction
mean(customer$Sat_Program)
mean(customer$Sat_FastFood)
mean(customer$Sat_Petrol)
mean(customer$Sat_Grocery)

# Create dataframe -> values come from means found above
combinedSat.data <- read.table(text="
Firm, AvgSat
Program,7.262657
Petrol,8.078195
FastFood,7.195489
Grocery, 6.7599", header=TRUE, sep=",")

combinedSat.data

# Create Plot
SatPlot <- combinedSat.data %>% ggplot(aes(x=Firm, y=AvgSat)) + geom_col(aes(fill=Firm)) 
print(SatPlot + labs(title="Comparing the Average Customer Satisfaction of Each Merchant",
                     y="Customer Satisfaction", x = "Firm"))

# NPS Calculations -> slicing data to find detractor, promoters and passives
detractor <- count(customer[which(customer$NetPromoter == 0),]) + count(customer[which(customer$NetPromoter == 1),]) + count(customer[which(customer$NetPromoter == 2),]) + count(customer[which(customer$NetPromoter == 3),]) +
  count(customer[which(customer$NetPromoter == 4),]) + count(customer[which(customer$NetPromoter == 5),]) + count(customer[which(customer$NetPromoter == 6),])
detractor = detractor/count(customer)
detractor

passive <- count(customer[which(customer$NetPromoter == 7),]) + count(customer[which(customer$NetPromoter == 8),])
passive = passive/count(customer)
passive

promoter <- count(customer[which(customer$NetPromoter == 9),]) + count(customer[which(customer$NetPromoter == 10),])
promoter = promoter/count(customer)
promoter

# Create NPS Pie Chart
pie.data <- read.table(text="
CustomerType, value
Detractor, 0.5368421
Passive, 0.2641604
Promoter, 0.1989975", header=TRUE, sep=",")

bp<- ggplot(pie.data, aes(x="", y=value, fill=CustomerType))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Have the % Value of each Segment shown on chart
pie + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)


# Create a basic bar
pie = ggplot(pie.data, aes(x="", y=value, fill=CustomerType)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Net Promotor Score (NPS)")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie

# A major goal of the loyalty program is to increase customer loyalty and expand the program. However, 
# in order to do so, the merchant members must be sure that the customers are satisffied with the program and would 
# recommend it to others. From the pie chart, we see that 54% of survey takers are detractors and would talk badly about
# the loyalty program and are not satisfied with the program. In essence, 54% of survey takers will not recommed the loyalty
# program to a peer.

# NPS Scores between 7 and 8 are passive customers, they are neither loyal or disloyal and are likely to leave if there are better 
# competitive offerings. Passive consumers are often satisfied but not unenthusiastic, not highly likely to recommend the loyalty 
# program to their peers.

# Promoters are customers who are satisfied with the loyalty program and are likely to recommend the program to their peers. 
# Promoters are also more likley to be more loyal which entails that they will be long term customers who will repurchase
# products from the program/merchant increasing customer retention.

# Calculation of NPS
(promoter - detractor) * 100

# The NPS score of the program is -34%, this is very low and is not desirable. An NPS score above or around 50% is usually considered
# to be good, and the FSG Loyalty Program is far from reaching that. 

# Find Mean Satisfaction Values
mean(customer$Sat_Program)
mean(customer$Sat_FastFood)
mean(customer$Sat_Petrol)
mean(customer$Sat_Grocery)
mean(customer$NetPromoter)

# Read Mean Satisfaction Values into Data Frame
combined.data2 <- read.table(text="
Firm, AvgSat
Program,7.26
Petrol,8.08
FastFood,7.20
Grocery, 6.76
NetPromoter, 5.94", header=TRUE, sep=",")

combined.data2 

# Plot average satisfaction
SatPlot <- combined.data2 %>% ggplot(aes(x=Firm, y=AvgSat)) + geom_col(aes(fill=Firm)) 
SatPlot <- SatPlot + geom_text(aes(label = combined.data2$AvgSat), vjust = 2)
print(SatPlot + labs(title="Comparing the Average Customer Satisfaction of Each Merchant",
                     y="Customer Satisfaction", x = "Firm"))



# Binomial Regression predicting customers active in 2016
loyalty.reg <- glm(Active2016 ~ NetPromoter +Sat_Grocery + Sat_Program + Sat_FastFood + Sat_Petrol, data=customer.merged, family = binomial())
summary(loyalty.reg)


# Calculating probabilities by substituting values into binomial regression:

# Calculation substituting the values for our detractor group
calc <- (1/(1 + exp(-5.910551  + (0.180271 * 2.161863887) + (0.007996 * 6.339055794) + (-0.112420  * 5.82035561) + (0.260620 * 6.35928878) + (0.679713 * 7.823421214))))
(1 - calc) *100

# Calculation substituting average values
avgcalc <- (1/(1 + exp(-5.910551  + (0.180271 * 5.940351) + (0.007996 * 6.7599) + (-0.112420  * 7.262657) + (0.260620 * 7.195489) + (0.679713 * 8.078195))))
(1 - avgcalc) *100


# Creating Data Frame
combined.data2 <- read.table(text="
Firm, NumPurchaseTrans, NumRedeemTrans, TotalSales, TotalRedeemDollars, AvgSalesPerTrans, AvgRedeemDollarPerTrans, AverageBenefit, Benefit
Petrol,34190, 117, 812541.94, 1889.92, 23.76548523, 16.15316239, 7.612322836, 810652.02
FastFood, 6090, 532, 68663.28, 3317.45, 11.27475862, 6.235808271, 5.03895035, 65345.83
Grocery, 3525, 0, 263759.8, 0, 74.82547518, 0, 74.82547518, 263759.8", header=TRUE, sep=",")

combined.data2

# 'title' in print statement has what graphs are being made
SatPlot <- combined.data2 %>% ggplot(aes(x=Firm, y=AverageBenefit)) + geom_col(aes(fill=Firm)) 
print(SatPlot + labs(title="Comparing the Redeemed Dollars at Each Firm",
                     y="Redeemed Dollars ($)", x = "Firm"))

SatPlot <- combined.data2 %>% ggplot(aes(x=Firm, y=AverageBenefit)) + geom_col(aes(fill=Firm)) 
SatPlot <- SatPlot + scale_y_continuous(labels=comma)
print(SatPlot + labs(title="Comparing the Average Benefit per Transaction in Dollars at Each Firm",
                     y="Total Benefit ($)", x = "Firm"))

combined.data <- read.table(text="
Firm, Purpose, Amount
Petrol, 'Redeemed Dollars', 1889.92 
Petrol, 'Sales', 812541.94 
Petrol, 'Benefit', 810652.02 
Grocery, 'Redeemed Dollars', 0 
Grocery, 'Sales', 263759.8 
Grocery, 'Benefit', 263759.8 
FastFood, 'Redeemed Dollars', 3317.45 
FastFood, 'Sales', 68663.28 
FastFood, 'Benefit', 65345.83", header=TRUE, sep=",")

combined.data
barchart <- ggplot(combined.data, aes(x=Firm, y=Amount, fill=Purpose)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90))

barchart <- barchart + scale_y_continuous(labels=comma)

print(barchart + labs(title="Comparing the Total Points Redeemed vs Purchased at each Sales Firm",
                      y="Dollars", x = "Firm"))


combined.data <- read.table(text="
Firm, Purpose, Amount
Petrol, 'Redeemed Dollars', 1889.92 
Petrol, 'Sales', 812541.94 
Petrol, 'Benefit', 810652.02 
Grocery, 'Redeemed Dollars', 0 
Grocery, 'Sales', 74.82547518 
Grocery, 'Benefit', 263759.8 
FastFood, 'Redeemed Dollars', 3317.45 
FastFood, 'Sales', 68663.28 
FastFood, 'Benefit', 65345.83", header=TRUE, sep=",")

combined.data
barchart <- ggplot(combined.data, aes(x=Firm, y=Amount, fill=Purpose)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90))
barchart <- barchart + scale_y_continuous(labels=comma)

print(barchart + labs(title="Comparing the Total Points Redeemed vs Purchased at each Sales Firm",
                      y="Dollars", x = "Firm"))


# Create plt1 which groups by SalesFirm and calculates average Rewards Points
plt1 <- purchase %>% group_by(SalesFirm) %>% summarise(AveragePoints = mean(PointReward)) 

plt1

PurchasePlot <- plt1 %>% ggplot(aes(x=SalesFirm, y=AveragePoints)) + geom_col(aes(fill=SalesFirm)) 

PurchasePlot <- PurchasePlot + scale_y_continuous(labels=comma)

print(PurchasePlot + labs(title="Comparing the Average Points Generated per Transaction of each Sales Firm",
                          y="Average Points Generated", x = "Sales Firm"))



# Find mean NPS based on Gender
plt2 <- customer %>% group_by(Gender) %>% summarise(AverageNetPromoter = mean(NetPromoter)) 

PurchasePlot <- plt2 %>% ggplot(aes(x=Gender, y=AverageNetPromoter)) + geom_col(aes(fill=Firm)) 

PurchasePlot <- PurchasePlot + scale_y_continuous(labels=comma)

PurchasePlot <- PurchasePlot + ylim(0, 10)

print(PurchasePlot + labs(title="",
                          y="Net Promoter Score", x = "Gender"))


# Filter data
FF.data <- customer.merged[which(customer.merged$SalesFirm == 'FastFood'),]
FF.M.data <- FF.data[which(FF.data$Gender == 'M'),]
FF.F.data <- FF.data[which(FF.data$Gender == 'F'),]

# Find mean NPS for FastFood Customers based on Gender
mean(FF.M.data$NetPromoter)
mean(FF.F.data$NetPromoter)

mean(FF.M.data$Sat_Program)
mean(FF.F.data$Sat_Program)

mean(FF.M.data$SalesAmt)
mean(FF.F.data$SalesAmt)

G.data <- customer.merged[which(customer.merged$SalesFirm == 'Grocery'),]
G.M.data <- G.data[which(G.data$Gender == 'M'),]
G.F.data <- G.data[which(G.data$Gender == 'F'),]

mean(G.M.data$NetPromoter)
mean(G.F.data$NetPromoter)

mean(G.M.data$Sat_Program)
mean(G.F.data$Sat_Program)

mean(G.M.data$SalesAmt)
mean(G.F.data$SalesAmt)

P.data <- customer.merged[which(customer.merged$SalesFirm == 'Petrol'),]
P.M.data <- P.data[which(P.data$Gender == 'M'),]
P.F.data <- P.data[which(P.data$Gender == 'F'),]

mean(P.M.data$NetPromoter)
mean(P.F.data$NetPromoter)

mean(P.M.data$Sat_Program)
mean(P.F.data$Sat_Program)

mean(P.M.data$SalesAmt)
mean(P.F.data$SalesAmt)



FF.CC.data <- FF.data[which(FF.data$OwnCreditCard == 'Y'),]
FF.NCC.data <- FF.data[which(FF.data$OwnCreditCard == 'N'),]

mean(FF.CC.data$NetPromoter)
mean(FF.NCC.data$NetPromoter)

mean(FF.CC.data$Sat_Program)
mean(FF.NCC.data$Sat_Program)

mean(FF.CC.data$SalesAmt)
mean(FF.NCC.data$SalesAmt)


G.CC.data <- G.data[which(G.data$OwnCreditCard == 'Y'),]
G.NCC.data <- G.data[which(G.data$OwnCreditCard == 'N'),]

# No Credit Card spends $81.96 vs $60.79 per trans. Has lower Net Promoter and Sat_Program 
mean(G.CC.data$NetPromoter)
mean(G.NCC.data$NetPromoter)

mean(G.CC.data$Sat_Program)
mean(G.NCC.data$Sat_Program)

mean(G.CC.data$SalesAmt)
mean(G.NCC.data$SalesAmt)

FF.CC.data <- FF.data[which(FF.data$OwnCreditCard == 'Y'),]
FF.NCC.data <- FF.data[which(FF.data$OwnCreditCard == 'N'),]

mean(P.M.data$NetPromoter)
mean(P.F.data$NetPromoter)

mean(P.M.data$Sat_Program)
mean(P.F.data$Sat_Program)

mean(P.M.data$SalesAmt)
mean(P.F.data$SalesAmt)



# CLUSTER ANALYSIS

# Segment Data to only detractors
cust.segment <- cust.segment[which(cust.segment$Sat_Program <= 6),]
cust.segment <- cust.segment[which(cust.segment$NetPromoter <= 6),]

# only cluster all the base variables (not the demographics!!)
customer.clust <- scale(cust.segment[,c(1,5,17)])

##HCA
# Calculate distance matrix
dist.cust <- dist(customer.clust, method = "euclidean") 

# Conduct hierarchical cluuster analysis
hclust.pda <- hclust(dist.cust, method="ward.D")

# Plot dendogram -> might take a while
# DO NOT DO WITH LOTS OF DATA: WILL CRASH R
plot(hclust.pda, hang =-1)

# Insert number of groups you see in dendogram
nGroup <- 3

# Cut tree into nGroup clusters
groups <- cutree(hclust.pda, k=nGroup) 

# draw dendrogram with red borders around the nGroup clusters: Change 1:3 to however many groups you have
rect.hclust(hclust.pda, k=nGroup, 1:3, border="red")


# Add groups to data frame
cust.segment <- cbind(cust.segment, groups)

# export new dataset with group column
write.csv(cust.segment, "test_cluster2.csv")

# Set the working dorectory to where the csv file will be created. Then, you can open the file
# in Excel and use pivot tables and other tools.


# Linear regression
test <- lm(Sat_Grocery ~ Sat_Petrol, data=customer)
summary(test)
Program <- lm(Sat_Program ~ Sat_Petrol + Sat_Grocery + Sat_FastFood, data=customer)
NPS <- lm(NetPromoter ~ Sat_Petrol + Sat_Grocery + Sat_FastFood, data=customer)

summary(Program)
summary(NPS)

# Count number of transaction made by each customer
test <- customer.merged %>% group_by(MemberID) %>% summarise(num = table(MemberID)) 

table(test$MemberID)
summary(test)

count = 0
for (i in 1:length(test$num)) {
  if (i > 1) {
    count = count + 1
  }
}

count

# Arrange data into bins
test1 <- test[which(test$num > 100),]
test1 <- test1[which(test1$num <= 150),]


summary(test1)

# Create Data Frame
repeat.data <- read.table(text="
NumPurch, NumCust
1, 180
2 - 10, 781
11 - 25, 482
26 - 50, 328 
51 - 100, 177
101 - 150, 31 
150+, 16", header=TRUE, sep=",")

repeat.data
repeat.data$NumPurch <- factor(repeat.data$NumPurch, levels = repeat.data$NumPurch)

# Plot Data
barchart <- ggplot(repeat.data, aes(x=NumPurch, y=NumCust, fill=NumPurch)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90))
barchart <- barchart + scale_y_continuous(labels=comma)
barchart <- barchart + geom_text(aes(label = repeat.data$NumCust), vjust = 2)
print(barchart + labs(title="Comparing the Number of Returning Customers",
                      y="Number of Customers", x = "Number of Purchases"))

# You made it to the end