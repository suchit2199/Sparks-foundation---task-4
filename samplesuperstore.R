install.packages("tidyverse")
library(tidyverse)
samplesuperStore <- read.csv(file.choose(), header = TRUE, sep = ',')
View(samplesuperStore)

#Summary
dim(samplesuperStore)
str(samplesuperStore)
summary(samplesuperStore)
head(samplesuperStore)
tail(samplesuperStore)


#Check and remove missing values
colSums(is.na(samplesuperStore))

#Rrequency of category
freqcategory <- table(samplesuperStore$Category)
View(freqcategory)
#Calculate revenue
samplesuperStore1 <- mutate(samplesuperStore, Revenue = Sales*Quantity)
samplesuperStore1
#Revenue by category
revenueperCategory <- samplesuperStore1 %>% group_by(Category) %>% summarise(total_revenue = sum(Revenue))
View(revenueperCategory)
#Graph
a <- ggplot(revenueperCategory,aes(Category,total_revenue)) + geom_bar(stat="identity", fill="steelblue" ,width=0.5) +
     geom_text(aes(label=total_revenue), vjust=-0.3, size=3.5)+
     theme_minimal() 
a


#Frequency of subcategory
freqsubcategory <- table(samplesuperStore1$Sub.Category)
View(freqsubcategory)
#Revenue by subcategory
revenueperSubCategory <- samplesuperStore1 %>% group_by(Sub.Category) %>% summarise(totalrevenue = sum(Revenue))
View(revenueperSubCategory)
#Graph
b <- ggplot(revenueperSubCategory,aes(Sub.Category,totalrevenue)) + geom_bar(stat="identity", fill="steelblue") + 
     geom_text(aes(label=totalrevenue), vjust=-0.3, color="black", size=2.5) +
     theme_minimal()   
b
c <- b + coord_flip()
c

#Revenue by region
revenueperRegion <- samplesuperStore1 %>% group_by(Region) %>% summarise(totalrevenue = sum(Revenue))
View(revenueperRegion)
#Graph
d <- ggplot(revenueperRegion,aes(Region,totalrevenue)) + geom_bar(stat="identity", fill="lightblue",width = 0.5) + 
     geom_text(aes(label=totalrevenue), vjust=-0.3, color="black", size=2.5) +
     theme_minimal()  
d

#Revenue by State
revenueperState <- samplesuperStore1 %>% group_by(State) %>% summarise(totalrevenue = sum(Revenue))
View(revenueperState)
#Arrange 
revenueperState1 <- arrange(revenueperState,revenueperState$totalrevenue)
View(revenueperState1)
#Top 10 State by Revenue
revenueperState2 <- slice(revenueperState1,39:49)
View(revenueperState2)
#Graph
e <- ggplot(revenueperState2,aes(State,totalrevenue)) + geom_bar(stat="identity", fill="lightblue",width = 0.5) + 
     geom_text(aes(label=totalrevenue), vjust=-0.3, color="black", size=2.5) +
     theme_minimal()  
e

#Revenue by ShipMode
revenueperShipMode <- samplesuperStore1 %>% group_by(Ship.Mode) %>% summarise(totalrevenue = sum(Revenue))
View(revenueperShipMode)
#Graph
f <- ggplot(revenueperShipMode,aes(Ship.Mode,totalrevenue)) + geom_bar(stat="identity", fill="lightblue",width = 0.5) + 
     geom_text(aes(label=totalrevenue), vjust=-0.3, color="black", size=2.5) +
     theme_minimal()  
f

#quantity
quantityVScategory <-  samplesuperStore1 %>% group_by(Category) %>%  summarise(totalquantity = sum(Quantity)) %>%
  mutate(Category= factor(Category, levels = c("Furniture","Office Supplies","Technology")),
         cumulative = cumsum(totalquantity),
         midpoint = cumulative - totalquantity / 2,
         label = paste0(Category, " ", round(totalquantity / sum(totalquantity) * 100, 1), "%"))
View(quantityVScategory)
#Graph
g <- ggplot(quantityVScategory, aes(x = 1, weight = totalquantity, fill = Category)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_minimal() 
g

#sales as per segment
salesperSegment <- samplesuperStore1 %>% group_by(Segment) %>% summarise(sales = sum(Sales))
View(salesperSegment)
#Graph
h <- ggplot(salesperSegment,aes(Segment,sales)) + geom_bar(stat="identity", fill="red",width = 0.5) + 
  geom_text(aes(label=sales), vjust=-0.3, color="black", size=2.5) +
  theme_minimal()
h

#correlation
samplesuperStore2 <- samplesuperStore %>% select("Postal.Code","Sales","Quantity","Discount","Profit")
View(samplesuperStore2)
correlation <- round(cor(samplesuperStore2),1)
correlation
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(correlation,method = "circle")

#Linear regression
lmSales <- lm(samplesuperStore2$Profit~samplesuperStore$Sales) 
summary(lmSales)
sales <- samplesuperStore2$Sales[1:100]

plot(samplesuperStore2$Sales,samplesuperStore2$Profit)
abline(lmSales, col = "green")
plot(lmSales)







