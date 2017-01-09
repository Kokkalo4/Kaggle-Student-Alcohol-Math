#Load the files
studentmath <- read.csv("c:/users/alex/desktop/R/exercise data/Student Alcohol Consumption/student-mat.csv", header = T)

#Load packages
library(dplyr)
library(plyr)
library(Hmisc)
library(ggplot2)
library(gridExtra)

#Checking Structure and values of data
str(studentmath)
describe(studentmath)
head(studentmath)
tail(studentmath)

#Some basic plots to understand the data
ggplot(studentmath, aes(school)) + geom_bar(aes(fill = as.factor(studentmath$school))) +
  scale_fill_discrete(name = "School" ,labels = c("GP: Gabriel Pereira " , "MS: Mousinho da Silveira")) +
  labs(x= "School of Students",y= "Number of Students" , title = "School")

ggplot(studentmath, aes(sex)) + geom_bar(aes(fill = as.factor(studentmath$sex))) +
  scale_fill_discrete(name = "Sex" ,labels = c("Female " , "Male")) +
  labs(x= "Sex of Students",y= "Number of Students" , title = "Sex of Students")

ggplot(studentmath, aes(age)) + geom_bar(aes(fill = as.factor(studentmath$age))) + scale_fill_discrete(name = "Age") +
  labs(x = "Age" , y = "Number of Students" , title = "Age of Students")

ggplot(studentmath, aes(address)) + geom_bar(aes(fill = as.factor(studentmath$address))) + scale_fill_discrete(name = "Address", labels = c("Rural","Urban")) +
  labs(x = "Address" , y = "Number of Students" , title = "Address of Students")

ggplot(studentmath, aes(famsize)) + geom_bar(aes(fill = as.factor(studentmath$famsize))) + scale_fill_discrete(name = "Family Size", labels = c("Greater than 3","Less or Equal to 3")) +
  labs(x = "Family Size" , y = "Number of Students" , title = "Family Size")

ggplot(studentmath, aes(famsize)) + geom_bar(aes(fill = as.factor(studentmath$Pstatus))) + scale_fill_discrete(name = "Family Cohabitation Status" , labels = c("Living Apart", "Living Together")) +
  labs(x = "Family Cohabitation Status" , y = "Number of Students" , title = "Family Cohabitation Status")

ggplot(studentmath, aes(Medu)) + geom_bar(aes(fill = as.factor(studentmath$Medu))) + scale_fill_discrete(name = "Mother Education Level", labels = c("None","Primary Education(4th Grade)","5th to 9th Grade","Secondary Education","Higher Education")) +
  labs(x = "Mother Education Level" , y = "Number of Cases" , title = "Mother Education Level")+ 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

ggplot(studentmath, aes(Fedu)) + geom_bar(aes(fill = as.factor(studentmath$Fedu))) + scale_fill_discrete(name = "Father Education Level", labels = c("None","Primary Education(4th Grade)","5th to 9th Grade","Secondary Education","Higher Education")) +
  labs(x = "Father Education Level" , y = "Number of Cases" , title = "Father Education Level")+ 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

ggplot(studentmath, aes(Mjob)) + geom_bar(aes(fill = as.factor(studentmath$Mjob))) + scale_fill_discrete(name = "Mother's Job" , labels = c("At Home", "Health", "Other", "Services", "Teacher")) +
  labs(x= "Mother's Job" , y = "Number of Cases" , title = "Mother's Job") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(Fjob)) + geom_bar(aes(fill = as.factor(studentmath$Fjob))) + scale_fill_discrete(name = "Father's Job" , labels = c("At Home", "Health", "Other", "Services", "Teacher")) +
  labs(x= "Father's Job" , y = "Number of Cases" , title = "Father's Job") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(reason)) + geom_bar(aes(fill = as.factor(studentmath$reason))) + scale_fill_discrete(name = "Reason for choosing School" , labels = c("Course Preference" , "Close to Home" , "Other" , "School Reputation")) + 
  labs(x = "Reasons for Choosing School" , y = "Number of Students" , title = "Reasons for Choosing School") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(guardian)) + geom_bar(aes(fill = as.factor(studentmath$guardian))) + scale_fill_discrete(name = "Guardian", labels = c("Father","Mother","Other")) +
  labs(x = "Guardian" , y = "Number of Cases" , title = "Student's Guardian") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(traveltime)) + geom_bar(aes(fill = as.factor(studentmath$traveltime))) + scale_fill_discrete(name = "Travel Time", labels =c("<15 min","15 to 30 min","30 min to 1 hour","> 1 hour")) +
  labs(x = "Travel Time" , y = "Number of Cases", title = "Student Travel Time") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(studytime)) + geom_bar(aes(fill = as.factor(studentmath$studytime))) + scale_fill_discrete(name = "Study Time", labels = c("< 2 hours", "2 to 5 Hours", "5 to 10 Hours" , "> 10 Hours")) +
  labs(x = "Study Time" , y = "Number of Cases" , title = "Student Study Time") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(failures)) + geom_bar(aes(fill = as.factor(studentmath$failures))) + scale_fill_discrete(name = "Number of Past Class Failures") +
  labs(x = "Failures", y = "Number of Students" , title = "Number of Past Class Failures") +
  theme(axis.text.x= element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(schoolsup)) + geom_bar(aes(fill = as.factor(studentmath$schoolsup))) + scale_fill_discrete(name = "Extra Educational Support" , labels = c("No","Yes")) +
  labs(x = "Extra School Support" , y = "Number of Students" , title = "Students Extra School Support") +
  theme(axis.text.x= element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(famsup)) + geom_bar(aes(fill = as.factor(studentmath$famsup))) + scale_fill_discrete(name = "Family Educational Support" , labels = c("No","Yes")) +
  labs(x = "Family Educational Support" , y = "Number of Students" , title = "Students Family Educational Support") +
  theme(axis.text.x= element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(paid)) + geom_bar(aes(fill = as.factor(studentmath$paid))) + scale_fill_discrete(name = "Extra Paid Math Classes" , labels = c("No","Yes")) +
  labs(x = "Extra Paid Math Classes" , y = "Number of Students" , title = "Student Paying Extra Math Classes") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(activities)) + geom_bar(aes(fill = as.factor(studentmath$activities))) + scale_fill_discrete(name = "Extra Curricular Activities", labels=c("No","Yes")) +
  labs(x = "Extra Curricular Activities", y = "Number of Students" , title = "Number of Students Attending Extra Curricular Activities") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(nursery)) + geom_bar(aes(fill = as.factor(studentmath$nursery))) + scale_fill_discrete(name = "Attended Nursery" , labels = c("No","Yes")) +
  labs(x = "Attended Nursery" , y = "Number of Students" , title = "Attended Nursery") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(higher)) + geom_bar(aes(fill = as.factor(studentmath$higher))) + scale_fill_discrete(name = "Wants to Take Higher Education" , labels = c("No","Yes")) +
  labs(y = "Number of Students" , title = "Students who want to take Higher Education") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(internet)) + geom_bar(aes(fill = as.factor(studentmath$internet))) + scale_fill_discrete(name = "Internet Access" , labels = c("No","Yes")) +
  labs(x = "Internet Access" , y = "Number of Students" , title = "Students who have Internet Access") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(romantic)) + geom_bar(aes(fill = as.factor(studentmath$romantic))) + scale_fill_discrete(name = "Romantic Relationship?", labels = c("No","Yes")) +
  labs(x = "Romantic Relationship" , y = "Number of Cases" , title = "Students Romantic Relationship") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(famrel)) + geom_bar(aes(fill = as.factor(studentmath$famrel))) + scale_fill_discrete(name = "Quality of Family Relationship" , labels = c("Very Bad", "Bad", "Medium", "Good", "Excellent")) + 
  labs(x = "Family Relationship" , y = "Number of Cases" , title = "Graph of Quality of Family Relationship") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(freetime)) + geom_bar(aes(fill = as.factor(studentmath$freetime))) + scale_fill_discrete(name = "Free Time After School" , labels =c("Very Low", "Low", "Medium", "High", "Very High")) + 
  labs(x = "Free Time" , y = "Number of Cases" , title = "Graph of Free Time After School") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(goout)) + geom_bar(aes(fill = as.factor(studentmath$goout))) + scale_fill_discrete(name = "Going Out with Friends", labels = c("Very Low", "Low", "Medium", "High", "Very High")) +
  labs(x = "Going out with Friends" , y = "Number of Cases" , title = "Students Going Out with Friends") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(Dalc)) + geom_bar(aes(fill = as.factor(studentmath$Dalc))) + scale_fill_discrete(name = "Workday Alcohol Consumption", labels =c("Very Low", "Low", "Medium", "High", "Very High")) +
  labs(x = "Workday Alcohol Consumption" , y = "Number of Cases" , title = "Graph of Student Workday Alcohol Consumption") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(Walc)) + geom_bar(aes(fill = as.factor(studentmath$Walc))) + scale_fill_discrete(name = "Weekend Alcohol Consumption", labels =c("Very Low", "Low", "Medium", "High", "Very High")) +
  labs(x = "Weekend Alcohol Consumption" , y = "Number of Cases" , title = "Graph of Student Weekend Alcohol Consumption") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(health)) + geom_bar(aes(fill = as.factor(studentmath$health))) + scale_fill_discrete(name = "Student Health Status", labels=c("Very Bad","Bad","Average","Good","Very Good")) + 
  labs(x = "Student Health Status", y = "Number of Cases" , title = "Graph of Student Health Status") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(studentmath, aes(absences)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Number of Absences",y= "Number of Cases" , title = "Plot of Student Absences")

#GRADES
graph1 <- ggplot(studentmath, aes(G1)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x="First Period Grade", y = "Number of Cases", title = "Plot of First Period Grades") +
  geom_vline(data=studentmath, aes(xintercept=mean(studentmath$G1),  colour= "red" ),
             linetype="dashed", size=1)
graph2 <- ggplot(studentmath, aes(G2)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x="Second Period Grade", y = "Number of Cases", title = "Plot of Second Period Grades") +
  geom_vline(data=studentmath, aes(xintercept=mean(studentmath$G2),  colour= "red" ),
             linetype="dashed", size=1)
graph3 <- ggplot(studentmath, aes(G3)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x="Third Period Grade", y = "Number of Cases", title = "Plot of Third Period Grades") +
  geom_vline(data=studentmath, aes(xintercept=mean(studentmath$G3),  colour= "red" ),
             linetype="dashed", size=1)
grid.arrange(graph1,graph2,graph3, nrow=3)

#Daily and weekly alcohol consumption and Grade Scores.
graph4 <- ggplot(studentmath, aes(x = Walc, y = G1 , color = sex)) + geom_jitter()
graph5 <- ggplot(studentmath, aes(x = Walc, y = G2 , color = sex)) + geom_jitter()
graph6 <- ggplot(studentmath, aes(x = Walc, y = G3 , color = sex)) + geom_jitter()
grid.arrange(graph4,graph5,graph6, nrow=3)

graph7 <- ggplot(studentmath, aes(x = Dalc, y = G1 , color = sex)) + geom_jitter()
graph8 <- ggplot(studentmath, aes(x = Dalc, y = G2 , color = sex)) + geom_jitter()
graph9 <- ggplot(studentmath, aes(x = Dalc, y = G3 , color = sex)) + geom_jitter()
grid.arrange(graph7,graph8,graph9, nrow=3)

ggplot(studentmath , aes(x = school , y = age , fill = sex)) + geom_bar(stat="identity",position="dodge") + xlab("School") + ylab("Age")



#Looking for Relationships

graph10 <- boxplot(studentmath$health ~ studentmath$Dalc, xlab = "Student Health" ,ylab="Daily Alcohol Consumption" , col="darkgreen", boxwex=0.4 , main="Student Health and Daily Alcohol Consumption")
graph11 <- boxplot(studentmath$health ~ studentmath$Walc, xlab = "Student Health" ,ylab="Weekend Alcohol Consumption" , col="darkgreen", boxwex=0.4 , main="Student Health and Daily Alcohol Consumption")
grid.arrange(graph10,graph11, nrow=2)





ggplot(studentmath, aes(x = Pstatus , y = absences , color = sex)) + geom_jitter()

plot(studentmath$Pstatus,studentmath$absences) #students living apart tend to have more absences than those whose family lives together


ggplot(studentmath , aes(x = school , y = sex , fill = sex)) + geom_bar(stat="identity",position="dodge")
plot(studentmath$school~studentmath$sex)
ggplot(studentmath , aes(x = school , y = age , fill = sex)) + geom_bar(stat="identity",position="dodge")
plot(studentmath$school~studentmath$age)
ggplot(studentmath , aes(x = school , y = address , fill = sex)) + geom_bar(stat="identity",position="dodge")
plot(studentmath$school~studentmath$address)
ggplot(studentmath , aes(x = school , y = famsize , fill = sex)) + geom_bar(stat="identity",position="dodge")
plot(studentmath$school~studentmath$famsize)


plot(studentmath$famsize~studentmath$Pstatus)
barplot(table(studentmath$famsize,studentmath$Pstatus), col= c("red", "green"), main = )

plot(studentmath$famsize~studentmath$Medu)
barplot(table(studentmath$famsize,studentmath$Medu), col= c("red", "green"), main = )

plot(studentmath$famsize~studentmath$Fedu)
barplot(table(studentmath$famsize,studentmath$Fedu), col= c("red", "green"), main = )

barplot(table(studentmath$paid,studentmath$school), col= c("red", "green"), main = )
plot(studentmath$paid~studentmath$school)

barplot(table(studentmath$paid,studentmath$famsize), col= c("red", "green"), main = )
plot(studentmath$paid~studentmath$famsize)

barplot(table(studentmath$paid), col= c("red", "green"), main = )

plot(studentmath$paid~studentmath$Pstatus)
plot(studentmath$paid~studentmath$failures)

ggplot(studentmath, aes(x = traveltime , y = studytime, color = sex)) + geom_jitter() 
ggplot(studentmath, aes(x = traveltime , y = studytime, color = sex)) + geom_bar( stat="identity")
plot(studentmath$absences ~ studentmath$guardian)

plot(studentmath$nursery~studentmath$failures)
ggplot(studentmath, aes(x = school, y = failures , color = sex)) + geom_jitter()
ggplot(studentmath, aes(x = school, y = activities , color = sex)) + geom_jitter()

ggplot(studentmath, aes(x = sex, y = higher )) + geom_jitter()
ggplot(studentmath, aes(x = age , y = higher , color = age)) + geom_jitter()


ggplot(studentmath, aes(x = internet, y = romantic , color = age)) + geom_jitter()

ggplot(studentmath, aes(x = sex, y = romantic , color = age)) + geom_jitter()


ggplot(studentmath, aes(x = higher, y = romantic , color = age)) + geom_jitter()


ggplot(studentmath, aes(x = health, y = Dalc , color = age)) + geom_jitter()
ggplot(studentmath, aes(x = health, y = Dalc , color = sex)) + geom_jitter()

ggplot(studentmath, aes(x = health, y = Walc , color = age)) + geom_jitter()
ggplot(studentmath, aes(x = health, y = Walc , color = sex)) + geom_jitter()

ggplot(studentmath, aes(x = health, y = Walc , color = sex)) + geom_jitter()
boxplot(studentmath$health ~ studentmath$Walc , col="grey")



boxplot(Walc ~ school , data = studentmath, col = "red")

boxplot(Dalc ~ school , data = studentmath, col = "red")


#Copied from Kaggle -- SUPER TO USE FOR OTHER DATASETS

library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(alluvial)
library(waffle)
library(extrafont)



#Summary

#Short exploratory data analysis focusing on the alcohol variables from the Portuguese class dataset. The data is a survey of students in portuguese language courses in secondary school.

#kaggle: Student Alcohol Consumption dataset

#Dataset origin:
  
 # Using Data Mining To Predict Secondary School Student Alcohol Consumption.

#Fabio Pagnotta, Hossain Mohammad Amran

#Department of Computer Science,University of Camerino


#data.source <- read.table("../input/student-por.csv",sep=",",header=TRUE)

#data.source$Dalc <- as.factor(data.source$Dalc)      
#data.source$Dalc <- mapvalues(data.source$Dalc, 
#                              from = 1:5, 
#                              to = c("Very Low", "Low", "Medium", "High", "Very High"))

#data.source$Walc <- as.factor(data.source$Walc)      
#data.source$Walc <- mapvalues(data.source$Walc, 
#                              from = 1:5, 
#                              to = c("Very Low", "Low", "Medium", "High", "Very High"))



#windowsFonts(FontAwesome=windowsFont("FontAwesome"))

#alcohol.d <- as.data.frame(table(data.source$Dalc))
#par.d <- as.numeric(alcohol.d$Freq)
#names(par.d) <- alcohol.d$Var1
#par.d <- round(par.d/10)

#waffle.col <- c("#00d27f","#adff00","#f9d62e","#fc913a","#ff4e50")

#c1 <- waffle(par.d, rows=5, 
             #use_glyph="glass", 
#             size=2, 
#             title = "Workday alcohol consumption among students",
#             glyph_size=8,
#             xlab="1 glass == 10 students",
#             colors=waffle.col,
#             legend_pos= "top"
#)

#alcohol.w <- as.data.frame(table(data.source$Walc))
#par.w <- as.numeric(alcohol.w$Freq)
#names(par.w) <- alcohol.w$Var1
#par.w <- round(par.w/10)

#c2 <- waffle(par.w, rows=5, 
#             #use_glyph="glass", 
#             size=2, 
#             title = "Weekend alcohol consumption among students",
#             glyph_size=8,
#             xlab="1 glass == 10 students",
#             colors=waffle.col,
#             legend_pos= "top"
#)

#grid.arrange(c1,c2, nrow=2)


#School and gender

c3 <- ggplot(studentmath, aes(x=Dalc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("School")+
  ggtitle("Workday alcohol consumption per school and sex")


c4 <- ggplot(data.source, aes(x=Walc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("School")+
  ggtitle("Weekend alcohol consumption per school and sex")

grid.arrange(c3,c4, nrow=2)

#Alcohol and grades
#Workday alcohol consumption and grades

#workday
c5 <- ggplot(data.source, aes(x=Dalc, y=G1, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("First period grade")

c6 <- ggplot(studentmath, aes(x=Dalc, y=G2, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Second period grade")

c7 <- ggplot(studentmath, aes(x=Dalc, y=G3, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Final period grade")

grid.arrange(c5,c6,c7,ncol=3)


#Weekend alcohol consumption and grades

#weekend
c8 <- ggplot(data.source, aes(x=Walc, y=G1, fill=Walc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("First period grade")

c9 <- ggplot(data.source, aes(x=Walc, y=G2, fill=Walc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Second period grade")

c10 <- ggplot(data.source, aes(x=Walc, y=G3, fill=Walc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Final period grade")

grid.arrange(c8,c9,c10,ncol=3)

#Alcohol consumption and school absences

ggplot(studentmath, aes(x=Dalc, y=absences, fill=Dalc))+
  geom_violin()+
  scale_fill_manual(values = waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Absences distribution per Workday alcohol consumption")+
  xlab("Alcohol consumption")+
  ylab("Number of school absences")



#The very high alcohol consumption category has an interesting shape as it expends while others tend to decrease. We can also notice it is nicely shaped as a bottle. Coincidence? I think not.

ggplot(studentmath, aes(x=Walc, y=absences, fill=Walc))+
  geom_violin()+
  scale_fill_manual(values = waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Absences distribution per Weekend alcohol consumption")+
  xlab("Alcohol consumption")


#Alcohol consumption and student's age

ggplot(studentmath, aes(x=age, fill=Dalc))+
  geom_histogram(binwidth=1, colour="black")+
  facet_grid(~Dalc)+
  scale_fill_manual(values= waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Workday alcohol consumption per age")+
  xlab("Student's age")  


ggplot(studentmath, aes(x=age, fill=Walc))+
  geom_histogram(binwidth=1, colour="black")+
  facet_grid(~Walc)+
  scale_fill_manual(values= waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekend alcohol consumption per age")+
  xlab("Student's age")      





---- 
  
str(studentmath)


testtab <- table(studentmath$age)
prop.table(testtab)

testag <- table(studentmath$sex)
prop.table(testag)
