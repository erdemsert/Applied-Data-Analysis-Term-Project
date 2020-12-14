library(dplyr)
library(ggplot2)
library(tidyr)
library(moments)

######################### Turkey's Math, Science, Reading Scores (2003-2018) ################

pisaDataFile <- read.csv("PISA_TestScores.csv")
TurkeyInds <- which(pisaDataFile[1] == 'Turkey')
Turkey_Scores <- data.frame(pisaDataFile[TurkeyInds,])
OecdInds <- which(pisaDataFile[1] == 'OECD')
Oecd_Scores <- data.frame(pisaDataFile[OecdInds,])

value <- c(Turkey_Scores$PISA.Math.Score, Turkey_Scores$PISA.Reading.Score, Turkey_Scores$PISA.Science.Score)
condition <- c(rep("Math", 6), rep("Reading",6), rep("Science", 6) )
specie <- rep(c("2003", "2006", "2009", "2012", "2015", "2018"), 3)
data <- data.frame(specie, condition, value)
cls <- rep(c("#3C3B6E", "red", "lightblue"), 2);

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Turkey", subtitle = "Students Performance in Math, Science, Reading Scores (2003-2018)") +
  xlab("Years") + ylab("PISA Scores") + scale_fill_manual(values = cls) + labs(fill="") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),    # Center title position and size
    plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
    plot.caption = element_text(hjust = 0, face = "italic")# move caption to the left
  ) 

########################### Education Spending ##################

eduSpending <-read.csv("EducationSpending_2015.csv")
eduSpending <- data.frame(eduSpending)
primarySpending <- eduSpending$Value[which(eduSpending[2] == 'Primary')]
secondarySpending <- eduSpending$Value[which(eduSpending[2] == 'Secondary')]
tertiarySpending <- eduSpending$Value[which(eduSpending[2] == 'Tertiary')]

countries <- c("Turkey", "South Korea", "Finland")
specie <- c(rep(countries[1], 3) , rep(countries[2], 3) , rep(countries[3], 3))
Levels <- rep(c("Primary" , "Secondary", "Tertiary"), 3)

value <- c(primarySpending[1], secondarySpending[1], tertiarySpending[1], 
           primarySpending[2], secondarySpending[2], tertiarySpending[2], 
           primarySpending[3], secondarySpending[3], tertiarySpending[3])
data <- data.frame(specie, Levels, value)

# Stacked
ggplot(data, aes(fill=Levels, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("Education Spending (Primary & Secondary & Tertiary)", subtitle = "US dollars/student") +
  xlab("Countries") + ylab("Dollar") + theme(plot.title = element_text(hjust = -0.5)) + theme_bw() +
  geom_text(aes(label = round(value,2)), colour = "white", position = position_stack(vjust = 0.5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),    # Center title position and size
    plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
    plot.caption = element_text(hjust = 0, face = "italic")# move caption to the left
  ) 

################################### Student Well-Being - TR #######################################
library(ggplot2)

student_wellbeing <-read.csv("Student_Wellbeing.csv")

bottomSatisf <-  student_wellbeing$Value[1]+ student_wellbeing$Value[2] +student_wellbeing$Value[3] + student_wellbeing$Value[4]
midSatisf <- student_wellbeing$Value[5] + student_wellbeing$Value[6] + student_wellbeing$Value[7] + student_wellbeing$Value[8]
upSatisf <- student_wellbeing$Value[9] + student_wellbeing$Value[10] +student_wellbeing$Value[11]

data <- data.frame(
  Satisfaction=c("[0-3]", "[4-7]", "[8-10]"),
  count=c(bottomSatisf, midSatisf, upSatisf)
)

data$fraction = data$count / sum(data$count)
data$ymax = cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$category, "\n value: ", data$count)
data$ymin = c(0, head(data$ymax, n=-1))
data$pct <- round(data$fraction*100)
data$pct <- paste(data$pct,"%",sep="")

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Satisfaction)) +
  geom_rect()+
  geom_text( x=3.5, aes(y = labelPosition, label = pct), color = "white")+
  coord_polar(theta="y") +
  xlim(c(2, 4))+ 
  theme_void() + 
  ggtitle("Students' Life Satisfaction | Turkey") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, vjust = -7.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.position = c(0.5, 0.5)
  )

################################### Student Well-Being - OECD #######################################
library(ggplot2)

student_wellbeing <-read.csv("Student_Wellbeing.csv")

bottomSatisf <-  student_wellbeing$Value[34]+ student_wellbeing$Value[35] +student_wellbeing$Value[36] + student_wellbeing$Value[37]
midSatisf <- student_wellbeing$Value[38] + student_wellbeing$Value[39] + student_wellbeing$Value[40] + student_wellbeing$Value[41]
upSatisf <- student_wellbeing$Value[42] + student_wellbeing$Value[43] + student_wellbeing$Value[44]

data <- data.frame(
  Satisfaction=c("[0-3]", "[4-7]", "[8-10]"),
  count=c(bottomSatisf, midSatisf, upSatisf)
)

data$fraction = data$count / sum(data$count)
data$ymax = cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$category, "\n value: ", data$count)
data$ymin = c(0, head(data$ymax, n=-1))
data$pct <- round(data$fraction*100)
data$pct <- paste(data$pct,"%",sep="")

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Satisfaction)) +
  geom_rect()+
  geom_text( x=3.5, aes(y = labelPosition, label = pct), color = "white")+
  coord_polar(theta="y") +
  xlim(c(2, 4))+ 
  theme_void() + 
  ggtitle("Students' Life Satisfaction | OECD") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, vjust = -7.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.position = c(0.5, 0.5)
  )


################################### Enrollment Rate #########################################
library(ggplot2)

enrollmentRate <-read.csv("EnrollmentRate.csv")
enrollmentRate <- data.frame(enrollmentRate)

tur_Enroll <- enrollmentRate$Value[which(enrollmentRate[1] == 'TUR' & enrollmentRate[3] == 'AGE_3' & enrollmentRate[5] == '2018')]
fin_Enroll <- enrollmentRate$Value[which(enrollmentRate[1] == 'FIN' & enrollmentRate[3] == 'AGE_3' & enrollmentRate[5] == '2018')]
kor_Enroll <- enrollmentRate$Value[which(enrollmentRate[1] == 'KOR' & enrollmentRate[3] == 'AGE_3' & enrollmentRate[5] == '2018')]

countries <- c("Turkey", "Finland",  "South Korea")
values <- c(tur_Enroll, fin_Enroll,  kor_Enroll)

df2 <- data.frame(Country=factor(countries,levels=unique(countries)), Number = values )

ggplot(df2, aes(x=Country, y=Number)) +
  geom_bar(stat="identity", fill= c("#ff0000", "#9ecae1", "#9ecae1"))+
  geom_text(aes(label=paste(round(Number,2), "%")), vjust=1.6, color="white", size=3.5)+
  
  xlab("Counrty") + ylab("% in same age group") +
  ggtitle(label = "Student Enrollment Rate", subtitle = "Age 3 (2018)")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),    # Center title position and size
    plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
    plot.caption = element_text(hjust = 0, face = "italic")# move caption to the left
  ) 

################################## Class Sizes ##################################

ClassSizes <-read.csv("ClassSizes.csv")
ClassSizes <- data.frame(ClassSizes)

turkey_Students <- ClassSizes$Students[which(ClassSizes[1] == 'Turkey')]
turkey_Classrooms <- ClassSizes$Student.Classroms[which(ClassSizes[1] == 'Turkey')]
tur_ClassSizes_PRY <- turkey_Students / turkey_Classrooms

finland_Students <- ClassSizes$Students[which(ClassSizes[1] == 'Finland' )]
finland_Classrooms <- ClassSizes$Student.Classroms[which(ClassSizes[1] == 'Finland')]
fin_ClassSizes_PRY <- finland_Students / finland_Classrooms

korea_Students <- ClassSizes$Students[which(ClassSizes[1] == 'Korea' )]
korea_Classrooms <- ClassSizes$Student.Classroms[which(ClassSizes[1] == 'Korea' )]
kor_ClassSizes_PRY <- korea_Students / korea_Classrooms

class_size <- c(tur_ClassSizes_PRY, fin_ClassSizes_PRY, kor_ClassSizes_PRY)
class_year <- ClassSizes$Time
class_country <- ClassSizes$Country
class_subject <- ClassSizes$SUBJECT

data2 <- data.frame(class_country, class_year, class_subject, class_size)

ggplot(data=data2,aes(x=class_year,y=class_size, shape=class_subject, col=class_country))+
  geom_point(size=3)+
  geom_line(size=1)+ theme_light()+
  ggtitle("Average Class Sizes")+
  theme( plot.title = element_text(color = "Black",size = 24,face = "bold",hjust = 0.5),
         legend.title = element_text(size=0),
         legend.position = "top",legend.box = "horizontal",
         legend.text = element_text(size = 15, color = "Black"),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10))+
  xlab("Year")+
  ylab("Class size")

######################## Average PISA Test Scores from 2003 to 2018 (Turkey, Finland, South Korea, OECD) ########################
library(ggplot2)

test_result <- read.csv("PISA_TestScores.csv")
test_result$pisa_average <- (test_result$PISA.Math.Score +test_result$PISA.Science.Score+test_result$PISA.Reading.Score)/3
head(test_result)

ggplot(data=test_result,aes(x=Year,y=pisa_average,col=Entity,shape=Entity), )+
  geom_point(size=3)+
  geom_line(size=1)+
  ggtitle("PISA Test Results")+
  theme( plot.title = element_text(color = "Black",size = 14,face = "bold",hjust = 0.5),
         legend.title = element_text(size=0),
         legend.position = "right",
         legend.text = element_text(size = 15, color = "Black"),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10))+
  xlab("Year (2003-2018)")+
  ylab("Average PISA Score")

##################################### Descriptive Statistics####################################
  
#Turkey's descriptive statistic
test_result_Turkey <- test_result %>% dplyr:: group_by(Entity)%>%
dplyr::select(Entity,Year,pisa_average)%>%filter(Entity=="Turkey")

summary_Turkey <- summary(test_result_Turkey$pisa_average)
sd_Turkey <- sd(test_result_Turkey$pisa_average)
skew_Turkey <- round(skewness(test_result_Turkey$pisa_average),2)
kurtosis_Turkey <- round(kurtosis(test_result_Turkey$pisa_average),2)



#Finland descriptive statistic
test_result_Finland <- test_result %>% dplyr:: group_by(Entity)%>%
dplyr::select(Entity,Year,pisa_average)%>%filter(Entity=="Finland")

summary_Finland <- summary(test_result_Finland$pisa_average)
sd_Finland <- sd(test_result_Finland$pisa_average)
skew_Finland <- round(skewness(test_result_Finland$pisa_average),2)
kurtosis_Finland <- round(kurtosis(test_result_Finland$pisa_average),2)



#South Korea descriptive statistic
test_result_SouthKorea <- test_result %>% dplyr:: group_by(Entity)%>%
dplyr::select(Entity,Year,pisa_average)%>%filter(Entity=="South Korea")

summary_SouthKore <- summary(test_result_SouthKorea$pisa_average)
sd_SouthKore <- sd(test_result_SouthKorea$pisa_average)
skew_SouthKore <- round(skewness(test_result_SouthKorea$pisa_average),2)
kurtosis_SouthKore <- round(kurtosis(test_result_SouthKorea$pisa_average),2)



#OECD descriptive statistic
test_result_OECD <- test_result %>% dplyr:: group_by(Entity)%>%
dplyr::select(Entity,Year,pisa_average)%>%filter(Entity=="OECD")

summary_OECD <- summary(test_result_OECD$pisa_average)
sd_OECD <- sd(test_result_OECD$pisa_average)
skew_OECD <- round(skewness(test_result_OECD$pisa_average),2)
kurtosis_OECD <- round(kurtosis(test_result_OECD$pisa_average),2)




