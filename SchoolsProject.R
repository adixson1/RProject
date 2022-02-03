#Ayara Dixson
#R Project
#November24

#Read in the school csv file
CAschools<- read.csv("/Users/ayara/Downloads/CASchools.csv")
head(CAschools)

#Average Math and Reading score in district
mean(CAschools$read)
mean(CAschools$math)

#Minimum and Max incomes
min(CAschools$income, na.rm=TRUE)
max(CAschools$income, na.rm=TRUE)

#Create the score by adding the math and reading scores and averaging
CAschools$score<-((CAschools$math+CAschools$read)/2)

#Create a loop that assigns High, Above Average, Average, Below Average, and Low incomes
income2<-na.omit(CAschools$income)
is.na(income2)
avginc<-(income2)
n <- length(avginc)

for(a in 1:n){
  if(avginc[a]> 45){
    print("High")  
  } else if(avginc[a]>=35&&avginc[a]<=45){
    print("Above Average")
  }
  else if(avginc[a]>=25 && avginc[a]<=35){
    print("Average")
  }
  else if(avginc[a]>=15 && avginc[a]<=25){
    print("Below Average")
  }
  else if(avginc[a]<5){
    print("Low")
  }
  else if(avginc[a]=="" ){
    print("NA")
  }
}

#Create a scatter plot based on test scores and reduced lunch
plot(score~lunch, data=CAschools, col="blue", pch=1, xlim=c(0,100),
     cex.main=0.9, main="Scores Based on Percentage Qualifying for Reduced Lunch")


#Create a scatter plot based on test scores and income assistance
plot(score~calworks, data=CAschools, col="red", pch=1, xlim=c(0,100),
     cex.main=0.9, main="Scores Based on Percentage Qualifying for Income Assistance")

