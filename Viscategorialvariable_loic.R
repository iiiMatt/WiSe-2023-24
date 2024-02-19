# contribution by Loic
# Visualisation categorial variable
# the category variables we will be using are :
    # Survived
    # Pclass
    # Sex
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("languageserver")
library(gridExtra)
library(ggplot2)
library(dplyr)
library(gridExtra)

#I'm dividing the data set into two parts - one for those who have survive
#and another for those who have not.
survivedja = subset(titanic_cleaned, Survived == "yes")
survivednein = subset(titanic_cleaned, Survived == "no")
#I count the number of people who have survived or died according to sex
Anzahl_survivor = sum(survivedja[1] == "yes")# 342 Person survive
Anzahl_survivor_manner = sum(survivedja[3] == "male")# 109 male survive 
Anzahl_survivor_frau = sum(survivedja[3] == "female")# 233 female survive 
Anzahl_nichtsurvivor = sum(survivednein[1] == "no")# 549 Person dead
Anzahl_nichtsurvivor_manner = sum(survivednein[3] == "male")# 468 male dead 
Anzahl_nichtsurvivor_frau = sum(survivednein[3] == "female")# 81 female  dead 


vis_categorial_variable <- function(data){
# I compare the data set data with my dataset surviveja and if the dataset
# that is entered is the right one I calculate the Relative frequency of the 
# survivors according to the class they occupied.
  if(identical(deparse(substitute(data)), "survivedja")){
    class1 <- sum(data[2]== 1)/sum(table(data[1]=="yes"))
    class2 <- sum(data[2]== 2)/sum(table(data[1]=="yes"))
    class3 <- sum(data[2]== 3)/sum(table(data[1]=="yes"))
# I calculate the Relative frequency of the  survivors according to the sex
# they have.    
    survivemanner <- sum(data[3]== "male")/sum(table(data[1]=="yes"))
    survivefrauen <- sum(data[3]== "female")/sum(table(data[1]=="yes"))
# here we create a dataframe with two columns (the class and relative frequency)    
    df_class = data.frame( Pclass =  c("Class1", "Class2", "Class3"),
                           relativeHaeufigkeit = c(class1 , class2 , class3 ))
# here we create a dataframe with two columns (the sex and relative frequency)     
    df_sex = data.frame( Sex =  c("Man", "Woman"),
                         relativeHaeufigkeit = c(survivemanner , survivefrauen ))
# we construct a histogram showing the number of people who survived according to their sex
    plot_sex = ggplot(data = df_sex , aes(x = factor(Sex , Sex), y = relativeHaeufigkeit, fill=Sex)) + labs(title ="number of people who survived according to their sex" , x= "Sex",y="relative H?ufigkeit")+
      geom_bar(stat = "identity") + geom_text(aes(label = round(relativeHaeufigkeit,2)), nudge_y = 0.05) + scale_fill_brewer(palette = "Set1") + theme_minimal()
# we construct a histogram showing the number of people who survived according to their class   
    plot_class = ggplot(data = df_class, aes(x = factor(Pclass , Pclass), y = relativeHaeufigkeit, fill=Pclass)) + labs(title ="number of people who survived according to their class" , x= "Pclass",y="relative H?ufigkeit")+
      geom_bar(stat = "identity") + geom_text(aes(label = round(relativeHaeufigkeit,2)), nudge_y = 0.05) + scale_fill_brewer(palette = "Set1") + theme_minimal()
# we display both histograms on the same page    
    result = grid.arrange(plot_sex, plot_class, nrow = 1)
    return(result)
  }else if  (identical(deparse(substitute(data)), "survivednein")){
# I compare the data set data with my dataset survivenein and if the dataset
# that is entered is the right one I calculate the Relative frequency of the 
# died persons according to the class they occupied.
    class1 <- sum(data[2]== 1)/sum(table(data[1]=="no"))
    class2 <- sum(data[2]== 2)/sum(table(data[1]=="no"))
    class3 <- sum(data[2]== 3)/sum(table(data[1]=="no"))
# I calculate the Relative frequency of the died persons according to the sex
# they have    
    deadmanner <- sum(data[3]== "male")/sum(table(data[1]=="no"))
    deadfrauen <- sum(data[3]== "female")/sum(table(data[1]=="no"))
# here we create a dataframe with two columns (the class and relative frequency)    
    df_class = data.frame( Pclass =  c("Class1", "Class2", "Class3"),
                           relativeHaeufigkeit = c(class1 , class2 , class3 ))
# here we create a dataframe with two columns (the sex and relative frequency)    
    df_sex = data.frame( Sex =  c("Man", "Woman"),
                         relativeHaeufigkeit = c(deadmanner , deadfrauen ))
# we construct a histogram showing the number of people who died according to their sex
    plot_sex = ggplot(data = df_sex , aes(x = factor(Sex , Sex), y = relativeHaeufigkeit, fill=Sex)) + labs(title ="number of people who died according to their sex" , x= "Sex",y="relative H?ufigkeit")+
      geom_bar(stat = "identity") + geom_text(aes(label = round(relativeHaeufigkeit,2)), nudge_y = 0.05) + scale_fill_brewer(palette = "Set1") + theme_minimal()
# we construct a histogram showing the number of people who died according to their class    
    plot_class = ggplot(data = df_class, aes(x = factor(Pclass , Pclass), y = relativeHaeufigkeit, fill=Pclass)) + labs(title ="number of people who died according to their class" , x= "Pclass",y="relative H?ufigkeit")+
      geom_bar(stat = "identity") + geom_text(aes(label = round(relativeHaeufigkeit,2)), nudge_y = 0.05) + scale_fill_brewer(palette = "Set1") + theme_minimal()
# we display both histograms on the same page 
    result = grid.arrange(plot_sex, plot_class, nrow = 1)
    return(result)
  }
  
}
vis_categorial_variable(survivedja) 

# here is the information that emerges from our graphs
# 85% of those who died were men and 15% were women
# 15% of those who died were in class1 , 18% in class2 and 68% in class3
# 32% of those who survive were men and 68% were women
# 40% of those who survive were in class1 , 25% in class2 and 35% in class3