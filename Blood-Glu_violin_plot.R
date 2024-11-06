KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302
#KOA_Long3<- pivot_longer(KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302,
                       # cols= c(glucose_visit2, glucose_visit5),
                        #names_to = "Visit_No.", values_to = "blood_glucose",
                        #values_drop_na = TRUE ) |> 
 # mutate(
    #Visit_No. = parse_number(Visit_No.)
  #)
#KOA_Long3

#create violin plot for direct data
KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$Blood_glucose_dif <- KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$glucose_visit5 -
  KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$glucose_visit2
KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302


o<- ggplot(data=KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302,mapping = aes(x= anthtro_diet_visit2, y= Blood_glucose_dif, fill = anthtro_diet_visit2)) +
  geom_violin() +
  coord_flip() +
  xlab("Diet Type ") +
  ylab("Blood Glucose Difference Scores") +
 labs(fill = "Diet Type") 
  
green_plot<- o+ scale_fill_brewer(palette = "greens")
green_plot

p<- ggplot(data= KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302, mapping = aes(x =anthtro_diet_visit2, y =Blood_glucose_dif, fill = anthtro_diet_visit2)) +
  geom_boxplot() +
  geom_jitter(aes(), show.legend = F) +
  xlab("Diet Type ") +
  ylab("Blood Glucose Difference Scores") +
  labs(fill = "Diet Type") 



box_plot1<- p + scale_fill_brewer(aesthetics = c("fill", "color"), palette = "greens") 
box_plot1

#generate descriptive stats by group
  library(summarytools)
common_stats<-  stby(
   data = KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302,
   #sort by group
   INDICES  = KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$anthtro_diet_visit2,
   #function
   FUN = descr,
   stats = "common" #common descriptives
  )
common_stats

#use the psych package to do the same thing:
install.packages("psych")
library(psych)
describeBy(
  KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302,
  KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$anthtro_diet_visit2
)
#lets do t-tests!
KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$BG_mean <- (KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$glucose_visit2+ KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$glucose_visit5)/2
KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302$BG_mean
library(tidyverse)
#install.packages("ggpubr")
library(ggpubr)
library(rstatix)
#install.packages("car") #contains levene's test
library(car)

leveneTest(Blood_glucose_dif ~ anthtro_diet_visit2, data = KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302)

KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302 |> 
  group_by(anthtro_diet_visit2) |> 
  identify_outliers(Blood_glucose_dif)
#normality tests
KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302 |>
  group_by(anthtro_diet_visit2) |> 
  shapiro_test(Blood_glucose_dif)
 ggqqplot(KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302, x = "BG_mean", facet.by ="anthtro_diet_visit2")
#homogeneity of variance
 KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302 |> levene_test(Blood_glucose_dif ~ anthtro_diet_visit2)
 
 #t-test time
 stat.test2 <- KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302 |> 
   t_test(Blood_glucose_dif ~ anthtro_diet_visit2, var.equal = TRUE, detailed = FALSE) |> 
   add_significance()
 stat.test2
 
 #effect size
 KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_11_01_1302 |> 
   cohens_d(anthtro_diet_visit2 ~ Blood_glucose_dif, var.equal = TRUE)
Box_plot_T<- box_plot1 + stat_compare_means(method="t.test")
Box_plot_2<- Box_plot_T + stat_summary(fun = "mean", size = .8, color = "darkgreen")

Box_plot_3 <- Box_plot_2 + labs(title ="Mean Differences in Fasting Blood Glucose Over 6 Weeks", caption = "Group mean difference scores are highlighted in green", y = "Blood Glucose in mg/dL")

ggsave("BG_T_test.png", path =  "C:/Users/vanditta/OneDrive - UAB - The University of Alabama at Birmingham/Documents")
getwd()