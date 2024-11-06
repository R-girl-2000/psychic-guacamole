library(tidyverse)
KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_10_25_1834
KOA_Long<- pivot_longer(KneeOsteoarthritisSt_GlucoseDOM_DATA_2024_10_25_1834,
                        cols= c(glucose_visit2, glucose_visit5),
                        names_to = "Visit_No.", values_to = "blood_glucose",
                        values_drop_na = TRUE ) |> 
  mutate(
    Visit_No. = parse_number(Visit_No.)
  )
KOA_Long
KOA_Long<-KOA_Long[-c(1:3)]
KOA_Long


#KOA_Long<- KOA_Long |> 
#mutate(
    #group = rep(1:41, 2))
#KOA_Long

ggplot(data=KOA_Long3,mapping = aes(x= Visit_No., y= blood_glucose, z = ps_pid)) +
geom_line(mapping = aes(color = ps_pid))


 
KOA_Long |> 
  mutate(
    facet_1 = rep(c(1,1,2,2), 18),
    facet_2 = rep(c(1,1,2,2), 18)
  )
ggsave("BG_DP_01_to_051.jpg")