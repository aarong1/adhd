# https://digital.nhs.uk/data-and-information/publications/statistical/adult-psychiatric-morbidity-survey/survey-of-mental-health-and-wellbeing-england-2023-24/data-sets

#chapter 9 
#ADHD

       
  
asrs_all <- tibble::tribble(
 ~sex, ~asrs, ~`16-24`, ~`25-34`, ~`35-44`, ~`45-54`, ~`55-64`, ~`65-74`, ~`75+`, ~Total,
'Females','0',  26.3, 31.9, 34.6, 32.9, 41.9, 49.6, 53.8, 38.1,
'Females','1',  17.4, 27.1, 19.4, 24.9, 24.8, 23.9, 22.8,   23,
'Females','2',  11.4, 14.3, 13.5, 16.5,   12, 13.9, 11.9, 13.4,
'Females','3',  17.6, 10.8, 12.4,   10,    8,  7.6,  7.3, 10.5,
'Females','4',  10.4,  7.7, 10.6,  7.6,  7.9,  3.1,  3.1,  7.4,
'Females','5',  10.3,  5.8,  6.4,  5.9,  4.3,  1.6,    1,  5.1,
'Females','6',   6.6,  2.5,  3.1,  2.3,  1.1,  0.4,  0.2,  2.3,
'Females','4 or more',  27.3,   16, 20.1, 15.7, 13.3,    5,  4.3, 14.9,
'Males', '0',  25.1, 25.2, 34.4, 34.7, 39.8, 54.3, 57.1, 37.2,
'Males', '1',  18.8, 22.2, 22.5, 25.2, 23.6, 23.7, 22.3, 22.7,
'Males', '2',  19.8, 20.3, 19.1, 14.7, 15.5, 12.6, 12.3, 16.7,
'Males', '3',  15.8, 13.7, 11.3, 13.8,    9,  5.7,  5.4, 11.1,
'Males', '4',  10.3, 11.1,  5.9,  6.2,  6.3,  2.4,  2.7,  6.7,
'Males', '5',   7.2,  5.8,  4.1,  4.3,  4.9,  0.9,  0.1,  4.2,
'Males', '6',   3.1,  1.7,  2.7,  1.1,  0.8,  0.3,  0.1,  1.5,
'Males', "4 or more",   20.6,   18.6,   12.7,   11.6,   12.1,    3.6,      3,   12.4
)

asrs_warrants_test <- asrs_all |> 
  filter(asrs == '4 or more') |> 
  mutate(asrs = asrs)

asrs_5 <- asrs_all |> 
  filter(asrs %in% c('5','6'))|> 
  mutate(asrs = '5 or more')

asrs_all_6 <- asrs_all |> 
  filter(asrs == '6')|> 
  mutate(asrs = 'All 6')

asrs <- rbind(asrs_warrants_test,
      asrs_5,
      asrs_all_6)

asrs |> 
  pivot_longer(cols = -c(1,2)) |> 
  filter(asrs == 'All 6') |> 
  group_by(paste( sex) )|> 
  e_charts(name) |> 
  e_tooltip() |> 
  e_bar(value) 
  
asrs |> 
  pivot_longer(cols = -c(1,2)) |> 
  filter(asrs == '5 or more') |> 
  group_by(paste( sex) )|> 
  e_charts(name) |> 
  e_tooltip() |> 
  e_bar(value) 

asrs |> 
  pivot_longer(cols = -c(1,2)) |> 
  filter(asrs == '4 or more') |> 
  group_by(paste( sex) )|> 
  e_charts(name) |> 
  e_tooltip() |> 
  e_bar(value) 


