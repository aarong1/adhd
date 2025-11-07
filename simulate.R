# simulate.R

x <- read_fst("./yearly_start_populations.fst")

x <- x |> 
  mutate(death = na_if(death,0))

# x <- x |>
#   as_tibble() |>
#   count(age, sex  , HSCT, year, name='pop')

pop <- x |> 
  select(id,age,sex,year,death) #|> 
# slice_sample(prop=0.1)

pop <- pop |> 
  #c("3-5", "6-9",  "10-15", "16-17",  "18-29", "30-39", "40-49", "50+")
  mutate(
    age_adhd = cut(
      as.numeric(age),
      breaks = c(-Inf, 2, 5, 9, 15 ,17, 29, 39, 49, Inf),
      labels = c("0-2", "3-5", "6-9", "10-16", "16-17", "18-29", "30-39", "40-49", "50+")
    )
  )

pop <- pop |> 
  left_join(incid_df, 
            by = c(sex = 'sex',age_adhd = 'age', 'year')) 

pop <- pop |> 
  left_join(prev_df, 
            by = c(sex = 'sex',age_adhd = 'age', 'year'))

# sum(na.rm =T,pop$prev_prob > runif(row(pop)))
# 12414/220413

# sum(na.rm =T,pop$incid_prob > runif(row(pop)))
# 830/220413

pop <- pop |> 
  mutate( ADHD_baseline = ifelse(prev_prob > runif(n()),'ADHD',NA))

# pop |> 
#   count(year,age_adhd, ADHD) |> 
#   filter(!is.na(ADHD)) |> 
#   ggplot(aes(year, n*100, fill = age_adhd)) +
#   geom_col()

# check alive

# ( (death > year ) | is.na(death) )
# &
# is.na(ADHD) # check no adhd

pop |> count(ADHD_baseline,death)

pop |> count(year,ADHD_baseline)

pop <- pop |> 
  mutate( ADHD = ifelse(prev_prob > runif(n()) & year==2020,'adhd',NA))

pop <- pop |> 
  mutate( ADHD = ifelse(!is.na(ADHD_baseline) & year==2020,'adhd',NA))

pop |> count(ADHD,death)


count(pop,ADHD,year)

pop <- pop |> 
  group_by(id) |> 
  arrange(year) |> 
  fill(ADHD, .direction = 'down') |> 
  ungroup()

count(pop,ADHD,year)

sus <- pop |> 
  split( ~ ( (death > year ) | is.na(death) ) &
           is.na(ADHD)  ) %>%
  pluck('TRUE')

exempt <- pop |> 
  split( ~ ( (death > year ) | is.na(death) ) &
           is.na(ADHD)  ) %>%
  pluck('FALSE') 

exempt <-  exempt |> 
  mutate( ADHD_incid = NA,
          ADHD_incid_prev = NA)

sus <- sus |> 
    mutate( ADHD_incid = ifelse(incid_prob/10 > runif(n()), year, NA))

count(sus,is.na(ADHD_incid),year) 

sus <- sus |> 
  group_by(id) |> 
  arrange(year) |> 
  fill(ADHD_incid, .direction = 'down') |> 
  ungroup()

count(sus,ADHD,year)
count(pop,ADHD,year)

count(sus,is.na(ADHD_incid),year) 
# 2561/210926 * 5

sus <- sus |> 
  mutate( ADHD_incid_prev = coalesce(as.character(ADHD_incid),ADHD)) 

count(sus,ADHD_incid_prev,year)

count(sus,as.character(ADHD_incid),ADHD)

# sus <- sus |>
#   mutate( ADHD_incid_prev = ifelse(is.na(ADHD), ADHD_incid, ADHD)) #as.character(ADHD_incid)))

# sus |>
#   mutate( ADHD_incid_prev = coalesce(as.character(ADHD_incid)),ADHD) |> 
#   filter(!is.na(ADHD_incid_prev))

pop <- rbind(sus,exempt)

pop <- pop |> 
  mutate( ADHD_incid_prev = coalesce(as.character(ADHD_incid),ADHD)) 

count(pop,ADHD_baseline,year)
count(pop,ADHD,year)
count(pop,ADHD_incid = is.na(ADHD_incid),year)
count(pop,ADHD_incid_prev = is.na(ADHD_incid_prev),year)

#2030
2561/210926 * 5*100
# 0.06070849

# pop |> 
#   group_by(year) |>
#   summarise(
#     sim_incid_prev = sum(!is.na(ADHD_incid_prev) , na.rm = T)/n(),
#     sim_incid_prev1 = sum(!is.na(ADHD_incid) , na.rm = T)/n(),
#     sim_incid_prev2 = sum(!is.na(ADHD) , na.rm = T)/n(),
#     #c=sum(!is.na(ADHD_incid_prev) , na.rm = T),
#     #n=n()

#   ) |> 
#   pivot_longer(-c(year),
#                names_to = 'type',
#                values_to = 'prevalence') |>
#   ggplot(aes(year)) +
#   geom_line(aes(y=prevalence,col=type))+
#   ylim(0,NA)


