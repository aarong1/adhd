# main_output

source('datasets.R')
source('analysis.R')
source('simulate.R')

source("adult_psychiatry_survey.R")

source('ppl_w_learning_disabilities_expt_stats.R')
source('adhd_summary_linked_from_ahdh_uk.R')

source('copilot_results.R')

pop <- pop |>
  mutate(
      age_adhd_sicbl=cut(x = age,
                     breaks = c(-Inf,10,18,25,35,45,55,65,75,Inf),
                     labels = c('0 to 9', '10 to 17', '18 to 24', '25 to 34', '35 to 44',
                                '45 to 54','55 to 64','65 to 74', '75+')
  )
)

pop <- pop |>
  left_join(sicbl,by = c('sex', age_adhd_sicbl='age'))

pop$prev_rate_lower <-2.5/100
pop$prev_rate_higher <- 3.4/100

pop$prev_rate_kid <- 5/100
pop$prev_rate_adult <- 4/100
pop$nice_prev_rate_kid <- 5/100
pop$nice_prev_rate_adult <- 4/100

pop <- pop |>
  mutate(ADHD_prev_sicbl = ifelse(sicbl_prev > runif(n()),'ADHD',NA))

pop <- pop |>
  mutate(ADHD_prev_lower = ifelse(prev_rate_lower > runif(n()),'ADHD',NA))

pop <- pop |>
  mutate(ADHD_prev_higher = ifelse(prev_rate_higher > runif(n()),'ADHD',NA))

pop <- pop |>
  mutate(ADHD_nice_kid = ifelse(nice_prev_rate_kid > runif(n()) & age<19,'ADHD',NA))

pop <- pop |>
  mutate(ADHD_nice_adult = ifelse(nice_prev_rate_adult > runif(n()) & age>=19,'ADHD',NA))

pop <- pop |>
  mutate(
    ADHD_prev_nice = coalesce(ADHD_nice_kid, ADHD_nice_adult)
    )

pop <- pop |>
  mutate(run = sample(1:10,replace = T,size=n()))

# all prevalence actual
# all prevalence rates
# prevalence from my simulation
# incidence from my simulation
# 17 year olds turning 18 from my simulation
# estimated actual prevalence (*9) from my simulation
# referrals from my simulation
# accepted referrals from my simulation

# estimated 'real' incidence from estimated 'real' prevalence

# prevalence from Dr O'Neil
# prevalence from nice
# prevalence from high estimate
# prevalence from low estimate
# prevalence from learning difficulties, experimental statistics

# referrals to trusts from copilot

# Plot
# prevalence_modelled_adult
# referrals_modelled_adult_child

#########################################

# all prevalence rates

pop <- pop |>
  mutate(ADHD_incid_prev = coalesce(as.character(ADHD_incid),ADHD_baseline))

count(pop, year, ADHD_incid,sum(!is.na(ADHD_incid)))
count(pop, year, ADHD_incid,sum(!is.na(ADHD_incid)))

# https://pmc.ncbi.nlm.nih.gov/articles/PMC9884156
# However, fewer than 20% of ADHD adults are accurately diagnosed and treated (Barkley & Brown, 2008; Ginsberg et al., 2014),

(
prevalence_rates_graph <- pop |>

  group_by(year) |>
  summarise(
    sicbl = sum(ADHD_prev_sicbl == 'ADHD', na.rm = T)/n(),
    lower = sum(ADHD_prev_lower == 'ADHD', na.rm = T)/n(),
    higher = sum(ADHD_prev_higher == 'ADHD', na.rm = T)/n(),
    nice = sum(ADHD_prev_nice == 'ADHD', na.rm = T)/n(),
    sim_baseline_prev = sum(ADHD_baseline == 'ADHD', na.rm = T)/n()*5,
    # sim_incid= sum(!is.na(ADHD_incid), na.rm = T)/n()*5,
    sim_incid_prev = sum(!is.na(ADHD_incid_prev) , na.rm = T)/n()*5) |>

  pivot_longer(-c(year),
               names_to = 'type',
               values_to = 'prevalence') |>

  ggplot(aes(year)) +
  geom_line(aes(y=prevalence,col=type))+
  ylim(0,NA)+
  theme_minimal()
)

  # geom_line(aes(y=sicbl )) +
  # geom_line(aes(y=lower )) +
  # geom_line(aes(y=higher )) +
  # geom_line(aes(y=nice )) +
  # geom_line(aes(y=sim )) +
  # facet_wrap(~age_adhd)



# all prevalence
(
prevalent_counts_graph <- pop |>
  group_by(year) |>
  summarise(
    sicbl = sum(ADHD_prev_sicbl == 'ADHD', na.rm = T)*10,
    lower = sum(ADHD_prev_lower == 'ADHD', na.rm = T)*10,
    higher = sum(ADHD_prev_higher == 'ADHD', na.rm = T)*10,
    nice = sum(ADHD_prev_nice == 'ADHD', na.rm = T)*10,
    sim_baseline_prev = sum(ADHD_baseline == 'ADHD', na.rm = T)*5*10,
    sim_incid_prev = sum(!is.na(ADHD_incid_prev) , na.rm = T)*5*10
  ) |>
  pivot_longer(-c(year),
               names_to = 'type',
               values_to = 'prevalence') |>
  ggplot(aes(year)) +
  geom_line(aes(y=prevalence,col=type))+
  theme_minimal()
)

# prevalence from my simulation
(
prevalence_my_sim_graph <- pop |>
  filter( (death > year) | is.na(death)) |>
  # filter( (death > year) ) |>
  count(year,age_adhd, ADHD_incid_prev=!is.na(ADHD_incid_prev)) |>
  filter(ADHD_incid_prev) |>
  ggplot(aes(year, n, fill = age_adhd)) +
  geom_col( )+theme_minimal()
)
# incidence from my simulation

(
yearly_incidence_graph <- pop |>
  #filter( (death > year) |is.na(death)) |>
  count(year,age_adhd, ADHD_incid = year ==ADHD_incid ,run) |>
  filter(!is.na(ADHD_incid)) |>
  group_by(year,age_adhd, ADHD_incid) |>
  summarise(n = mean(n)) |>
  ggplot(aes(year, n*100, fill = age_adhd)) +
  geom_col( )+theme_minimal()
)

##### cumulative #####

(
  cumulative_incidence_graph <- pop |>
    #filter( (death > year) |is.na(death)) |>
    count(year,age_adhd, ADHD_incid, run) |>
    filter(!is.na(ADHD_incid)) |>
    group_by(year,age_adhd, ADHD_incid) |>
    summarise(n = mean(n)) |>
    ggplot(aes(year, n*100, fill = age_adhd)) +
    geom_col( )+theme_minimal()
)

# pop |>
#   #filter( (death > year) |is.na(death)) |>
#   count(year,age_adhd, cumulative_ADHD_incid= ADHD_incid,run) |>
#   filter(!is.na(ADHD_incid)) |>
#   group_by(year,age_adhd, ADHD_incid) |>
#   summarise(n = mean(n)) |>
#   ggplot(aes(year, n*100, fill = age_adhd)) +
#   geom_col( )

# 17 year olds turning 18 from my simulation

(
new_adult_transitions <- pop |>
  filter( age==18) |>
  filter(!is.na(ADHD_incid_prev)) |>
  count(year) |>
  ggplot(aes(year, n*10)) +
  geom_col( )+
    theme_minimal()
)

# estimated actual prevalence (*5)

new_adult_transitions <- pop |>
  filter( (death > year) | is.na(death)) |>
  # filter( (death > year) ) |>
  count(year,age_adhd, ADHD_incid_prev) |>
  filter(!is.na(ADHD_incid_prev)) |>
  ggplot(aes(year, n*10, fill = age_adhd)) +
  geom_col( )+
  theme_minimal()

# referrals from my simulation
## can take as 'incidence' as above or


(
adult_referals <- pop |>
  filter(age>=18) |>
  filter( (death > year) | is.na(death)) |>
  count(year, age_adhd, ADHD_incid_prev, run) |>
  filter(!is.na(ADHD_incid_prev)) |>
  group_by(year,age_adhd,ADHD_incid_prev) |>
  summarise(n = mean(n)) |>
  ggplot(aes(year, n*10*10*5*0.1, fill = age_adhd)) +
  geom_col( )
)
# accepted adult referrals from my simulation

(
accepted_adult_referals <- pop |>
  filter(age>=18) |>
  filter( (death > year) |is.na(death)) |>
  count(year, age_adhd, ADHD_incid_prev, run) |>
  filter(!is.na(ADHD_incid_prev)) |>
  group_by(year,age_adhd,ADHD_incid_prev) |>
  summarise(n = mean(n)) |>
  ggplot(aes(year, n*10*10*5*0.1*0.7, fill = age_adhd)) +
  geom_col( )
)

#Child referals

(
  child_referals <- pop |>
    filter(age<18) |>
    filter( (death > year) | is.na(death)) |>
    count(year, age_adhd, ADHD_incid_prev, run) |>
    filter(!is.na(ADHD_incid_prev)) |>
    group_by(year,age_adhd, ADHD_incid_prev) |>
    summarise(n = mean(n)) |>
    ggplot(aes(year, n*10*10*5*0.1, fill = age_adhd)) +
    geom_col( )
)

# accepted adult referrals from my simulation

(
  accepted_child_referals <- pop |>
    filter(age<18) |>
    filter( (death > year) |is.na(death)) |>
    count(year, age_adhd, ADHD_incid_prev, run) |>
    filter(!is.na(ADHD_incid_prev)) |>
    group_by(year,age_adhd,ADHD_incid_prev) |>
    summarise(n = mean(n)) |>
    ggplot(aes(year, n*10*10*5*0.1*0.7, fill = age_adhd)) +
    geom_col( )
)

# estimated 'real' incidence from estimated 'real' prevalence

pop |>
  count(year, ADHD, death)


pop |>
  #filter( (death > year) |is.na(death)) |>
  count(year,age_adhd, ADHD, run) |>
  filter(!is.na(ADHD)) |>
  group_by(year,age_adhd) |>
  summarise(n = mean(n)) |>
  ggplot(aes(year, n*10*10, fill = age_adhd)) +
  geom_col( )

pop |>
  #filter( (death > year) |is.na(death)) |>
  count(year,age_adhd,age, ADHD, run) |>
  filter(!is.na(ADHD)) |>
  group_by(year,age_adhd) |>
  summarise(n = mean(n),
            max(age),
            min(age),
            n_at_max_age = n[which.max(age)],
            n_at_min_age = n[which.min(age)]) |>

  # summarise(n_at_max_age = n[which.max(age)]) #|>

  group_by(age_adhd) |>
  arrange((year)) #|> View()


### change = this_year_cohort - last_year_cohort = age_in - age_out - death - incidence
## incidence = age_in - age_out - death - this_year_cohort + last_year_cohort

# prevalence from Dr O'Neil
# prevalence from nice
# prevalence from high estimate
# prevalence from low estimate
# prevalence from learning difficulties, experimental statistics

# referrals to trusts from copilot

# Plot
# prevalence_modelled_adult
# referrals_modelled_adult_child

