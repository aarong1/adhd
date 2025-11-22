# ---- echarts4r plots: ADHD dashboard ----
# Prereqs: your 'pop' tibble exists with the columns used below.
# Packages


library(dplyr)
library(tidyr)
library(echarts4r)
library(reactable)

# main_output

source('datasets.R')
source('analysis.R')
source('simulate.R')

# source("adult_psychiatry_survey.R")

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

# pop %>% count(age>17) %>% mutate(n/sum(n))
# # A tibble: 2 × 3
# `age > 17`       n `n/sum(n)`
# <lgl>        <int>      <dbl>
#   1 FALSE       595289      0.270
# 2 TRUE       1610502      0.730

pop$prev_rate_lower <-2.5/100 *0.73 + 0.27*5/100
pop$prev_rate_higher <- 3.4/100 *0.73 + 0.27*5/100

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
    ADHD_prev_nice = coalesce(ADHD_nice_kid, ADHD_nice_adult))

pop <- pop |>
  mutate(run = sample(1:10,replace = T,size=n()))



# Helper: ensure year is a character (echarts is happiest this way)
as_year_chr <- function(df) df %>% mutate(year = as.character(year))

# Helper: apply common options to a chart
e_common <- function(e, ylab = NULL, xlab = NULL,zoom = F) {
  e %>%
    e_y_axis(min = 0, name = ylab) %>%
    e_axis_labels(x = xlab, y = ylab) %>%
    e_legend(top = 0) %>%

               e_tooltip(
                 trigger = "item",
                 # confine=T,
                 # formatter = e_tooltip_item_formatter(
                 #   #style = c("percent"),
                 #   digits = 2,
                 #   locale = NULL
                 # )
               )%>%
    { if (zoom) e_datazoom(., show = TRUE) else . } %>%
    e_grid(top='17%', left = "15%", right = "15%", bottom = "10%") %>%
    # e_theme("roma") %>%
    e_toolbox_feature("saveAsImage")
}

# 1) Prevalence rates (share)
(
  prevalence_rates_graph_e <- pop |>

    group_by(year) |>
    summarise(
      sicbl = sum(ADHD_prev_sicbl == 'ADHD', na.rm = T)/n(),
      lower = sum(ADHD_prev_lower == 'ADHD', na.rm = T)/n(),
      higher = sum(ADHD_prev_higher == 'ADHD', na.rm = T)/n(),
      nice = sum(ADHD_prev_nice == 'ADHD', na.rm = T)/n(),
      sim_baseline_prev = sum(ADHD_baseline == 'ADHD', na.rm = T)/n()*5,
      # sim_incid= sum(!is.na(ADHD_incid), na.rm = T)/n()*5,
      # sim_incid_prev = sum(!is.na(ADHD_incid_prev) , na.rm = T)/n()*5, #) |>
      sim_incid_prev = (sum(!is.na(ADHD_baseline), na.rm = TRUE) * 5  + sum(!is.na(ADHD_incid), na.rm = TRUE))/n()
    ) %>%
    pivot_longer(-c(year),
                 names_to = 'type',
                 values_to = 'prevalence') |>
    mutate(type = recode(type,
                         higher = 'Higher (3.4%)',
                         lower = 'Lower (2.5%)',
                         nice = "NICE",
                         sicbl = "Expt England stats",
                         sim_baseline_prev = "Extrapolated Prevalence",
                         sim_incid_prev = "Simulated Incidence")) %>%
    group_by(type) %>%
    as_year_chr() %>%

   e_charts(year, zlevel=0) %>%
    e_legend(top='0%') %>%
   e_line(names = as.character(1:6), prevalence, ) %>%
   e_common(ylab = "Rates", xlab = "Year") %>%
    e_tooltip(
      # confine=T,
      formatter = e_tooltip_item_formatter(
        style = c("percent"),
        digits = 1,
        locale = NULL
      )
    )
)

# 2) Prevalent counts (scaled)
(prevalent_counts_e <- pop %>%
  group_by(year) %>%
  summarise(
    sicbl = sum(ADHD_prev_sicbl == "ADHD", na.rm = TRUE) * 10,
    lower = sum(ADHD_prev_lower == "ADHD", na.rm = TRUE) * 10,
    higher = sum(ADHD_prev_higher == "ADHD", na.rm = TRUE) * 10,
    nice = sum(ADHD_prev_nice == "ADHD", na.rm = TRUE) * 10,
    sim_baseline_prev = sum(ADHD_baseline == "ADHD", na.rm = TRUE)  * 10,#* 5
    # sim_incid_prev = sum(!is.na(ADHD_incid_prev), na.rm = TRUE) * 5 *10 ,

    sim_incid_prev = sum(!is.na(ADHD_baseline), na.rm = TRUE) *10, # * 5
    sim_incid_prev_i = sum(!is.na(ADHD_incid), na.rm = TRUE)
  ) %>%
    mutate(multiplier = 5 - (0:10)/10) %>%
    # mutate(multiplier = 5 ) %>%
        mutate(sim_baseline_prev= sim_baseline_prev* multiplier) %>%
    mutate(sim_incid_prev = sim_incid_prev*multiplier + sim_incid_prev_i ) %>%
    select(-c(multiplier,sim_incid_prev_i)) %>%
  pivot_longer(-year, names_to = "type", values_to = "count") %>%
    mutate(type = recode(type,
                         higher = 'Higher (3.4)',
                         lower = 'Lower (2.5)',
                         nice = "NICE",
                         sicbl = "Expt England stats",
                         sim_baseline_prev = "Extrapolated Prevalence",
                         sim_incid_prev = "Simulated Incidence")) %>%
  as_year_chr() %>%
  group_by(type) %>%
  e_charts(year) %>%
  e_line(count) %>%
  e_common( ylab = "Count",'Year', zoom = F)
)

# 3) Prevalence from simulation (stacked by age group)

(
  prevalence_my_sim_e <- pop %>%
           mutate(
             age_adhd = factor(
               age_adhd,
               levels = c(
                 "0-2", "3-5", "6-9", "10-16", "16-17",
                 "18-29", "30-39", "40-49", "50+"
               )
             )
  ) %>%
  filter((death > year) | is.na(death)) %>%
  count(year, age_adhd, ADHD_incid_prev = !is.na(ADHD_incid_prev)) %>%
  filter(ADHD_incid_prev) %>%
    mutate(n = n*10*5) %>%
  as_year_chr() %>%
  group_by(age_adhd) %>%
  e_charts(year) %>%
  e_bar(n, stack = "total") %>%
  e_common( ylab = "Count", 'Year',zoom = F)
)


(
  prevalence_HSCT_my_sim_e <- pop  %>%
    filter((death > year) | is.na(death)) %>%
    count(year, HSCT, ADHD_incid_prev = !is.na(ADHD_incid_prev)) %>%
    filter(ADHD_incid_prev) %>%
    as_year_chr() %>%
    mutate(n = n*10*5) %>%
    group_by(HSCT) %>%
    e_charts(year) %>%
    e_bar(n, stack = "total") %>%
    e_common( ylab = "Count", 'Year',zoom = F)
)
# 4) Yearly incidence (stacked by age group)

comp <- expand.grid(year = 2020:2030,
            age_adhd=c(
              "0-2", "3-5", "6-9", "10-16", "16-17",
              "18-29", "30-39", "40-49", "50+"
            ),
            run = 1:10)

(yearly_incidence_e <- pop %>%
    mutate(
      age_adhd = factor(
        age_adhd,
        levels = c(
          "0-2", "3-5", "6-9", "10-16", "16-17",
          "18-29", "30-39", "40-49", "50+"
        )
      ),
      age_kid = ifelse(age<18,'Child','Adult')) %>%
  count(year,age_adhd,wt = incid_prob) %>%
  mutate(n = round(n * 10)) %>%
  as_year_chr() %>%
  group_by(age_adhd) %>%
  e_charts(year) %>%
  e_bar(n, stack = "total") %>%
  e_common("Year", ylab = "Incidence", zoom = F)
)

(
  yearly_incidence_e <- pop %>%
    mutate(
      age_adhd = factor(
        age_adhd,
        levels = c(
          "0-2", "3-5", "6-9", "10-16", "16-17",
          "18-29", "30-39", "40-49", "50+"
        )
      ),
      age_kid = ifelse(age<18,'Child','Adult')) %>% #count(year)
    count(year,age_kid,wt = incid_prob) %>%
    mutate(n = round(n * 10)) %>%
    as_year_chr() %>%
    group_by(age_kid) %>%
    e_charts(year) %>%
    e_bar(n, stack = "total") %>%
    e_common("Year", ylab = "Incidence", zoom = F)
)

16000/60*1.9*12
# [1] 6840

1500000*0.001
# [1]1400

incid_referrals <- pop %>%
  mutate(
    age_adhd = factor(
      age_adhd,
      levels = c(
        "0-2", "3-5", "6-9", "10-16", "16-17",
        "18-29", "30-39", "40-49", "50+"
      )
    ),
    age_kid = ifelse(age<18,'Child','Adult')) %>%
  count(year,age_kid,HSCT,wt = incid_prob) %>%
  mutate(n = round(n * 10 )) %>%
  pivot_wider(names_from = HSCT,values_from = n) %>%
# arrange(desc(year)) #%>%
  arrange(desc(age_kid))

incid_referrals %>%
  write.csv('incid_referrrals.csv')

# (
#   yearly_incidence_e <- pop %>%
#   mutate(
#     age_adhd = factor(
#       age_adhd,
#       levels = c(
#         "0-2", "3-5", "6-9", "10-16", "16-17",
#         "18-29", "30-39", "40-49", "50+"
#       )
#     )
#   ) %>% #count(year, wt = incid_prob)
#   count(year, age_adhd,  ADHD_incid = (year == ADHD_incid)) %>%
#   filter(T == ADHD_incid) %>%
#     right_join(comp) %>% #filter(is.na(n))
#     replace_na(list(n=0)) %>%
#   group_by(year, age_adhd) %>%
#   summarise(n = mean(n), .groups = "drop") %>%
#   mutate(n = n * 100) %>%
#   as_year_chr() %>%
#   group_by(age_adhd) %>%
#   e_charts(year) %>%
#   e_bar(n, stack = "total") %>%
#   e_common("Year", ylab = "Incidence", zoom = F)
# )

# 5) "Cumulative" incidence (as currently computed in your code)

(
  cumulative_incidence_e <- pop %>%
  mutate(
    age_adhd = factor(
      age_adhd,
      levels = c(
        "0-2", "3-5", "6-9", "10-16", "16-17",
        "18-29", "30-39", "40-49", "50+"
      )
    )
  ) %>%
    filter( (death > year) |is.na(death)) |>
    count(year,age_adhd ,ADHD_incid = !is.na( ADHD_incid)) |>
    filter(T==ADHD_incid) |>
    group_by(year,age_adhd) |>
    summarise(n = mean(n)*100) %>%
    # mutate(n = n*10) %>%
    as_year_chr() %>%
    group_by(age_adhd) %>%
    e_charts(year) %>%
    e_bar(n, stack = "total") %>%
    e_common("Year", ylab = "Incidence", zoom = F)
)

# 6) New adult transitions (turning 18 with ADHD_incid_prev)
com1p = expand.grid(year = 2020:2030,
                   run = 1:10)

(
new_adult_transitions_e <- pop %>%
  filter(age == 18, !is.na(ADHD_incid_prev)) %>%
  count(year,run) %>%
  right_join(com1p) %>% #filter(is.na(n))
  replace_na(list(n=0)) %>%
    group_by(year) %>%
    summarise(n=mean(n)) %>%
  mutate(n = n * 100) %>%
  as_year_chr() %>%
  e_charts(year) %>%
  e_bar(name = '18 Year Olds w/ ADHD',n) %>%
  e_common("Year", ylab = "Count", zoom = F)
)

pop %>%
  filter(age == 18, !is.na(ADHD_incid_prev)) %>%
  count(year,run) %>%
  right_join(com1p) %>% #filter(is.na(n))
  replace_na(list(n=0)) %>%
  group_by(year) %>%
  summarise(n=mean(n)) %>%
  mutate(n = n * 100)

# 7) Adult referrals (modelled, ≥18)
adult_referrals_e <- pop %>%
  filter(age >= 18, (death > year) | is.na(death)) %>%
  count(year, age_adhd, ADHD_incid_prev = !is.na(ADHD_incid_prev), run) %>%
  # filter(!is.na(ADHD_incid_prev)) %>%
  filter(T==ADHD_incid_prev) %>%
  group_by(year, age_adhd, ADHD_incid_prev) %>%
  summarise(n = mean(n), .groups = "drop") %>%
  mutate(n = n * 10 * 10 * 5 * 0.1) %>%
  as_year_chr() %>%
  group_by(age_adhd) %>%
  e_charts(year, zlevel = 0) %>%
  e_bar(n, stack = "total") %>%
  e_common("Year", ylab = "Referrals", zoom = F)

adult_referrals_tbl <- pop %>%
  filter(age >= 18, (death > year) | is.na(death)) %>%
  count(year, HSCT, ADHD_incid_prev = !is.na(ADHD_incid_prev), run) %>%
  # filter(!is.na(ADHD_incid_prev)) %>%
  filter(T==ADHD_incid_prev) %>%

  group_by(year, HSCT) %>%
  summarise(n = mean(n), .groups = "drop") %>%
  mutate(n = n * 10 * 10 * 5 * 0.1)

reactable(select(adult_referrals_tbl,Year = year, HSCT, Count = n))

# 8) Accepted adult referrals (70% acceptance, ≥18)
accepted_adult_referrals_e <- pop %>%
  filter(age >= 18, (death > year) | is.na(death)) %>%
  count(year, age_adhd, ADHD_incid_prev, run) %>%
  filter(!is.na(ADHD_incid_prev)) %>%
  group_by(year, age_adhd, ADHD_incid_prev) %>%
  summarise(n = mean(n), .groups = "drop") %>%
  mutate(n = n * 10 * 10 * 5 * 0.1 * 0.7) %>%
  as_year_chr() %>%
  group_by(age_adhd) %>%
  e_charts(year) %>%
  e_bar(n, stack = "total") %>%
  e_common("Year", ylab = "Accepted referrals", zoom = F)

# 9) Child referrals (modelled, <18)
child_referrals_e <- pop %>%
  filter(age < 18, (death > year) | is.na(death)) %>%
  count(year, age_adhd, ADHD_incid_prev, run) %>%
  filter(!is.na(ADHD_incid_prev)) %>%
  group_by(year, ADHD_incid_prev, age_adhd) %>%
  summarise(n = mean(n), .groups = "drop") %>%
  mutate(n = n * 10 * 10 * 5 * 0.1) %>%
  as_year_chr() %>%
  group_by(age_adhd) %>%
  e_charts(year) %>%
  e_bar(n, stack = "total") %>%
  e_common("Year", ylab = "Referrals", zoom = F)

# 10) Accepted child referrals (70% acceptance, <18)
accepted_child_referrals_e <- pop %>%
  filter(age < 18, (death > year) | is.na(death)) %>%
  count(year, age_adhd, ADHD_incid_prev, run) %>%
  filter(!is.na(ADHD_incid_prev)) %>%
  group_by(year, age_adhd, ADHD_incid_prev) %>%
  summarise(n = mean(n), .groups = "drop") %>%
  mutate(n = n * 10 * 10 * 5 * 0.1 * 0.7) %>%
  as_year_chr() %>%
  group_by(age_adhd) %>%
  e_charts(year) %>%
  e_bar(n, stack = "total") %>%
  e_common("Year", ylab = "Accepted Referrals", zoom = F)

############################################################
############################################################
# 11) n at max/min age (lines per age_adhd for n_at_max_age)

#
# library(dplyr)
# library(forcats)
#
# # 0) Choose and freeze age-bin order (make sure this matches your bins)
# # age_levels <- c("0-2","3-5","6-9","10-15","16-17","18-29","30-39","40-49","50+")
# prev_bin <- function(b) {
#   # map each bin to the immediately younger bin (or NA for first)
#   idx <- match(b, age_levels)
#   out <- ifelse(is.na(idx) | idx == 1, NA_integer_, idx - 1L)
#   factor(age_levels[out], levels = age_levels)
# }
#
# # 1) Define your "real" prevalence flag (pick one)
# pop <- pop %>%
#   mutate(
#     age_adhd = factor(age_adhd, levels = age_levels),
#     # Choose ONE of your prevalence sources; here I use ADHD (your "estimated real" stock)
#     prev_flag = as.integer(!is.na(ADHD_incid_prev))     # or ADHD_prev_nice == "ADHD", etc.
#   )
#
# # 2) Create lagged (t-1) values per person/run
# pop_lag <- pop %>%
#   arrange(run, id, year) %>%
#   group_by(run, id) %>%
#   mutate(
#     prev_flag_lag = dplyr::lag(prev_flag),
#     age_bin_lag   = dplyr::lag(age_adhd),
#     year_lag      = dplyr::lag(year)
#   ) %>%
#   ungroup()
#
# # 3) Tidy helper columns: who "stayed" in the same bin with prior prevalence,
# #    and who "aged in" from the immediately younger bin with prior prevalence
# pop_flow <- pop_lag %>%
#   mutate(
#     stayed_from_last_year  = as.integer(prev_flag_lag == 1 & age_bin_lag == age_adhd & year_lag == (year - 1)),
#     aged_in_from_younger   = as.integer(prev_flag_lag == 1 & age_bin_lag == prev_bin(as.character(age_adhd)) & year_lag == (year - 1))
#   )
#
# # 4) Aggregate by year × age bin (optionally averaged across runs)
# inc_from_prev <- pop_flow %>%
#   group_by(year, age_adhd, run) %>%
#   summarise(
#     prevalent_t      = sum(prev_flag == 1, na.rm = TRUE),
#     stayed_t         = sum(stayed_from_last_year, na.rm = TRUE),
#     aged_in_t        = sum(aged_in_from_younger, na.rm = TRUE),
#     incidence_t      = prevalent_t - stayed_t - aged_in_t,
#     pop_t_bin        = n(),                           # total persons in bin (denominator option 1)
#     at_risk_t_bin    = sum((prev_flag_lag != 1) | is.na(prev_flag_lag), na.rm = TRUE), # denom option 2
#     .groups = "drop_last"
#   ) %>%
#   ungroup() %>%
#   # Average across stochastic runs (if you want)
#   group_by(year, age_adhd) %>%
#   summarise(
#     prevalent_t   = mean(prevalent_t),
#     stayed_t      = mean(stayed_t),
#     aged_in_t     = mean(aged_in_t),
#     incidence_t   = mean(incidence_t),
#     pop_t_bin     = mean(pop_t_bin),
#     at_risk_t_bin = mean(at_risk_t_bin),
#     .groups = "drop"
#   ) %>%
#   # incidence rates (pick a denominator)
#   mutate(
#     inc_rate_per_pop   = incidence_t / pmax(pop_t_bin, 1),
#     inc_rate_per_risk  = incidence_t / pmax(at_risk_t_bin, 1)
#   )
#
# # ---- Plot with echarts4r (ordered legend/stack by age bins) ----
# library(echarts4r)
#
# infered_incidence_plot <- inc_from_prev %>%
#   filter(!is.na(age_adhd)) %>%
#   mutate(year = as.character(year),
#          incidence_t =incidence_t*10 ) %>%
#   group_by(age_adhd) %>%
#   e_charts(year) %>%
#   e_bar(incidence_t,stack = 'total') %>%
#   e_y_axis(min = 0, name = "Incidence (count)") %>%
#   e_axis_labels(x = "Year", y = "Incidence") %>%
#   e_tooltip(trigger = "axis") %>%
#   e_legend(top = '5%') %>%
#   e_datazoom(show = F) %>%
#   e_grid(top='10%', left = "5%", right = "5%", bottom = "10%") %>%
#   e_theme("walden") %>%
#   e_toolbox_feature("saveAsImage") %>%
#   e_title("Incidence derived from prevalence flows (by age bin)")
#
# # ---- end of script ----
#


save(prevalence_rates_graph_e,
     prev_e,
     incid_e,
                prevalent_counts_e,
                prevalence_my_sim_e,
                prevalence_HSCT_my_sim_e,
                yearly_incidence_e,
                cumulative_incidence_e,
                new_adult_transitions_e,
                adult_referrals_e,
                adult_referrals_tbl,
                accepted_adult_referrals_e,
                child_referrals_e,
                accepted_child_referrals_e,file = 'deploy.RData'
      )
#rm(list=ls())
 # load('deploy.RData')

