library(readr)
MHSDS <- read_csv("MHSDS_historic (1).csv")
#View(MHSDS)

MHSDS <- MHSDS %>%
  filter(INDICATOR_ID == 'ADHD007') %>%
  filter(AGE_GROUP != 'Unknown') %>%
  mutate(age = factor(AGE_GROUP,
            levels = c("0 to 4" ,
                       "5 to 17",
                      "18 to 24",
                      "25+" ))) %>%
  mutate(dt = as.Date(REPORTING_PERIOD_START_DATE,format = '%d/%m/%Y') )

MHSDS %>%
  # filter(1==month(as.Date(REPORTING_PERIOD_START_DATE))) %>%
  group_by(age) %>%
  e_chart(dt) %>%
  e_line(VALUE)
  unique(MHSDS$AGE_GROUP)

  pop %>%
    mutate(age_mhsds=cut(age, breaks = c(-Inf,4,17,24,Inf),
                labels = c("0 to 4" ,
                           "5 to 17",
                           "18 to 24",
                           "25+" ))) %>%

    # filter(1==month(as.Date(REPORTING_PERIOD_START_DATE))) %>%
    count(age_mhsds,year, adhd = !is.na(ADHD_incid)) %>%
    filter(adhd ==TRUE ) %>%
    mutate(year=as.character(year))  %>%
    mutate(n=n*10) %>%
    group_by(age_mhsds) %>%
    e_chart(year) %>%
    e_line(n)


  england_pop <- read_csv("england_pop.csv")

  england_pop <- england_pop %>%
    filter(Geography == 'All - England and Wales',
       Ethnicity == 'All',
       Age_Group != 'All') %>%
    select(age1 = Age_Group, pop=Numerator)

  # count(Age_Group) %>%
  # print(n=20) %>% View()

pop_lookup <- tribble(
    ~age1, ~age2,
'Age 0 to 4',          "0 to 4",
'Age 5 to 7',          "5 to 17",
'Age 8 to 9',          "5 to 17",
'Age 10 to 14',        "5 to 17",
'Age 15',              "5 to 17",
'Age 16 to 17',        "5 to 17",
'Age 18 to 19',        "18 to 24",
'Age 20 to 24',        "18 to 24",
'Age 25 to 29',        "25+",
'Age 30 to 34',        "25+",
'Age 35 to 39',        "25+",
'Age 40 to 44',        "25+",
'Age 45 to 49',        "25+",
'Age 50 to 54',        "25+",
'Age 55 to 59',        "25+",
'Age 60 to 64',        "25+",
'Age 65 to 69',        "25+",
'Age 70 to 74',        "25+",
'Age 75 to 79',        "25+",
'Age 80 to 84',        "25+",
'Age 85 and over',     "25+")


england_pop <- england_pop %>%
  left_join(pop_lookup,by='age1') %>%
  group_by(age2) %>%
  summarise(pop = sum(as.numeric(pop))) %>%
  rename(age=age2)

MHSDS <- MHSDS |>
  left_join(england_pop) %>%
  mutate(VALUE = as.numeric(VALUE)/pop) %>%
mutate(d = as.numeric(dt - as.Date('2018-01-01')),
       value  = as.numeric(VALUE))

{
  fit <- split(MHSDS,
               f = ~AGE_GROUP
  ) %>%

    lapply(function(df){
      print(df);
      x <- glm(data = df,
               #offset = rep(200,nrow(df)),
               # value ~ sqrt(d)

               value ~ log(d)

                 # sqrt(year+1),
                 # (year-3)^2,
                 # log(year+1),
                 # exp(0.13 * dt) ,
               # offset(of +
               # year #,
               # link =
               # span = 0.4,
               # degree = 1,
               #family = poisson(link = "log")
               # offset = offset(of),
               #family=gaussian(link = power(lambda = -0.1))#'gaussian'
      )

    })

  preds <- lapply(fit,function(model){
    predict(object =  model ,newdata = tibble(dt = seq.Date('2018-01-01','2030-01-01',by = 'month'),
                                              d = as.numeric(seq.Date('2018-01-01','2030-01-01',by = 'month')-as.Date('2018-01-01')) )
            ,type='response' )
  })

  pred_df <- unlist(preds) |>
    data.frame()
  pred_df <- pred_df |>
    rownames_to_column('name')
  pred_df <- pred_df |>
    separate(name,'\\.',into = c('age','rn1'))

  pred_df <- cbind(pred_df,
  tibble(dt = seq.Date('2018-01-01','2030-01-01',by = 'month'),
         d = as.numeric(seq.Date('2018-01-01','2030-01-01',by = 'month')-as.Date('2018-01-01')))
  )

  ref_df <- pred_df |>
    rename(value = 'unlist.preds.') %>%
    # mutate(year = as.character(as.numeric(rn))) |> #+1999
    select(dt,d,age,value)

  # rbind(fit, fit2) |>
  #   summarise(mean.groups = c(age,sex))

  (
    ref_e <- ref_df %>%

      # mutate(
      #   age = factor(
      #     age,
      #     levels = c(
      #       "0-2", "3-5", "6-9", "10-16", "16-17",
      #       "18-29", "30-39", "40-49", "50+"
      #     )
      #   )
      # ) %>%
      # arrange(age) %>%
      group_by(paste(age)) |>
      # filter(as.numeric(year) > 5)  |>
      filter(dt >= as.Date('2019-01-01')) |>
      # mutate(year=as.character(as.numeric(year)+1999)) |>
      # filter(year>2003) |>#View()
      e_charts(dt) |>
      # e_scatter(value) |>
      # e_title('Incidence','per100k person-years') |>
      e_title('referrals' , subtext = 'New referrals - monthly - actual and projected') |>

      e_legend(top='10%') |> #,type='scroll'
      e_grid(top='30%')  |>
      # e_loess(name ='fitted lines',smooth = T,formula = value~year ,showSymbol = FALSE) |>
      # e_glm(name = 'fit',formula = value~year) |>
      e_line(  serie = value,itemStyle = list(opacity=0), lineStyle = list(opacity=1)) |>
      e_data( MHSDS |>
                group_by(paste(age)) %>%

                mutate(value = as.numeric(VALUE)))  |>
      e_line( value) |> #name= 'empirical',
      # e_theme('roma') |>
      # e_x_axis(max = 30, axisLabel = as.character((1:36)+1999)) |>
      e_tooltip()
  )
}


mhsds_referrals <- ref_df %>%
  mutate(year=as.numeric(format(dt,'%Y'))) %>%
  left_join(by = c('age' = 'age_group','year'),
    pop %>%
        mutate(age_group= cut(age, breaks= c(-Inf, 4,17,24,Inf),
                        labels = c("0 to 4",
                                   "5 to 17",
                                   "18 to 24",
                                   "25+"))) %>%
                          count(year,HSCT,age_group,name='ni_pop/10')
  ) %>%
  mutate(wt = `ni_pop/10`*value) %>%
  mutate(service = ifelse(age %in% c('18 to 24', '25+'),'Adult','Child')) %>%
  count(year,service,HSCT,wt =wt*10, name='referrals') %>%
  mutate(referrals = round(referrals)) %>%
  filter(!is.na(HSCT)) %>%
  pivot_wider(names_from = HSCT, values_from = referrals) %>%
  filter(!year%in%c(2018,2019,2030))


mhsds_referrals %>%
  write.csv('mhsds_referrals.csv')


adult_referrals <- pop %>%
  mutate(status= cut(age, breaks= c(-Inf,17,Inf),
                      labels = c(
                                 "0 to 17",

                                 "18+"))) %>%
  count(year,HSCT,status,name='ni_pop') %>%
  mutate(ni_pop = ni_pop*10) %>%
  filter(!is.na(HSCT)) %>%
  filter(status == '18+') %>%
  mutate(adult_referrals_01pc = 0.001*ni_pop,
         adult_referrals_02pc = 0.002*ni_pop,
         adult_referrals_03pc = 0.003*ni_pop)

adult_referrals %>%
  write.csv('adult_referrals.csv')

#   year HSCT   service value type
referrals_modelled_adult_child %>%
  select(year=Year,HSCT=Trust,Child,Adult) %>%
  pivot_longer(c(Child,Adult),names_to = 'service') %>%
  mutate(type = 'modelled')
# Year Trust  Child Adult Total
bind_rows(
referrals_modelled_adult_child %>%
    select(year=Year,HSCT=Trust,Child,Adult) %>%
    pivot_longer(c(Child,Adult),names_to = 'service') %>%
    mutate(type = 'modelled'),

adult_referrals %>%
  mutate(service = 'Adult') %>%
  select(year,HSCT,service,value = adult_referrals_01pc) %>%
  mutate(type = '.1pc referral'),

adult_referrals %>%
  mutate(service = 'Adult') %>%
  select(year,HSCT,service,value = adult_referrals_02pc) %>%
  mutate(type = '.2pc referral'),

adult_referrals %>%
  mutate(service = 'Adult') %>%
  select(year,HSCT,service,value = adult_referrals_03pc) %>%
  mutate(type = '.3pc referral'),


mhsds_referrals %>%
  pivot_longer(-c(1,2),names_to = 'HSCT') %>%
  mutate(type ='mhsds'),

incid_referrals %>%
  pivot_longer(-c(1,2),names_to = 'HSCT') %>%
  rename(service = age_kid) %>%
  mutate(type = 'incid')
) %>% count(year,type,service,wt=value,name='value') %>%
  pivot_wider(names_from = service,values_from=value) %>%
  group_by(type) %>%
  filter(!year %in%c(2020,2021,2030)) %>%
  mutate(year = as.character(year)) %>%
  e_charts(year) %>%
  e_line(Adult) %>%
  e_line(Child, lineStyle = list(type = 'dashed')) %>%
  e_title('Referrals','Adult solid and children dashed' ) %>% e_tooltip()


#
# pop %>%
#   mutate(
#     age_adhd = factor(
#       age_adhd,
#       levels = c(
#         "0-2", "3-5", "6-9", "10-16", "16-17",
#         "18-29", "30-39", "40-49", "50+"
#       )
#     )
#   ) %>%
#   filter(year == 2020) %>%
#   count(age_adhd,sex) %>%
#   mutate(n=n*10) %>%
#   group_by(paste(sex)) %>%
#   e_charts(age_adhd) %>%
#   e_bar(n) %>%
#   e_theme('roma') %>%
#   e_grid(top='20%') %>%
#   e_title('NI population demographics','By ADHD reported age bands and sex. Bote Bin widths are not equal')





  # write.csv('population.csv')
