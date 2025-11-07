# Health and Care of People with Learning Disabilities, Experimental Statistics 2023 to 2024
# https://digital.nhs.uk/data-and-information/publications/statistical/health-and-care-of-people-with-learning-disabilities/experimental-statistics-2023-to-2024

sicbl <- read_csv("health_care_ld_sicbl_2023-24 (1).csv")

lookup <- tribble(
  ~lrg, ~sm, '0 to 9', NA, '0 to 9', NA, '10 to 17', '10 to 13', '10 to 17', '14 to 17', '18 to 24', NA, '18 to 24', NA, '25 to 34', NA, '25 to 34', NA, '35 to 44', NA, '35 to 44', NA, '45 to 54', '45 to 49', '45 to 54', '50 to 54', '55 to 64', '55 to 59', '55 to 64', '60 to 64', '65 to 74', '65 to 69', '65 to 74', '70 to 74', '75 and over', NA, '75 and over', NA)

coverage <- read_csv("health_care_ld_SICBL_coverage_2023_24.csv")

# LDO1_REG - total registered patients 
# LDOB001 - GP registered patients in participatning GPs 

sicbl <- sicbl |> 
   filter(INDICATOR %in% c( 'LDOB089', 
                            #'LDOB001',
                            #'LDOB091',
                            #'LDOB002',
                            #'LDOB003A',
                            'LDOB003B')) |> #,'LDO1_REG'
  left_join(lookup, by = c(AGE_BAND = 'sm')) |> 
  mutate(AGE_BAND=coalesce(lrg,AGE_BAND)) |> 
  count(ind = INDICATOR,age = AGE_BAND, sex = SEX, wt = VALUE) |> 
  pivot_wider(names_from = ind,values_from = n, id_cols = c(sex,age)) |>
  mutate(prev = LDOB089 / LDOB003B*100) 


sicbl <- sicbl |> 
  mutate(sex = ifelse(sex == 'M', 
                      'Males',
                      'Females')) |> 
  select(sex, age, sicbl_prev=prev) |> 
  filter(age != 'ALL')


# sicbl |> 
#   filter(INDICATOR %in% c( 'LDOB089','LDOB091')) |> 
#   count(wt=VALUE)


# sicbl |> 
#   filter(INDICATOR == 'LDOB003A') |> 
#   count(age = AGE_BAND, sex = SEX, wt = VALUE) |> View()

