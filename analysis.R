library(fst)
library(tidyverse)
library(echarts4r)

# prevalence |>
#   mutate(year= as.character(year)) |>
#   filter(sex =='Males') |>
#   group_by(paste(age,sex)) |>
#   e_charts(year) |>
#   e_line(value)|>
#   e_title('Prevalence','Males, per10k')|>
#   e_legend(top='13%') |>
#   e_grid(top='30%')


# prevalence |>
#   mutate(year= as.character(year)) |>
#   filter(sex =='Females') |>
#   group_by(paste(age,sex)) |>
#   e_charts(year) |>
#   e_line(value)|>
#   e_title('Prevalence','per10k')|>
#   e_legend(top='13%') |>
#   e_data(prevalence |>
#            mutate(year= as.character(year)) |>
#            filter(sex =='Males') |>
#            group_by(paste(age,sex))
#   ) |>
#   e_line(value, lineStyle=list(type = 'dashed'),
#          itemStyle = list(opacity=0)) |>
#   e_grid(top='30%') |>
#   e_tooltip(trigger = 'item') |>
#   e_lm(value ~ year)


# incidence |>
#   mutate(year= as.character(year)) |>
#   filter(sex =='Males') |>
#   group_by(paste(age,sex)) |>
#   e_charts(year) |>
#   e_line(value)|>
#   e_title('Incidence','Males, per100k')|>
#   e_legend(top='13%') |>
#   e_grid(top='30%')


# fit = incidence |>
#   group_by(age,sex) %>%
#   filter(sex == 'Males',
#          age=='6-9') %>%
#
#   lm(data = .,
#      value ~ log(year) #,
#      # span = 0.4,
#      # degree = 2,
#      # family = "gaussian"
#   )
#
# predict(object =  fit,
#         newdata = tibble(year = 2000:2035)) |>
#   set_names(2000:2035)

{
  fit <- split(incidence |>
                 filter(year>=2004)  |>
                 mutate(value = str_split(value," ") |>
                          map_chr(1) |>
                          as.numeric()) |>
                 mutate(year=year-1999), f = ~age+sex) |>

    lapply(function(df){
      #print(df);
      x <- glm(data = df,
               #offset = rep(200,nrow(df)),
               value ~
                 # sqrt(year+1),
                 log(year+1),
                 #year,

               # span = 0.4,
               # degree = 1,
               #family = poisson(link = "log")
               family='gaussian'
      )

    })

  preds <- lapply(fit,function(model){
    predict(object =  model ,newdata = tibble(year = ((2000:2035)-1999 ) )
            ,type='response' )
  })

  pred_df <- unlist(preds) |>
    data.frame()
  pred_df <- pred_df |>
    rownames_to_column('name')
  pred_df <- pred_df |>
    separate(name,'\\.',into = c('age','sex','rn'))

  incid_df <- pred_df |>
    rename(value = 'unlist.preds.') |>
    mutate(year = as.character(as.numeric(rn))) |> #+1999
    select(year,age,sex,value)

  # rbind(fit, fit2) |>
  #   summarise(mean.groups = c(age,sex))

  (
      incid_e <- incid_df %>%
      mutate(
        age = factor(
          age,
          levels = c(
            "0-2", "3-5", "6-9", "10-16", "16-17",
            "18-29", "30-39", "40-49", "50+"
          )
        )
      ) %>%
      arrange(age) %>%
      group_by(paste(age,sex)) |>
      mutate(year=as.character(as.numeric(year)+1999)) |>
      # filter(year>2003) |>#View()
      e_charts(year) |>
      # e_scatter(value) |>
      # e_title('Incidence','per100k person-years') |>
      e_title(subtext = 'per100k person-years') |>

      e_legend(top='10%',type='scroll') |>
      e_grid(top='30%')  |>
      # e_loess(name ='fitted lines',smooth = T,formula = value~year ,showSymbol = FALSE) |>
      # e_glm(name = 'fit',formula = value~year) |>
      e_line(  serie = value,itemStyle = list(opacity=0), lineStyle = list(opacity=1)) |>
      e_data( incidence |>
                mutate(value = str_split(value," ") |>
                         map_chr(1) |> as.numeric()) |>
                mutate(year= as.character(year)) |>
                #filter(sex =='Males') |>
                group_by(paste(age,sex)  )) |>
      e_line( value) |> #name= 'empirical',
      # e_theme('roma') |>
      # e_x_axis(max = 30, axisLabel = as.character((1:36)+1999)) |>
      e_tooltip()
  )
  }

#Prevalence ---
{
  fit <- split(prevalence  |>
                 filter(year > 2004) |>
                 mutate(year=year-1999), f = ~age+sex) |>


    lapply(function(df){
      #print(df);
      x <- glm(data = df,
               #offset = rep(200,nrow(df)),
               value ~ sqrt(year+1) ,
               # value ~ log(year+10) ,

               # span = 0.4,
                # degree = 2,
               #family = poisson(link = "log")
               family='gaussian'

      )

    })

  preds <- lapply(fit,function(model){
    predict(object =  model ,newdata = tibble(year = ((2000:2035)-1999 ) )
            ,type='response' )
  })


  pred_df <- unlist(preds) |>
    data.frame()
  pred_df <- pred_df |>
    rownames_to_column('name')
  pred_df <- pred_df |>
    separate(name,'\\.',into = c('age','sex','rn'))

  prev_df <- pred_df |>
    rename(value = 'unlist.preds.') |>
    mutate(year = as.character(as.numeric(rn))) |> #+1999
    select(year,age,sex,value)

  (
    prev_e <- prev_df |>
      group_by(paste(sex,age)) |>
      # filter(as.numeric(year >5) |>
      mutate(year=as.character(as.numeric(year)+1999)) |>
      e_charts(year) |>
      # e_scatter(value) |>
      # e_title('Prevalence','per10k') |>
      e_title(subtext = 'per10k') |>

      e_legend(top='13%') |>
      e_grid(top='30%')  |>
      # e_loess(name ='fitted lines',smooth = T,formula = value~year ,showSymbol = FALSE) |>
      # e_glm(name = 'fit',formula = value~year) |>

      e_line( serie = value,itemStyle = list(opacity=0),lineStyle = list(opacity=0.5)) |>
      e_data( prevalence |>
                mutate(year= as.character(year)) |>
                group_by(paste(sex,age))  ) |>
      e_line(value) |>
      # e_theme('roma') |>
      # e_x_axis(max = 30, axisLabel = as.character((1:36)+1999)) |>
      e_tooltip()
  )

}

 incid_df <- incid_df |>
  mutate(incid_prob = value /1e5, .keep = 'unused') |>
  mutate(year = as.numeric(year) + 1999)

prev_df <- prev_df |>
  mutate(prev_prob = value /1e4, .keep = 'unused') |>
  mutate(year = as.numeric(`year`) + 1999)


