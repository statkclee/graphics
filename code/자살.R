library(tidyverse)
library(gganimate)
library(gghighlight)
library(extrafont)
extrafont::loadfonts()

# 1. 데이터 출처 --------------------------------------------------------------------------
## https://data.oecd.org/healthstat/suicide-rates.htm

suicide_dat <- read_csv("data/DP_LIVE_04012024092405729.csv", col_names=TRUE)

# korea_friends <- c("Korea", "Germany", "Spain", "Japan", "United States", "OECD countries")
korea_friends <- c("한국", "독일", "스페인", "일본", "미국", "프랑스")
# korea_friends <- c("KOR", "DEU", "ESP", "JPN", "USA", "FRA")


# 2. 데이터 정제 --------------------------------------------------------------------------
suicide_dat$LOCATION <- plyr::revalue(suicide_dat$LOCATION, c("DEU" ="독일",
                                                              "KOR" = "한국",
                                                              "ESP" = "스페인",
                                                              "USA" = "미국",
                                                              "JPN" = "일본",
                                                              "FRA" = "프랑스"))

suicide_df <- suicide_dat %>% dplyr::filter(LOCATION %in% korea_friends & SUBJECT =="TOT") %>% 
  dplyr::select(country=LOCATION, TIME, suicide=Value) %>% 
  mutate(date = ymd(paste0(TIME,"-01-01"))) %>% dplyr::select(-TIME)

dlist <- unique(suicide_df$date)

# 3. 십만명당 자살자수 시각화  --------------------------------------------------------------------------
## 3.1. 십만명당 자살자수 국제 비교 ------------------------------------------------------------------

ggplot(data=suicide_df,
       aes(x=date, y=suicide, group=country, color=country, label=country))+
  geom_line(linewidth=1.1) +
  scale_x_date(breaks=seq(dlist[1], tail(dlist,1) + years(1), "5 year"),
               date_labels="%y",limits=c(dlist[1], tail(dlist,1)+years(1)))+
  facet_wrap(~country)+
  theme_korean() +
  theme(legend.position="none", plot.caption=element_text(hjust=0,size=8),plot.subtitle=element_text(face="italic"),
        axis.text=element_text(size=7.5))+
  labs(x="",y="",title="자살자수 국제 비교",
       caption="\n 자료출처: 십만명당 자살자수 OECD 데이터, https://data.oecd.org/healthstat/suicide-rates.htm",
       subtitle="십만명당 자살자수(남녀 총합)") 

## 3.2. 한국 등 일괄비교 ---------------------------------------------

suicide_df |> 
  ggplot(aes(x = date, y = suicide, group=country, color=country, label=country))+
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.0) +
    scale_x_date(breaks=seq(dlist[1], tail(dlist,1) + years(1), "5 year"),
                 date_labels="%y",limits=c(dlist[1], tail(dlist,1)+years(5)))+
    geom_text(data=suicide_df %>% dplyr::filter(date==dlist[54]), hjust=0, nudge_x=500) +
    theme_minimal(base_family = 'NanumSquare_ac') +
    labs(x="",y="",title="자살자수 국제 비교",
         caption="\n 자료출처: 십만명당 자살자수 OECD 데이터, https://data.oecd.org/healthstat/suicide-rates.htm",
         subtitle="십만명당 자살자수(남녀 총합)") 

## 3.3. 애니메이션 ---------------------------------------------
extrafont::loadfonts()
# https://stackoverflow.com/questions/64762634/r-animating-line-plot-using-gganimate-and-geom-text

suicide_gif <- suicide_df |> 
  # dplyr::filter(date >= ymd("2010-01-01")) |> 
  ggplot(aes(x=date, y=suicide, group = country, color = country, label=country))+
    geom_point(show.legend = FALSE) +
    geom_line(linewidth = 0.7, aes(group = country)) +
    scale_color_viridis_d() +
    scale_x_date(limits = as.Date(c("1960-01-01", "2021-12-31")),
                 date_labels="%y") +
    geom_text(aes(x = date, label = country), family = 'NanumSquare_ac',
              nudge_x = -1, hjust = -0.4, show.legend=FALSE, size = 3.5) +  
    theme_minimal(base_family = 'NanumSquare_ac') +
    theme(legend.position = "none")+
    labs(x="",
         y="",
         title="자살자수 국제 비교",
         caption="\n 자료출처: 십만명당 자살자수 OECD 데이터, https://data.oecd.org/healthstat/suicide-rates.htm",
         subtitle="십만명당 자살자수(남녀 총합)")  +
  transition_reveal(date, keep_last=TRUE)   

suicide_ani <- animate(suicide_gif, duration = 10, 
                    fps = 10, width = 1400, height = 865, 
                    renderer = gifski_renderer(), res = 200, type = "cairo")

anim_save("images/suicide_hr.gif", suicide_ani)


