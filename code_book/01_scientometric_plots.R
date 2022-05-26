
library(pacman)
p_load(tidyverse,tidyfst)

rgb(178, 24, 43,maxColorValue = 255) -> ar5_color
rgb(33, 102, 172,maxColorValue = 255) -> ar6_color

##################################################################
# venn diagram
p_load(eulerr)

read_csv("data/bp_ar5_ar6/ut.csv") %>% 
  distinct(ar,UT) %>% 
  as.data.table()-> ar56_ut
ar56_ut %>% distinct(UT) %>% pull(UT) %>% str_c("UT = (",.,")") %>%
  str_c(collapse = " OR ") %>% writeClipboard()
ar56_ut[ar==5,"UT"] %>% 
  inner_join(ar56_ut[ar==6,"UT"])

# rgb(239,138,98,maxColorValue = 255) -> ar5_color
# rgb(103,169,207,maxColorValue = 255) -> ar6_color

eulerr_options(fills = list(fill = c(ar5_color,ar6_color)),edges = list(lty = 0))
png("figure/venn.png",res = 1000,bg = "transparent",width = 4,height = 4,units = "in")
ar56_ut %>% 
  mutate(ar = str_c("AR",ar),true = T) %>% 
  wider_dt(UT,name = "ar",value = "true") %>% 
  replace_na_dt(to = F) %>% 
  select(-UT) %>% 
  euler() %>% 
  plot(quantities = T)
dev.off()

ar56_ut %>% 
  mutate(ar = str_c("AR",ar),true = T) %>% 
  wider_dt(UT,name = "ar",value = "true") %>% 
  replace_na_dt(to = F) %>% 
  select(-UT) %>% 
  summarise_dt(AR5 = sum(AR5),AR6 = sum(AR6),overlap = sum(AR5==T & AR6==T)) -> fig1a

##################################################################

##################################################################
# parallel plot

# https://incites.help.clarivate.com/Content/Research-Areas/essential-science-indicators.htm
if(F){
  read_csv("data/ipcc_ar5ar6_esi_category.csv") %>% 
    na.omit() %>% 
    transmute(UT = `Accession Number`,esi = `Research Area`) -> ut_esi
  
  read_csv("data/ar5_ar6_ut.csv") %>% 
    distinct(ar,UT) %>% 
    inner_join(ut_esi) %>% 
    count(ar,esi) %>% 
    complete_dt(ar,esi,fill = 0) %>% 
    group_by(ar) %>% 
    summarise(esi = esi,n =n,total = sum(n),prop = n/sum(n)) %>% 
    mutate(prop_label = tidyfst::percent(prop,digits = 2)) %>% 
    arrange(ar,-prop) %>% 
    mutate(ar = str_glue("AR{ar}")) %>% 
    print(n = Inf) -> ar_esi
  
  write_csv(ar_esi,"output/ar_esi.csv")
}


read_csv("output/ar_esi.csv") -> ar_esi

ar_esi %>% 
  mutate(prop = log(prop)) %>% 
  mutate(
    esi = factor(esi,levels = ar_esi %>% filter(ar == "AR5") %>% pull(esi) %>% rev())
  ) %>% 
  ggplot(aes(ar,esi)) +
  geom_tile(aes(fill = prop),alpha = .8) +
  geom_text(aes(label = prop_label)) +
  labs(x = NULL,y = NULL) +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.ticks=element_blank(),
    axis.text = element_text(face = "bold",size = 10),
    plot.background=element_blank(),
    panel.border=element_blank(),
    panel.grid = element_blank()
  )
ggsave("figure/esi_change.png",height = 4,width = 4)

ar_esi -> fig1d
##################################################################

##################################################################
# year distribution plot

# AR5:2013;AR6:2021

p_load(ggridges)
read_csv("data/bp_ar5_ar6/Paper.csv") %>% 
  select(UT,Year) -> ut_year
read_csv("data/ar5_ar6_ut.csv") %>% 
  distinct(ar,UT) %>% 
  inner_join(ut_year) %>% 
  unique() %>% 
  mutate(ar = str_glue("AR{ar}")) -> ar_yr_dist

ar_yr_dist %>% 
  group_by(ar) %>%
  summarise(Year = median(Year)) -> grp.median
tibble(
  ar = c("AR5","AR6"),
  Year = c(2013,2021)
) -> ipcc_publish

plot_scale = 2.5
ar_yr_dist %>% 
  ggplot(aes(Year,ar,fill = ar)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,alpha = .8,scale = .9) +
  geom_text(data = grp.median,aes(label = Year,color = ar), 
            position=position_nudge(y=-0.15), size=5 ) +
  geom_point(data = ipcc_publish,aes(color = ar), 
            position=position_nudge(y=-0.04), size=2* plot_scale) +
  geom_text(data = ipcc_publish,aes(label = Year,color = ar), 
            position=position_nudge(y=-0.15), size=5) +
  labs(x = NULL,y = NULL) +
  scale_fill_manual(values = c(ar5_color,ar6_color)) +
  scale_color_manual(values = c(ar5_color,ar6_color)) +
  scale_y_discrete(expand = c(0.01, 0.2),limits = rev) +
  scale_x_continuous(expand = c(0.01, .0)) +
  theme_ridges(grid = F) +
  theme(
    legend.position = "none",
    # axis.ticks=element_blank(),
    axis.text = element_text(face = "bold",size = 10* plot_scale),
    # plot.background=element_blank(),
    # panel.border=element_blank(),
    # panel.grid = element_blank()
  )

ggsave("figure/yr_distribution.png")

ar_yr_dist %>% 
  count_dt(ar,Year) %>% 
  arrange(ar,Year) -> fig1b

##################################################################

##################################################################
# continent radar plot

p_load(countrycode,colorspace)

# https://ourworldindata.org/grapher/continents-according-to-our-world-in-data
read.csv("data/continents-according-to-our-world-in-data.csv") %>% 
  as_tibble() %>% 
  transmute(CountryNorm = Entity,Continent) -> continent_list

read_csv("data/bp_ar5_ar6/ut.csv") %>% 
  distinct(ar,UT) %>% 
  as.data.table()-> ar56_ut

read_csv("data/bp_ar5_ar6/Institute.csv") %>% 
  distinct(UT,CountryNorm) %>% 
  inner_join(ar56_ut) %>% 
  # count(ar,CountryNorm) %>% 
  mutate(continent = countrycode(CountryNorm,"country.name","continent")) %>% 
  mutate(continent2 = countrycode(CountryNorm,"country.name","region")) %>% 
  mutate(
    continent = case_when(
      CountryNorm == "Cent Afr Republ" ~ "Africa",
      CountryNorm == "Papua N Guinea" ~ "Oceania",
      CountryNorm == "Micronesia" ~ "Oceania",
      T ~ continent
    )) -> continent_info

continent_info %>% 
  left_join(continent_list) %>% 
  mutate(
    Continent = ifelse(is.na(Continent),continent,Continent)
  ) %>% 
  mutate(
    Continent = ifelse(Continent=="Americas","North America",Continent)
  ) -> continent_raw

# continent_raw %>% 
#   print(n = Inf) %>% 
#   distinct(Continent)

ar56_ut %>% 
  count(ar,name = "total") -> ar_total

continent_raw %>% 
  distinct(UT,Continent,ar) %>% 
  count(ar,Continent) -> ar_continent_n

ar_continent_n %>% 
  inner_join(ar_total) %>% 
  mutate(prop = n/total) %>% 
  arrange(ar,n) %>% 
  mutate(ar = str_glue("AR{ar}")) %>% 
  as.data.table() %>% 
  .[c(1:10,12,11)] -> for_radar
for_radar -> fig1c

p_load(fmsb)

for_radar %>% 
  select(ar,Continent,prop) %>% 
  mutate(prop = prop * 100) %>% 
  df_mat(row = ar,col = Continent,value = prop) %>% 
  rbind(rep(60,6),rep(0,6),.) %>% 
  as.data.frame()-> data

colors_in = c(ar5_color %>% adjust_transparency(.3),ar6_color %>% adjust_transparency(.3))
colors_border = c(ar5_color %>% adjust_transparency(.9),ar6_color %>% adjust_transparency(.9))


png("figure/radar.png",res = 600,bg = "transparent",width = 4,height = 4,units = "in")
plot.new()
radarchart( data  , axistype=1 , seg = 6,
            pcol=colors_border , pfcol=colors_in , 
            plwd=1 , plty=1,palcex = 2,
            cglcol="grey", cglty=1, axislabcol="black",  cglwd=0.9,
            vlcex=0.8, calcex = .5,caxislabels=seq(0,60,10),
)
legend(x=1, y=-.8, 
       legend = rownames(data[-c(1,2),]), 
       bty = "n", pch=20 , col=colors_border , 
       text.col = "black", cex=.8, pt.cex=2)
dev.off()
##################################################################

p_load(patchwork,png)

readPNG("figure/venn.png",native = T) -> imag1
readPNG("figure/esi_change.png",native = T) -> imag4
readPNG("figure/yr_distribution.png",native = T) -> imag2
readPNG("figure/radar2.png",native = T) -> imag3

wrap_elements(imag1) + wrap_elements(imag2) + wrap_elements(imag3)+ wrap_elements(imag4) +
  plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 25))
ggsave("figure/figure1_scientometrics.png",width = 10,height = 10)

p_load(openxlsx)

write.xlsx(list(fig1a=fig1a,fig1b=fig1b,fig1c=fig1c,fig1d=fig1d),"output/SF-fig1.xlsx")








