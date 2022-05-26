
library(pacman)
p_load(tidyverse,tidyfst,ggrepel)

# read_csv("data/incites_micro_info.csv",col_types = "c") %>% 
  # mutate(micro = str_extract(micro,"^[0-9.]+"))-> micro_info
read_rds("data/ar_incites_topic.rds") -> ar_topics

ar_topics %>% 
  distinct(micro,UT,.keep_all = T) %>% 
  group_by(micro) %>% 
  summarise(ref_no = n(),Year = mean(Year)) %>% 
  ungroup() %>% 
  arrange(-ref_no)  -> micro_info2


ar_topics %>% 
  count(ar,micro) %>% 
  group_by(ar) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup %>% 
  pivot_wider(id_cols = "micro",names_from = "ar",
              values_from = "prop",values_fill = 0,names_prefix = "ar") %>% 
  mutate(ar6_score = ar6/(ar5+ar6)) %>% 
  arrange(-ar6_score) %>% 
  select(micro,ar6_score) -> micro_ar6_score

micro_info2 %>% 
  inner_join(micro_ar6_score) %>% 
  filter(ref_no >= 100) %>% 
  inner_join(
    ar_topics %>% 
      distinct(micro,micro_name)
  ) %>% 
  arrange(ar6_score) %>% 
  print(n = Inf) -> micro_to_plot

micro_to_plot %>% 
  ggplot(aes(Year,ar6_score)) +
  geom_vline(aes(xintercept = mean(Year)),linetype = "dashed") +
  geom_hline(yintercept = .5,linetype = "dashed") +
  geom_point(aes(color = ref_no,size = ref_no)) + 
  geom_text_repel(aes(label = micro_name)) +
  # geom_text(aes(label = ref_no)) +
  scale_color_distiller(palette = "Spectral")+
  scale_size_continuous(range = c(2, 8))+
  scale_x_continuous(breaks = c(2013,2016),labels = c("Old","New")) +
  scale_y_continuous(breaks = c(.4,.9),labels = c("AR5","AR6")) +
  labs(x = NULL,y = NULL,color = "Reference No.") +
  guides(size = "none") +
  theme_classic() +
  theme(axis.ticks = element_blank(), 
        legend.position = c(.15,.8),
        # legend.box = "horizontal",
        # legend.background = element_blank(),
        axis.text = element_text(face = "bold",size = 15))

scale_just = .8
ggsave("figure/figure2_micro.png",width = 8*scale_just,height = 8*scale_just,dpi = 600)


micro_to_plot %>% 
  rename(micro_no = micro,
         cited_in_report = ref_no, 
         average_year = Year, ar6_tendency = ar6_score) %>% 
  transmute(micro_no,micro_name,cited_in_report,ar6_tendency,average_year) %>% 
  arrange(-cited_in_report) %>% 
  write_csv("output/micro_gt100.csv")


