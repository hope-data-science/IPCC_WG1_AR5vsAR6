
###############################################
# for data

library(pacman)
p_load(tidyverse,tidyfst,akc,tidytext)

merge_slash = \(df,string){
  string %>%  
    str_split(pattern = "\\/[:space:]*") %>% 
    unlist() %>% 
    data.table(Word = .) -> split_words
  df %>% 
    inner_join_dt(split_words,"Word") %>% 
    count(UT) %>% 
    filter(n==nrow(split_words)) %>% 
    select(-n)-> target_ut
  anti_table = CJ(UT=target_ut$UT,Word=split_words$Word)
  bind_table = target_ut %>% mutate(Word = string)
  df %>% anti_join(anti_table,by = c("UT","Word")) %>% 
    rbind(bind_table,fill = T)
}

fread("data/bp_ar5_ar6/Keyword.csv") %>% 
  mutate_dt(Word = tolower(Word)) %>% 
  mutate_dt(Word = str_remove(Word,"\\<.+?\\>$"))%>% 
  merge_slash("atm/ocean structure/ phenomena") %>%
  merge_slash("model evaluation/performance")%>%
  merge_slash("geographic location/entity")%>%
  merge_slash("circulation/ dynamics") %>%
  merge_slash("model evaluation/performance") %>%
  mutate_dt(Word = ifelse(Word %like% "\\)$",
                          str_extract(Word,"(?<=\\().+?(?=\\)$)"),Word)) %>% 
  distinct(Word,UT)-> au_key_info
fread("data/bp_ar5_ar6/ut.csv") -> ut_info

ut_info %>% 
  distinct_dt(ar,UT,Year) %>% 
  inner_join_dt(
    au_key_info 
  ) -> ar_keyword

ar_keyword %>% 
  distinct_dt(Word) %>% 
  mutate_dt(id = 1:.N) -> id_form

id_form %>% 
  keyword_merge(keyword = "Word") -> merged_form

id_form %>% inner_join_dt(merged_form) %>% 
  select(Word,keyword) -> map_dict

ar_keyword %>% 
  inner_join_dt(map_dict) -> ar_keyword_washed

ar_keyword_washed %>%
  count_dt(keyword) %>% 
  filter_dt(n >= 10) %>% 
  write_csv("NER/ar_top.csv")

ar_keyword_washed %>% 
  count_dt(ar,keyword) %>% 
  bind_tf_idf(keyword,ar,n) %>% 
  filter(ar==6,n >= 10,tf_idf > 0) %>% 
  # slice_max(tf_idf,n = 50) %>%
  # print(n = Inf) %>% 
  write_csv("NER/ar6_tfidf.csv")

###############################################
# for visualization
library(pacman)
p_load(tidyfst,tidyverse,tidytext,text.alignment)

fread("NER/abstract_title_out_without_overlap_v2.csv") %>%
  # fread("NER/abstract_title_out_without_overlap.csv") %>% 
  select_dt(UT,question:location) %>% 
  longer_dt(UT) %>% 
  filter_dt(value != "") %>% 
  unnest_tokens(value,value,token = str_split,pattern = ";;") %>% 
  distinct_dt() %>% 
  count_dt(value,name) %>% 
  filter_dt(value!="") %>% 
  rename_dt(keyword = value,class = name,no = n) -> dict_table_raw

dict_table_raw %>% 
  distinct_dt(keyword,.keep_all = T) %>% 
  select_dt(keyword,class) -> dict_table

fread("NER/ar6_tfidf.csv") %>% 
  select_dt(keyword,n,tf_idf) %>% 
  left_join_dt(dict_table) %>% 
  filter_dt(is.na(class)) -> no_match_words

no_match_words %>% 
  mutate_dt(
    class = c("question","question","metric",
              "method","method","method","question")
  ) -> ar6_to_add

fread("NER/ar6_tfidf.csv") %>% 
  select_dt(keyword,n,tf_idf) %>% 
  left_join_dt(dict_table) %>% 
  na.omit() %>% 
  rbind(ar6_to_add) %>% 
  arrange_dt(-tf_idf) -> ar6_res

ar6_res %>% count_dt(class)

ar6_res %>% 
  mutate_when(class=="metric",class = "method") %>% 
  mutate_when(keyword=="cmip6",class = "method") %>% 
  mutate_when(keyword=="regcm4",class = "method") %>% 
  mutate_when(keyword=="landslide",class = "question") %>% 
  mutate_when(keyword=="summer",class = "question") %>%
  mutate_when(keyword == "emergent constraint",class = "method") %>% 
  arrange(class,-tf_idf) -> ar6_res_corrected

tidyfst::fwrite(ar6_res_corrected,"output/ar6_breakthough.csv")

pacman::p_load(tidyverse,tidyfst,ggsci)
fread("output/ar6_breakthough.csv") -> ar6_res_corrected
ar6_res_corrected %>% 
  mutate_dt(
    class = rec_char(class %>% 
                       as.character(),rec = "question=Science;method=Technology")
  ) %>% 
  rename(name = keyword) %>% 
  mutate(name = ifelse(str_detect(name," "),
                       str_to_title(name),toupper(name))) %>% 
  mutate(name = str_replace_all(name,"\\/"," \\/ ") %>% 
           str_squish()) %>% 
  mutate(name = str_replace_all(name,"And","and")) %>% 
  mutate(name = str_replace_all(name,"Of","of")) %>% 
  mutate(name = str_replace(name,"HEATWAVE","Heatwave")) %>% 
  mutate(name = str_replace(name,"Statistical","Statistical")) %>% 
  # mutate(name = str_replace(name,"Atm","ATM")) %>% 
  # mutate(name = str_replace(name,"ATMo","Atmo")) %>% 
  mutate(name = str_replace_all(name,"\\/"," \\/")) %>% 
  rename(keyword = name) %>% 
  group_by(class) %>% 
  slice_max(tf_idf,n = 10) %>% 
  ungroup() %>% 
  mutate(keyword = fct_reorder(keyword,tf_idf)) %>% 
  {fwrite(.,"output/data4fig3.csv");. }%>% 
  ggplot(aes(keyword,tf_idf,label = keyword,color = class)) +
  # geom_col()  +
  geom_point(size = 3) +
  geom_segment(aes(xend = keyword,yend = 0)) +
  labs(x = NULL,y = "TF-IDF") +
  scale_color_npg() +
  guides(color = "none") +
  coord_flip() +
  facet_wrap(~class,scales = "free_y",nrow = 2) +
  theme_bw(base_size = 15)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
ggsave("figure/figure3_ar6_breakthough_dot.png",dpi = 600,height = 7,width = 7)




###########################################################
fread("NER/abstract_title_out_without_overlap.csv")[
  method %like% ";;;;","method"
] %>% pull(method)
