install.packages("gtExtras")
install.packages('gt')
install.packages("flextable")
library(RColorBrewer)
library(gt)
library(lme4)
cc_sf <- read_sf('C:/Users/samtg/github/neighborhoods_ccdistricts/data/shapefiles/city_council')

meta_clusters <- read_csv('data/meta_clusters_with_formatted_name.csv')
meta_clusters %>%
  select(proper_name, vote_cluster) %>%
  group_by(vote_cluster) %>%
  summarize(proper_names = paste(proper_name, collapse = ", "))%>%
  gt()
bsr <- read_csv('../cc_legislation/data/bills_with_cluster_count.csv')
bsr %>%
  filter(primary_sponsor_cluster == 3 & EventItemMatterStatus == 'Enacted')%>%
  select(EventItemMatterName, percent_cluster_3) %>%
  arrange(desc(percent_cluster_3)) %>%
  slice(1:10)

bsr %>%
  select(EventItemMatterName, percent_cluster_3) %>%
  arrange(desc(percent_cluster_3)) %>%
  slice(1:10)
tf_idf<- read_csv("data/top_words_by_cluster.csv")
tf_idf
ed_sf$ed_name <- ed_sf$ElectDist
c3_results <- read_csv("../cc_election_cleaning/election_results_with_vote_sponsor_cluster_DEC04.csv")
results_map <- left_join(c3_results,ed_sf, by = 'ed_name')

ggplot(c3_results)+
  geom_sf(aes(geometry = geometry))
mode = function(){
  return(sort(-table(myData$Age))[1])
}

district_clusters <- c3_results%>%
  drop_na(district_cluster)%>%
  group_by(district,district_cluster)%>%
  summarise(cluster = mean(district_cluster))
district_clusters$CounDist <- district_clusters$district
cluster_map <- left_join(cc_sf,district_clusters, by = 'CounDist')

ggplot(data = cluster_map) +
  geom_sf(aes(fill = cluster)) +
  theme_minimal() +
  labs(title = "Clusters", 
       fill = "Cluster")

ggplot(data = cluster_map) +
  geom_sf(aes(fill = factor(cluster))) +
  scale_fill_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "orange"),
                    na.value = "gray80") +
  theme_minimal() +
  labs(title = "Clusters", 
       fill = "Cluster")

ggplot(data = cluster_map) +
  geom_sf(aes(fill = factor(cluster))) + # Treat cluster as discrete
  scale_fill_brewer(palette = "Set3", na.value = "gray80") + # Brewer palette with NA handling
  theme_minimal() +
  labs(title = "Clusters", 
       fill = "Cluster")


winners <- c3_results%>%
  filter(winner == TRUE)

c3_results %>%
  drop_na(district_cluster)%>%
  group_by(district_cluster)%>%
  summarize(
    perc_white = mean(nhw21p),
    perc_black = mean(nhb21p),
    degree = mean(cvap21bapp),
    homeowners = mean(hh21op),
    wh_tr = mean(white_transplant_ratio),
  )%>%
  gt()


demo_clusters <- read_csv("../cc_election_cleaning/district_level_demo_clusters.csv")
cluster_influence <- read_csv("../cc_election_cleaning/top_variables_with_scores_per_cluster.csv")


cluster_influence %>%
  gt() %>%
  tab_header(
    title = "Top Variables and Scores for Each Cluster",
    subtitle = "Ranked by Importance"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  )

demo_clusters%>%
  filter(district_cluster == 3)%>%
  select(matched_name,demo_cluster)%>%
  arrange(desc(demo_cluster))%>%
  gt()

winners <- new_clusters%>%
  filter(winner == TRUE)

winners %>%
  drop_na(new_cluster,white_transplant_ratio)%>%
  group_by(new_cluster)%>%
  select(district,white_transplant_ratio)%>%
  summarise(wt = mean(white_transplant_ratio))
occupation <- read_csv('../cc_election_cleaning/occupation_election_feb21.csv')
occupation <- occupation%>%
  select(ElectDist,w2mgtp,w2banp,w2cmp,w2aep,w2scip,w2cossp,w2lawp,w2edup,w2admp,
         w2docp,w2htp,w2hsp,w2ffp,w2copp,w2foodp,w2bgp,w2pservep,w2salep,
         w2oadp,w2farmp,w2conp,w2emrp,w2prodp,w2tranp,w2matp)
prog_cluster$ElectDist <- prog_cluster$ElectDist_x
prog_cluster <- left_join(prog_cluster,occupation, by = 'ElectDist')
lm1 <- lm(vote_share ~ log_income + nhb21p, data = prog_cluster, na.action = na.omit)
lm2 <- lm(vote_share ~ log_income + nhb21p + h21p, data = prog_cluster, na.action = na.omit)
lm3 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio, data = prog_cluster, na.action = na.omit)
lm4 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + hh21op + w2admp, data = prog_cluster, na.action = na.omit)
lm5 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + hh21op + w2admp + w2oadp + w2edup, data = prog_cluster, na.action = na.omit)


stargazer(lm1,lm2,lm3,lm4,lm5, type = 'html', covariate.labels=c('Log MHHI', 'NH Black Share','Hispanic Share','White Transplant Share',
                                                                 '% BA or more','Homeowner Share','Summer 2020 Noise','Avg. Noise',
                                                                 '% Retail Employees'),omit.stat=c("LL","ser","f"),out = 'new_cluster_reg.html')

scaled_cluster_three <- prog_cluster %>%
  mutate(across(c(log_income, nhb21p, h21p, white_transplant_ratio, cvap21bapp, hh21op, summer_noise_complaints, mean_noise, perc_retail), scale))

scaled_cluster_three_demo <- left_join(scaled_cluster_three,demo_clusters, by='district')
sctd0 <- scaled_cluster_three_demo%>%
  filter(demo_cluster == 0)
sctd2 <- scaled_cluster_three_demo%>%
  filter(demo_cluster == 2)
sctd3 <- scaled_cluster_three_demo%>%
  filter(demo_cluster == 3)
lm1 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + w2admp + w2oadp + w2edup , data = sctd0)
lm2 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + w2admp + w2oadp + w2edup , data = sctd2)
lm3 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + w2admp + w2oadp + w2edup , data = sctd3)
stargazer(lm1,lm2,lm3, type = 'html',covariate.labels=c('Log MHHI', 'NH Black Share','Hispanic Share','White Transplant Share',
                                                                             '% BA or more','Artists and Journalists','Office Workers','Educators'),
                                                        omit.stat=c("LL","ser","f"),out='by_cluster.html')

dsa_occ <- read_csv('data/dsa_with_occupation.csv')
tc<-dsa_occ%>%
  filter(candidate == 'Tiffany Caban')
aa<-dsa_occ%>%
  filter(candidate == 'Alexa Aviles')
mh<- dsa_occ%>%
  filter(candidate == 'Michael Hollingsworth')
tc_mod <- lm(vote_share ~ log_mhhi21 + nhw21p + cvap21bapp + w2admp + w2cmp, data = tc)
aa_mod <- lm(vote_share ~ log_mhhi21 + nhw21p + cvap21bapp + w2admp + w2cmp, data = aa)
mh_mod <- lm(vote_share ~ log_mhhi21 + nhw21p + cvap21bapp + w2admp + w2cmp, data = mh)

stargazer(tc_mod,aa_mod,mh_mod, type = 'html',covariate.labels=c('Log MHHI', 'NH White Share','BA','Arists Journalists',
                                                        'Computers'),
          omit.stat=c("LL","ser","f"),out='race_specific.html')

dsa_occ <- read_csv('data/cluster_four_with_occupation.csv')
tc<-dsa_occ%>%
  filter(candidate == 'Tiffany Caban')
aa<-dsa_occ%>%
  filter(candidate == 'Alexa Aviles')
sh<- dsa_occ%>%
  filter(candidate == 'Shahana K. Hanif')
co <- dsa_occ%>%
  filter(candidate == 'Chi A. Osse')
tc_mod <- lm(vote_share ~ log_mhhi21 + nhw21p + cvap21bapp + w2admp + w2cmp, data = tc)
aa_mod <- lm(vote_share ~ log_mhhi21 + nhw21p + cvap21bapp + w2admp + w2cmp, data = aa)
sh_mod <- lm(vote_share ~ log_mhhi21 + nhw21p + cvap21bapp + w2admp + w2cmp, data = sh)
co_mod <- lm(vote_share ~ log_mhhi21 + nhw21p + cvap21bapp + w2admp + w2cmp, data = co)

stargazer(tc_mod, aa_mod, sh_mod, co_mod,
          type = 'html',
          column.labels = c("Tiffany Caban", "Alexa Aviles", "Shahana K. Hanif", "Chi Osse"),
          covariate.labels = c('Log MHHI', 'NH White Share', 'BA', 'Artists Journalists', 'Computers'),
          omit.stat = c("LL", "ser", "f"),
          out = 'race_specific.html')

prog_cluster%>%
  select(vote_share,ed_name,winner,candidate,cvap21bapp,white_transplant_ratio,dpp20bs,log_income,w2admp)%>%
  head()