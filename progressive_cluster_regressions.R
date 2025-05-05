library(tidyverse)
library(lme4)
library(ggrepel) 
library(ggokabeito) 
library(broom.mixed)
library(arm) 
library(tibble)

theme_clean <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}




occupation <- read_csv('../cc_election_cleaning/occupation_election_feb21.csv')
prog_cluster <- new_clusters%>%
  filter(new_cluster == 4)
nc_winners <- new_clusters%>%
  filter(winner == TRUE)
occupation <- occupation%>%
  select(ElectDist,w2mgtp,w2banp,w2cmp,w2aep,w2scip,w2cossp,w2lawp,w2edup,w2admp,
         w2docp,w2htp,w2hsp,w2ffp,w2copp,w2foodp,w2bgp,w2pservep,w2salep,
         w2oadp,w2farmp,w2conp,w2emrp,w2prodp,w2tranp,w2matp)
prog_cluster$ElectDist <- prog_cluster$ElectDist_x
prog_cluster <- left_join(prog_cluster,occupation, by = 'ElectDist')

prog_cluster<-prog_cluster%>%
  filter(candidate %in% c('Tiffany Caban','Alexa Aviles','Chi A. Osse','Jennifer Gutierrez','Pierina Ana Sanchez','Shahana K. Hanif','Lincoln Restler',
                          'Amanda Farias','Sandy Nurse','Julie Won'))

prog_cluster <- prog_cluster %>%
  mutate(log_income = ifelse(mhhi21 > 0, log(mhhi21), NA))

lm1 <- lm(vote_share ~ log_income + nhb21p, data = prog_cluster, na.action = na.omit)
lm2 <- lm(vote_share ~ log_income + nhb21p + h21p, data = prog_cluster, na.action = na.omit)
lm3 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio, data = prog_cluster, na.action = na.omit)
lm4 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + hh21op + w2admp, data = prog_cluster, na.action = na.omit)
lm5 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + hh21op + w2admp + w2oadp + w2edup + w2pservep, data = prog_cluster, na.action = na.omit)


stargazer(lm1,lm2,lm3,lm4,lm5, type = 'html', covariate.labels=c('Log MHHI', 'NH Black Share','Hispanic Share',
                                                                 'White Transplant Share',
                                                                 '% BA or more','Homeowner Share','Journos/Artists',
                                                                 'Administrative',
                                                                 'Educators'),omit.stat=c("LL","ser","f"),
          out = 'new_cluster_reg.html')

## FE

df4 <- prog_cluster %>%
  mutate(candidate = as.factor(candidate))

m_hier <- lmer(
  vote_share ~ 
    cvap21bapp +
    white_transplant_ratio +
    nhb21p +
    h21p +
    dpp20bs +
    log_income +
    w2admp +
    hh21op +
    w2edup +
    w2oadp +
    (1 + w2admp + log_income  | candidate),
  data = df4,
  REML = TRUE
)
summary(m_hier)
VarCorr(m_hier)

tidy(m_hier, effects = "ran_pars", conf.int = TRUE)

# plot the model

fixed_effects <- tidy(m_hier, effects = "fixed", conf.int = TRUE)

# Clean variable names (optional)
fixed_effects <- fixed_effects %>%
  filter(term != "(Intercept)") %>%  # optional: remove intercept
  mutate(term = recode(term,
                       "cvap21bapp" = "BA+ %",
                       "white_transplant_ratio" = "White Transplant Ratio",
                       "nhb21p" = "Non-Hispanic Black %",
                       "h21p" = "Hispanic %",
                       "dpp20bs" = "Bernie Primary %",
                       "log_income" = "Log Median Income",
                       "w2admp" = "% Office Clerks",
                       "hh21op" = "% Homeowner",
                       "w2edup" = "% Educators",
                       "w2oadp" = "% Journalists and Artists"
  ))

# Plot
ggplot(fixed_effects, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Estimated Effect on Vote Share",
    y = NULL,
    title = "Fixed Effects with 95% Confidence Intervals"
  ) +
  theme_minimal(base_size = 13)

#re
re <- ranef(m_hier, condVar = TRUE)

# Extract candidate-level effects
re_candidate <- re$candidate
se_candidate <- se.ranef(m_hier)$candidate

# Combine into a data frame
ranef_df <- as_tibble(re_candidate, rownames = "candidate") %>%
  rename(
    intercept = `(Intercept)`,
    slope_cvap = log_income,  
    slope_admin = w2admp      
  ) %>%
  mutate(
    se_intercept = se_candidate[, "(Intercept)"],
    se_slope_cvap = se_candidate[, "log_income"],
    se_slope_admin = se_candidate[, 'w2admp']
  )

ggplot(ranef_df, aes(x = reorder(candidate, slope_admin), y = slope_admin)) +
  geom_point() +
  geom_errorbar(aes(ymin = slope_admin - 1.96 * se_slope_admin,
                    ymax = slope_admin + 1.96 * se_slope_admin),
                width = 0.2) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Effect of Education",
    title = "Variation in Candidate Responsiveness to BA+"
  ) +
  theme_minimal(base_size = 13)

# plot the data (not the model)
df4<-df4%>%
mutate(highlight = candidate %in% prog_cluster$candidate)

n_cand <- n_distinct(df4$candidate)  
pal    <- brewer.pal(min(n_cand, 12), "Paired")  

label_data <- df4 %>%
  filter(highlight) %>%
  group_by(candidate) %>%
  slice_min(order_by = cvap21bapp, n = 1, with_ties = FALSE) %>%
  ungroup()

ggplot(df4,
       aes(x     = white_transplant_ratio,
           y     = vote_share,
           color = candidate)
) +
  geom_smooth(method = 'lm',
              se = FALSE,
              aes(size = highlight), alpha = 0.8) +
  geom_label_repel(
    data        = label_data,
    aes(label   = candidate),
    direction   = "y",
    size        = 3,
    seed        = 1234,
    show.legend = FALSE
  ) +
  scale_size_manual(
    values = c(`FALSE` = 0.1, `TRUE` = 1),
    guide  = "none"
  ) +
  scale_color_manual(values = pal) +
  labs(
    x     = "white_transplant_ratio",
    y     = "Vote Share",
    color = "Candidate"
  ) +
theme_minimal()+
  theme(legend.position = "none")

glimpse(df4)


# Now let's look at how these variables act on each demographic cluster. First
# OLS to recreate the analysis in the second chapter, then the FE and RE


prog_demo<- left_join(prog_cluster,demo_clusters, by='district')
sctd0 <- prog_demo%>%
  filter(demo_cluster == 0)
sctd2 <- prog_demo%>%
  filter(demo_cluster == 2)
sctd3 <- prog_demo%>%
  filter(demo_cluster == 3)
lm1 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + w2admp + w2oadp + w2edup , data = sctd0)
lm2 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + w2admp + w2oadp + w2edup , data = sctd2)
lm3 <- lm(vote_share ~ log_income + nhb21p + h21p + white_transplant_ratio + cvap21bapp + w2admp + w2oadp + w2edup , data = sctd3)
stargazer(lm1,lm2,lm3, type = 'html',covariate.labels=c('Log MHHI', 'NH Black Share','Hispanic Share','White Transplant Share',
                                                        '% BA or more','Artists and Journalists','Office Workers','Educators'),
          omit.stat=c("LL","ser","f"),out='by_cluster.html')

ggplot(prog_demo,aes(x =  log_income, y = vote_share, color = as.factor(demo_cluster)))+
  geom_smooth(method = "lm",se=FALSE) 
prog_demo$bus_ratio
m_hier <- lmer(
  vote_share ~ 
    cvap21bapp +
    white_transplant_ratio +
    nhb21p +
    h21p +
    dpp20bs +
    garcia213p +
    bus_ratio +
    log_income +
    w2admp +
    hh21op +
    w2edup +
    w2oadp +
    (1 +  w2admp + white_transplant_ratio| demo_cluster),
  data = prog_demo,
  REML = TRUE
)


# plot the fixed effects
fixed_effects <- tidy(m_hier, effects = "fixed", conf.int = TRUE)

fixed_effects <- fixed_effects %>%
  filter(term != "(Intercept)") %>% 
  mutate(term = recode(term,
                       "cvap21bapp" = "BA+",
                       "white_transplant_ratio" = "White Transplant Ratio",
                       "nhb21p" = "Non-Hispanic Black %",
                       "h21p" = "Hispanic %",
                       "dpp20bs" = "Bernie 2020 Primary Share",
                       "log_income" = "Log Median Income",
                       "w2admp" = "Designers and Journalists",
                       "hh21op" = "Homeowner",
                       "w2edup" = "Educators",
                       "w2oadp" = "Office and Admin. Support"
  ))

ggplot(fixed_effects, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Estimated Effect on Vote Share",
    y = NULL,
    title = "Fixed Effects with 95% Confidence Intervals"
  ) +
  theme_minimal(base_size = 13)
# plot the random effects
re <- ranef(m_hier, condVar = TRUE)

# Extract candidate-level effects
re_candidate <- re$demo_cluster
se_candidate <- se.ranef(m_hier)$demo_cluster

# Combine into a data frame
ranef_df <- as_tibble(re_candidate, rownames = "candidate") %>%
  rename(
    intercept = `(Intercept)`,
    slope_admin = w2admp,
    slope_wtr = white_transplant_ratio
  ) %>%
  mutate(
    se_intercept = se_candidate[, "(Intercept)"],
    se_slope_admin = se_candidate[, 'w2admp'],
    se_slope_wtr = se_candidate[, 'white_transplant_ratio']
  )


## plot random intercepts 

ggplot(ranef_df, aes(x = reorder(candidate, slope_admin), y = slope_admin)) +
  geom_point() +
  geom_errorbar(aes(ymin = slope_admin - 1.96 * se_slope_admin,
                    ymax = slope_admin + 1.96 * se_slope_admin),
                width = 0.2) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Effect of Education",
    title = "Variation in Candidate Responsiveness to BA+"
  ) +
  theme_minimal(base_size = 13)

# same with color

ggplot(ranef_df, aes(x = reorder(candidate, slope_admin), y = slope_admin)) +
  geom_errorbar(
    aes(ymin = slope_admin - 1.96 * se_slope_admin,
        ymax = slope_admin + 1.96 * se_slope_admin),
    width = 0.2,
    color = "#2C3E50",
    alpha = 0.6
  ) +
  geom_point(
    color = "#E74C3C",  # a strong red for points
    size = 3
  ) +
  coord_flip() +
  scale_x_discrete(labels = function(x) paste("Cluster", x)) +
  labs(
    x = NULL,
    y = "Effect of Education (BA+)",
    title = "Variation in Candidate Responsiveness to BA+ Voters",
    caption = "Points are candidate-specific slopes; error bars are 95% confidence intervals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    panel.grid.minor = element_blank()
  )
  
getAnywhere(select)
