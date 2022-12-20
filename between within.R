library(tidyverse)
library(apa)

sd.subjects1 = 100 #low standard deviation of 1nd sample (i.e. homogenous)
sd.subjects2 = 200 #high standard deviation of 2st sample (i.e. heterogenous)

sd.trial1 = 200 #sd of random noise on trial-level (i.e. imprecision of the measurement device)
sd.trial2 = 400 #higher sd = less precise measurement

m = 500 #arbitrary population mean (even though it may be more plausible that smaller sd is related to smaller mean)
subject.n = 50
trial.n = 50

odd  = function(x) return(x[1:length(x) %% 2 != 0])
even = function(x) return(x[1:length(x) %% 2 == 0])
se = function(x, na.rm = FALSE) {
  sd(x, na.rm) / sqrt(if(na.rm==F) length(x) else sum(is.na(x)==F))
}

m.subjects1 = m + rnorm(subject.n, sd=sd.subjects1) #draw subject-level means with sd = sample sd
m.subjects2 = m + rnorm(subject.n, sd=sd.subjects2) #draw subject-level means with sd = sample sd

#for each subject-level mean, draw trials with sd = trial-level imprecision
sample11 = m.subjects1 %>% lapply(function(x) {x + rnorm(trial.n, sd=sd.trial1)} %>% tibble(x=.)) %>%
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample12 = m.subjects1 %>% lapply(function(x) {x + rnorm(trial.n, sd=sd.trial2)} %>% tibble(x=.)) %>%
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample21 = m.subjects2 %>% lapply(function(x) {x + rnorm(trial.n, sd=sd.trial1)} %>% tibble(x=.)) %>%
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample22 = m.subjects2 %>% lapply(function(x) {x + rnorm(trial.n, sd=sd.trial2)} %>% tibble(x=.)) %>%
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())

sample11.m = sample11 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                          x.odd = x %>% odd() %>% mean(), 
                                                          x.even = x %>% even() %>% mean(),
                                                          x.se = se(x),
                                                          x.se.odd = x %>% odd() %>% se(),
                                                          x.se.even = x %>% even() %>% se())
sample12.m = sample12 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                          x.odd = x %>% odd() %>% mean(), 
                                                          x.even = x %>% even() %>% mean(),
                                                          x.se = se(x),
                                                          x.se.odd = x %>% odd() %>% se(),
                                                          x.se.even = x %>% even() %>% se())
sample21.m = sample21 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                          x.odd = x %>% odd() %>% mean(), 
                                                          x.even = x %>% even() %>% mean(),
                                                          x.se = se(x),
                                                          x.se.odd = x %>% odd() %>% se(),
                                                          x.se.even = x %>% even() %>% se())
sample22.m = sample22 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                          x.odd = x %>% odd() %>% mean(), 
                                                          x.even = x %>% even() %>% mean(),
                                                          x.se = se(x),
                                                          x.se.odd = x %>% odd() %>% se(),
                                                          x.se.even = x %>% even() %>% se())


betweenWithin = sample21.m %>% mutate(between = "high", within = "low") %>% #high between first for compatibility with figure 2
  bind_rows(sample22.m %>% mutate(between = "high", within = "high")) %>% 
  bind_rows(sample11.m %>% mutate(between = "low", within = "low")) %>% 
  bind_rows(sample12.m %>% mutate(between = "low", within = "high")) %>%
  mutate(between = between %>% as_factor(), within = within %>% as_factor())

betweenWithin %>% group_by(between, within) %>% 
  summarise(M = mean(x.m),           #should be ~ m
            SD = sd(x.m),            #should be ~ sd.subjects
            SE = se(x.m),            #should be ~ sd.subjects / sqrt(c(subject.n1, subject.n2, subject.n1, subject.n1))
            SE.subject = mean(x.se), #should be ~ c(rep(sd.trial1, 3), sd.trial2) / sqrt(c(trial.n1, trial.n1, trial.n2, trial.n1))
            precision.subject = 1 / SE.subject,
            precision.group = 1 / SE,  
            reliability = cor(x.odd, x.even))
#reliability is best at high between-subject variance (i.e., sample heterogeneity) plus low within-subject variability
#it is medium, when both between- and within-subject variance are high or low
#but it is worst if samples are homogenous but intra-individual variability is high.

#visualization (cf. Figure 2)
betweenWithin %>% group_by(between, within) %>% mutate(x.rank = x.m %>% rank() %>% {-.}) %>% 
  ggplot(aes(x = x.odd, y = x.even, color = x.rank)) + 
  geom_errorbar(aes(ymin=x.even - x.se.even, ymax=x.even + x.se.even)) +
  geom_errorbarh(aes(xmin=x.odd - x.se.odd, xmax=x.odd + x.se.odd)) +
  geom_point() + geom_smooth(method="lm") + 
  facet_grid(rows=vars(between), cols=vars(within), labeller = label_both) + 
  scale_color_viridis_c() + theme_bw() + theme(legend.position = "none")
