library(tidyverse)
library(apa)

sd.trial = 50 #sd of random noise on trial-level (i.e. imprecision of the measurement device)
#note: in practice, precision need not be constant across subjects and/or trials

sd.subjects1 = 100 #standard deviation of 1st sample (i.e. heterogenous)
sd.subjects2 = 10  #standard deviation of 2nd sample (i.e. homogenous)
m = 500 #arbitrary population mean (even though it may be more plausible that smaller sd is related to smaller mean)

subject.n = 64
trial.n = 100

odd  = function(x) return(x[1:length(x) %% 2 != 0])
even = function(x) return(x[1:length(x) %% 2 == 0])
se = function(x, na.rm = FALSE) {
  sd(x, na.rm) / sqrt(if(na.rm==F) length(x) else sum(is.na(x)==F))
}

m.subjects1 = m + rnorm(subject.n, sd=sd.subjects1) #draw subject-level means with sd = sample sd
#for each subject-level mean, draw trials with sd = trial-level imprecision
sample1 = m.subjects1 %>% lapply(function(x) {x + rnorm(trial.n, sd=sd.trial)} %>% tibble(x=.)) %>%
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample1.m = sample1 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                        x.odd = x %>% odd() %>% mean(), 
                                                        x.even = x %>% even() %>% mean(),
                                                        x.se = se(x),
                                                        x.se.odd = x %>% odd() %>% se(),
                                                        x.se.even = x %>% even() %>% se())

m.subjects2 = m + rnorm(subject.n, sd=sd.subjects2) #draw subject-level means with sd = sample sd
#for each subject-level mean, draw trials with sd = trial-level sd.trial
sample2 = m.subjects2 %>% lapply(function(x) {x + rnorm(trial.n, sd=sd.trial)} %>% tibble(x=.)) %>% 
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample2.m = sample2 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                        x.odd = x %>% odd() %>% mean(), 
                                                        x.even = x %>% even() %>% mean(),
                                                        x.se = se(x),
                                                        x.se.odd = x %>% odd() %>% se(),
                                                        x.se.even = x %>% even() %>% se())


# sample1.m %>% with(cor.test(x.odd, x.even, alternative="greater")) %>% apa::cor_apa(r_ci=T)
# sample2.m %>% with(cor.test(x.odd, x.even, alternative="greater")) %>% apa::cor_apa(r_ci=T)

#does not relate to spearman vs. pearson correlation
# sample1.m %>% with(cor.test(x.odd %>% rank(), x.even %>% rank(), alternative="greater")) %>% apa::cor_apa(r_ci=T)
# sample2.m %>% with(cor.test(x.odd %>% rank(), x.even %>% rank(), alternative="greater")) %>% apa::cor_apa(r_ci=T)

joined = sample1.m %>% mutate(sample = "heterogenous") %>% bind_rows(sample2.m %>% mutate(sample = "homogenous"))

joined %>% group_by(sample) %>% summarise(M = mean(x.m),           #should be ~ m
                                          SD = sd(x.m),            #should be ~ c(sd.subjects1, sd.subjects2)
                                          SE = se(x.m),            #should be ~ c(sd.subjects1, sd.subjects2) / sqrt(subject.n)
                                          SE.subject = mean(x.se), #should be ~ sd.trial / sqrt(trial.n)
                                          precision.subject = 1 / SE.subject,
                                          precision.group = 1 / SE,  
                                          reliability = cor(x.odd, x.even)) %>% arrange(desc(sample))
#looking at last 3 columns:
#at constant subject-level precision:
#homogenous samples optimize group-level precision at the cost of reliability
#heterogenous samples optimize reliability at the cost of group-level precision

#visualization (cf. Figure 2)
joined %>% group_by(sample) %>% mutate(x.rank = x.m %>% rank() %>% {-.}) %>% 
  ggplot(aes(x = x.odd, y = x.even, color = x.rank)) + 
  geom_errorbar(aes(ymin=x.even - x.se.even, ymax=x.even + x.se.even)) +
  geom_errorbarh(aes(xmin=x.odd - x.se.odd, xmax=x.odd + x.se.odd)) +
  geom_point() + geom_smooth(method="lm") + 
  facet_wrap(vars(sample)) + 
  scale_color_viridis_c() + theme_bw() + theme(legend.position = "none")
