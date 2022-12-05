library(tidyverse)
library(apa)

precision.trial = 50 #sd of random noise on trial-level
#note: in practice, precision need not be constant across subjects and/or trials

sd1 = 10  #standard deviation of 1st sample (i.e. homogenous)
sd2 = 100 #standard deviation of 2nd sample (i.e. heterogenous)
m = 500 #arbitrary population mean (even though it may be more plausible that smaller sd is related to smaller mean)

subject.n = 64
trial.n = 100

odd  = function(x) return(x[1:length(x) %% 2 != 0])
even = function(x) return(x[1:length(x) %% 2 == 0])
se = function(x, na.rm = FALSE) {
  sd(x, na.rm) / sqrt(if(na.rm==F) length(x) else sum(is.na(x)==F))
}

m.subjects1 = m + rnorm(subject.n, sd=sd1) #draw subject-level means with sd = sample sd
sample1 = m.subjects1 %>% lapply(function(x) {x + rnorm(trial.n, sd=precision.trial)} %>% tibble(x=.)) %>% #for each subject-level mean, draw trials with sd = trial-level precision.trial
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample1.m = sample1 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                        x.odd = x %>% odd() %>% mean(), 
                                                        x.even = x %>% even() %>% mean(),
                                                        x.se = se(x))

m.subjects2 = m + rnorm(subject.n, sd=sd2) #draw subject-level means with sd = sample sd
sample2 = m.subjects2 %>% lapply(function(x) {x + rnorm(trial.n, sd=precision.trial)} %>% tibble(x=.)) %>% #for each subject-level mean, draw trials with sd = trial-level precision.trial
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample2.m = sample2 %>% group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                                        x.odd = x %>% odd() %>% mean(), 
                                                        x.even = x %>% even() %>% mean(),
                                                        x.se = se(x))


sample1.m %>% with(cor.test(x.odd, x.even, alternative="greater")) %>% apa::cor_apa(r_ci=T)
sample2.m %>% with(cor.test(x.odd, x.even, alternative="greater")) %>% apa::cor_apa(r_ci=T)

#does not relate to spearman vs. pearson correlation
# sample1.m %>% with(cor.test(x.odd %>% rank(), x.even %>% rank(), alternative="greater")) %>% apa::cor_apa(r_ci=T)
# sample2.m %>% with(cor.test(x.odd %>% rank(), x.even %>% rank(), alternative="greater")) %>% apa::cor_apa(r_ci=T)

joined = sample1.m %>% mutate(sample = "homogenous") %>% bind_rows(sample2.m %>% mutate(sample = "heterogenous"))

joined %>% 
  ggplot(aes(x = x.odd, y = x.even)) + geom_point() + geom_smooth(method="lm") + 
  facet_wrap(vars(sample)) + theme_bw()

joined %>% group_by(sample) %>% summarise(precision.subject = mean(x.se), #should be ~ precision.trial / sqrt(trial.n)
                                          precision.group = se(x.m),      #should be ~ sd / sqrt(subject.n)
                                          reliability = cor.test(x.odd, x.even)$estimate)
