library(tidyverse)
library(apa)

sd.trial1 = 500 #sd of random noise on trial-level (i.e. imprecision of the measurement device)
sd.trial2 = 100 #lower sd = more precise measurement

sd.subjects = 50  #standard deviation of sample
m = 500 #arbitrary population mean (even though it may be more plausible that smaller sd is related to smaller mean)

trial.n1 = 50
trial.n2 = trial.n1 * 4 #4x the trials = 2x the subject-level precision (if no sequence effects)

subject.n1 = 64
subject.n2 = subject.n1 * 4 #4x the subjects = 2x the group-level precision

odd  = function(x) return(x[1:length(x) %% 2 != 0])
even = function(x) return(x[1:length(x) %% 2 == 0])
se = function(x, na.rm = FALSE) {
  sd(x, na.rm) / sqrt(if(na.rm==F) length(x) else sum(is.na(x)==F))
}

m.subjects = m + rnorm(subject.n2, sd=sd.subjects) #draw subject-level means with sd = sample sd
sample.imprecise = m.subjects %>% lapply(function(x) {x + rnorm(trial.n2, sd=sd.trial1)} %>% tibble(x=.)) %>% #for each subject-level mean, draw trials with sd = trial-level imprecision
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())
sample.precise = m.subjects %>% lapply(function(x) {x + rnorm(trial.n2, sd=sd.trial2)} %>% tibble(x=.)) %>% #for each subject-level mean, draw trials with sd = trial-level imprecision
  bind_rows(.id="subject") %>% group_by(subject) %>% mutate(trial = 1:n(), subject = subject %>% as.integer())


sample.baseline = #imprecise measurement, few subjects, few trials
  sample.imprecise %>% filter(subject <= subject.n1, trial <= trial.n1) %>% 
  group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                  x.odd = x %>% odd() %>% mean(), 
                                  x.even = x %>% even() %>% mean(),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())

sample.subjects = #imprecise measurement, many subjects, few trials
  sample.imprecise %>% filter(trial <= trial.n1) %>% 
  group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                  x.odd = x %>% odd() %>% mean(), 
                                  x.even = x %>% even() %>% mean(),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())

sample.trials = #imprecise measurement, few subjects, many trials
  sample.imprecise %>% filter(subject <= subject.n1) %>% 
  group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                  x.odd = x %>% odd() %>% mean(), 
                                  x.even = x %>% even() %>% mean(),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())

sample.measurement = #precise measurement, few subjects, few trials
  sample.precise %>% filter(subject <= subject.n1, trial <= trial.n1) %>% 
  group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                  x.odd = x %>% odd() %>% mean(), 
                                  x.even = x %>% even() %>% mean(),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())


hierarchy = sample.baseline %>% mutate(sample = "baseline") %>% 
  bind_rows(sample.subjects %>% mutate(sample = "subjects")) %>%
  bind_rows(sample.trials %>% mutate(sample = "trials")) %>% 
  bind_rows(sample.measurement %>% mutate(sample = "measurement")) %>% mutate(sample = sample %>% as_factor())

hierarchy %>% group_by(sample) %>% summarise(M = mean(x.m),           #should be ~ m
                                             SD = sd(x.m),            #should be ~ sd.subjects
                                             SE = se(x.m),            #should be ~ sd.subjects / sqrt(c(subject.n1, subject.n2, subject.n1, subject.n1))
                                             SE.subject = mean(x.se), #should be ~ c(rep(sd.trial1, 3), sd.trial2) / sqrt(c(trial.n1, trial.n1, trial.n2, trial.n1))
                                             precision.subject = 1 / SE.subject,
                                             precision.group = 1 / SE,  
                                             reliability = cor(x.odd, x.even))
#TODO conclusion

#visualization (cf. Figure 2)
hierarchy %>% 
  ggplot(aes(x = x.odd, y = x.even)) + 
  geom_errorbar(aes(ymin=x.even - x.se.even, ymax=x.even + x.se.even)) +
  geom_errorbarh(aes(xmin=x.odd - x.se.odd, xmax=x.odd + x.se.odd)) +
  geom_point() + geom_smooth(method="lm") + 
  facet_wrap(vars(sample), nrow=2) + theme_bw()
