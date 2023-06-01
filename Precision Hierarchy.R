library(tidyverse)
library(apa)

sd.trial1 = 500 #sd of random noise on trial-level (i.e. imprecision of the measurement device)
sd.trial2 = 250 #lower sd = more precise measurement

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

set.seed(5713213)

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
                                  x.sd = sd(x),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())

sample.subjects = #imprecise measurement, many subjects, few trials
  sample.imprecise %>% filter(trial <= trial.n1) %>% 
  group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                  x.odd = x %>% odd() %>% mean(), 
                                  x.even = x %>% even() %>% mean(),
                                  x.sd = sd(x),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())

sample.trials = #imprecise measurement, few subjects, many trials
  sample.imprecise %>% filter(subject <= subject.n1) %>% 
  group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                  x.odd = x %>% odd() %>% mean(), 
                                  x.even = x %>% even() %>% mean(),
                                  x.sd = sd(x),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())

sample.measurement = #precise measurement, few subjects, few trials
  sample.precise %>% filter(subject <= subject.n1, trial <= trial.n1) %>% 
  group_by(subject) %>% summarise(x.m = mean(x), #calculate subject-level means from simulated trial-data
                                  x.odd = x %>% odd() %>% mean(), 
                                  x.even = x %>% even() %>% mean(),
                                  x.sd = sd(x),
                                  x.se = se(x),
                                  x.se.odd = x %>% odd() %>% se(),
                                  x.se.even = x %>% even() %>% se())


hierarchy = sample.baseline %>% mutate(sample = "baseline", n.trials = trial.n1) %>% 
  bind_rows(sample.subjects %>% mutate(sample = "subjects", n.trials = trial.n1)) %>%
  bind_rows(sample.trials %>% mutate(sample = "trials", n.trials = trial.n2)) %>% 
  bind_rows(sample.measurement %>% mutate(sample = "measurement", n.trials = trial.n1)) %>% 
  mutate(sample = sample %>% as_factor())

hierarchy.summary = hierarchy %>% group_by(sample) %>% 
  summarise(n.subjects = n(),
            n.trials = n.trials %>% mean(),
            M = mean(x.m),           #should be ~ m
            SD.subject = mean(x.sd), #should be ~ c(rep(sd.trial1, 3), sd.trial2) #average SD within every subject, across trials
            SE.subject = mean(x.se), #should be ~ c(rep(sd.trial1, 3), sd.trial2) / sqrt(c(trial.n1, trial.n1, trial.n2, trial.n1))
            SD.total = sd(x.m),      #should be ~ sd.subjects #but is inflated by subject-level variance
            SE.total = se(x.m),      #should be ~ sd.subjects / sqrt(c(subject.n1, subject.n2, subject.n1, subject.n1))
            #SD.between = sqrt(SD.total^2 - SD.subject^2/n.trials), #estimation adapted from Penny & Holmes (2003) #may be incorrect
            precision.group = 1 / SE.total,  
            precision.subject = 1 / SE.subject,
            reliability = cor(x.odd, x.even)) #%>% select(sample, n.subjects, n.trials, SD.subject, contains("precision"), reliability) %>% rename(precision.measurement = SD.subject)

hierarchy.summary
#more subjects increase group-level precision but not subject-level precision or reliability
#more trials & higher measurement precision carry on to increase group-level precision while boosting subject-level precision & reliability
#(but benefit on group-level depends on within-subject variability, i.e., sd.trial)
# => plot to see relative benefits

#"input" parameters
hierarchy.summary %>% pivot_longer(cols=c("n.subjects", "n.trials", "SD.subject"), names_to = "input") %>% 
  group_by(input) %>% mutate(precision = case_when(input == "SD.subject" ~ ifelse(value < quantile(value, prob=.25), "high", "low"),
                                                   value >= quantile(value, prob=.75) ~ "high",
                                                   T ~ "low")) %>% ungroup() %>% 
  ggplot(aes(x=sample, y=value, fill=precision)) + facet_wrap(vars(input), nrow=1, scales="free") +
  geom_col() + theme_bw() + scale_fill_manual(values=c("low" = "red", "high" = "darkgreen"))

#"output" parameters
scale01 = function(x) { return((x - min(x)) / (max(x) - min(x))) }
## can't add individual y-labs with facet_wrap => use cowplot
# hierarchy.summary %>% pivot_longer(cols=c("precision.group", "precision.subject", "reliability"), names_to = "output") %>% 
#   mutate(output = output %>% factor(levels=c("precision.subject", "reliability", "precision.group"))) %>% 
#   group_by(output) %>% mutate(precision = (value - min(value)) / (max(value) - min(value))) %>% ungroup() %>% 
#   ggplot(aes(x=sample, y=value, fill=precision)) + facet_wrap(vars(output), nrow=1, scales="free") +
#   geom_col() + theme_bw() + labs(x="", y="") + theme(legend.position = "none") + 
#   theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#   scale_fill_gradient(low="red", high="darkgreen") #scale_fill_gradient2(low="red", mid="yellow", high="darkgreen")
cowplot::plot_grid(hierarchy.summary %>% mutate(colorCode = scale01(precision.subject)) %>% 
                     ggplot(aes(x=sample, y=precision.subject, fill=colorCode)) + 
                     geom_col() + theme_bw() + labs(y="Subject-Level Precision (1/unit)", x="") + theme(legend.position = "none") + 
                     theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x=element_blank()) +
                     #scale_fill_viridis_c(),
                     scale_fill_gradient(low="red", high="darkgreen"),
                   hierarchy.summary %>% mutate(colorCode = scale01(reliability)) %>% 
                     ggplot(aes(x=sample, y=reliability, fill=colorCode)) + 
                     geom_col() + theme_bw() + labs(y="Reliability (Pearson's r)", x="") + theme(legend.position = "none") + 
                     theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x=element_blank()) +
                     #scale_fill_viridis_c(),
                     scale_fill_gradient(low="red", high="darkgreen"),
                   hierarchy.summary %>% mutate(colorCode = scale01(precision.group)) %>% 
                     ggplot(aes(x=sample, y=precision.group, fill=colorCode)) + 
                     geom_col() + theme_bw() + labs(y="Group-Level Precision (1/unit)", x="") + theme(legend.position = "none") + 
                     theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x=element_blank()) +
                     #scale_fill_viridis_c(),
                     scale_fill_gradient(low="red", high="darkgreen"),
                   nrow=1)
#ggsave(filename="Precision Hierarchy.png", device="png", dpi=300, units="in", width=1920/300, height = 1080/300)

#scatter plots (cf. Figure 2)
hierarchy %>% mutate(x.rank = x.m %>% rank() %>% {-.}) %>% 
  ggplot(aes(x = x.odd, y = x.even, color = x.rank)) + 
  facet_wrap(vars(sample), nrow=2) + 
  geom_errorbar(aes(ymin=x.even - x.se.even, ymax=x.even + x.se.even)) +
  geom_errorbarh(aes(xmin=x.odd - x.se.odd, xmax=x.odd + x.se.odd)) +
  geom_point() + geom_smooth(method="lm") + 
  scale_color_viridis_c() + theme_bw() + theme(legend.position = "none")
#ggsave(filename="Precision Hierarchy Scatter.png", device="png", dpi=300, units="in", width=1920/300, height = 1080/300)
