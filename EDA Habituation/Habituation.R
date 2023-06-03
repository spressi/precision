library(tidyverse)

sampling.rate = 500
trials = 160
pause = 48.5

se = function(x, na.rm = FALSE) { sd(x, na.rm) / sqrt(if(na.rm==F) length(x) else sum(is.na(x)==F)) }

eda = read.table("eda.txt", sep="\t", col.names=c("EDA", "ECG", "Trigger", "empty"), na.strings="", skip=15) %>% 
  select(EDA, Trigger) %>% tibble() %>% 
  mutate(sample = 1:n(),
         time = (sample - 1) / 500,
         trial = NA) #filled later

trialOnsets = eda %>% filter({Trigger %>% diff() > 0} %>% c(F, .)) %>% pull(sample) %>% tail(trials)
US_trials = read_tsv("ratings.csv") %>% filter(shock) %>% pull(trial)

eda = eda %>% mutate(Trigger = case_when(sample %in% trialOnsets ~ 1,
                                         sample %in% (trialOnsets[US_trials] + 5.85 * sampling.rate) ~ 2, #US application
                                         T ~ 0))
eda$trial[eda$Trigger == 1] = 1:trials; eda$trial[1] = 0; eda = eda %>% fill(trial, .direction="down")
eda = eda %>% group_by(trial) %>% mutate(trialTime = time - min(time)) %>% ungroup() %>% 
  mutate(kind = case_when(trial == 0 | (trialTime > 11) ~ "pause",
                          (trial %in% US_trials) & (trialTime >= 5.85) ~ "shock", #TODO something about this doesn't work
                          (trial-1 %in% US_trials) & (trialTime <= 5) ~ "shock", #let shocks overlap into onset of new stimuli
                          T ~ "noshock") %>% as_factor(),
         kind2 = ifelse(kind == "pause", "pause", "experiment") %>% as_factor())
#eda %>% filter(kind == "pause") %>% filter(trialTime > 11) %>% pull(trial) %>% unique()


eda.plot = eda %>% filter(#sample > min(trialOnsets) - 2 * sampling.rate, #seconds before first trial #not needed because US appears 5.85 seconds after trial onset
                          trial != 0,
                          sample < max(trialOnsets) + 11 * sampling.rate) %>% #seconds after last trial
  mutate(sample = sample - min(sample), time = time - min(time)) #set beginning of sample & time to 0
pause.us.time = eda.plot %>% filter(kind2 == "pause") %>% pull(time) %>% range() %>% mean()
eda.plot = eda.plot %>% mutate(phase = case_when(kind2 == "pause" ~ "pause",
                                                 time < pause.us.time ~ "first",
                                                 T ~ "second"))
US_start = eda.plot %>% filter(Trigger == 2) %>% mutate(trial.us = 1:n()) %>% select(trial.us, sample, time, trialTime)
eda.us.plot = eda.plot %>% full_join(US_start %>% select(trial.us, sample), by="sample") %>% 
  fill(trial.us, .direction="down") %>% filter(trial.us %>% is.na() == F) %>% 
  group_by(trial.us) %>% mutate(trialTime.us = time - min(time)) %>% ungroup() %>% 
  filter(trialTime.us <= 7)
eda.us.scl = eda.us.plot %>% filter(trialTime.us < 1) %>% 
  group_by(trial.us) %>% summarise(SCL = mean(EDA), SCL.precision.trial = 1/se(EDA))
eda.us.bl = eda.us.plot %>% full_join(eda.us.scl, by="trial.us") %>% mutate(EDA.bl = EDA - SCL)
eda.us.scr = eda.us.bl %>% group_by(trial.us) %>% summarise(SCR = max(EDA.bl)) %>% 
  full_join(eda.us.scl, by="trial.us") %>% 
  left_join(eda.us.bl %>% select(trial, trial.us) %>% unique() %>% group_by(trial.us) %>% filter(trial == min(trial)), by="trial.us")
pause.us = eda.us.scr %>% filter(trial < pause) %>% pull(trial.us) %>% max() %>% {. + .5}
eda.us.scr = eda.us.scr %>% mutate(phase = ifelse(trial.us < pause.us, "first", "second") %>% as_factor())


# Precision ---------------------------------------------------------------
eda.us.scr %>% summarise(SCR.precision.subject = 1/se(SCR), #subject-level precision of SCR
                         SCL.precision.subject = 1/se(SCL), #subject-level precision of SCL
                         SCL.precision.trial_avg = mean(SCL.precision.trial)) #AVERAGE trial-level precision of SCL
#no trial-level precision of SCR because it is not a trial-level aggregate of the data


# Plots -------------------------------------------------------------------
myGgTheme = theme_bw() + theme(
  plot.title = element_text(hjust = 0.5),
  #axis.line = element_line(color='black'),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_rect(fill="white", color="white"),
  plot.background = element_rect(fill='white'), #element_blank(), #for transparency
  legend.background = element_rect(fill="white", color=NA),
  legend.key=element_rect(fill='white'),
  axis.text = element_text(color="black"),
  axis.ticks.x = element_line(color="black"),
  axis.line.x = element_line(color="black"),
  axis.line.y = element_line(color="black"),
  legend.text = element_text(size=14, color="black"),
  legend.title = element_text(size=14, color="black"),
  strip.text.x = element_text(size=12, color="black"),
  axis.text.x = element_text(size=16, color="black"),
  axis.text.y = element_text(size=16, color="black"),
  axis.title = element_text(size=16, color="black"))

#Panel A, Option 1: conformity to other panels (incorrect: we see all trials here, not only US trials => colors are different anyway)
# eda.plot %>% 
#   ggplot(aes(x = time/60, y = EDA, color=trial)) +
#   geom_path() + myGgTheme + scale_color_viridis_c(trans = "reverse") +
#   ylab("EDA (µS)") + xlab("Experiment Time (m)")

#Panel A, Option 2: show trial kind (shock, noshock, pause)
# eda.plot %>% ggplot(aes(x = time/60, y = EDA, color=kind, group=NA)) +
#   geom_path() + myGgTheme + #theme(legend.position = "none") +
#   scale_color_manual(values=c("shock" = "red", "noshock" = "blue", "pause" = "black")) +
#   ylab("EDA (µS)") + xlab("Experiment Time (m)")

#Panel A, Option 3: show trial kind2 (experiment pause)
#eda.experiment = eda.plot %>% ggplot(aes(x = time/60, y = EDA, color=kind2, group=NA)) +
eda.experiment = eda.plot %>% filter(kind2 != "pause") %>% ggplot(aes(x = time/60, y = EDA, group=phase)) +
  geom_vline(xintercept=pause.us.time/60, color="grey") +
  geom_vline(xintercept=eda.plot %>% filter(trial %in% US_trials) %>% group_by(trial) %>% 
               summarise(USonset = min(time) + 5.85) %>% pull(USonset) %>% {. / 60}, 
             color="red", linetype="dashed") +
  geom_path() + myGgTheme + #theme(legend.position = "none") +
  scale_color_manual("Phase", values=c("experiment" = "black", "pause" = "grey")) +
  ylab("EDA (µS)") + xlab("Experiment Time (min)")
eda.experiment


#Panel C: UCRs
eda.ucr = eda.us.plot %>% ggplot(aes(x = trialTime.us, y = EDA, color=trial.us, group=trial.us)) +
  geom_vline(xintercept=0, color="red", linetype="dashed") +
  geom_path() + scale_color_viridis_c(trans = "reverse") + myGgTheme + 
  ylab("EDA (µS)") + xlab("Trial Time (sec)") + labs(color = "Trial")
eda.ucr

#Baseline-corrected UCR (not used for Figure)
eda.ucr.bl = eda.us.bl %>% ggplot(aes(x = trialTime.us, y = EDA.bl, color=trial.us, group=trial.us)) +
  geom_path() + scale_color_viridis_c(trans = "reverse") + myGgTheme +
  ylab("BL-corrected EDA (µS)") + xlab("Experiment Time (sec)") + labs(color = "Trial")
eda.ucr.bl

#Panel B: SCL
#eda.scl = eda.us.scr %>% ggplot(aes(x = trial.us, y = SCL, color=trial.us)) +
eda.scl = eda.us.scr %>% ggplot(aes(x = trial.us, y = SCL, color=trial.us, group=phase)) +
  geom_vline(xintercept=pause.us, color="grey") + #break
  geom_smooth(method="lm", color="black") + geom_point() + scale_color_viridis_c(trans = "reverse") + myGgTheme +
  ylab("SCL (µS)") + xlab("Trial") + theme(legend.position = "none") #+ labs(color = "Trial")
eda.scl

#Panel D: SCR over trials
eda.scr = eda.us.scr %>% ggplot(aes(x = trial.us, y = SCR, color=trial.us, group=phase)) +
  geom_vline(xintercept=pause.us, color="grey") + #break
  #facet_wrap(vars(phase), scales="free_x") +
  geom_smooth(method="lm", color="black") + geom_point() + scale_color_viridis_c(trans = "reverse") + myGgTheme +
  ylab("SCR (µS)") + xlab("Trial") + theme(legend.position = "none") #+ labs(color = "Trial")
eda.scr

#SCR x SCL (not used for Figure)
eda.us.scr %>% ggplot(aes(x = SCR, y = SCL, color=phase, group=phase)) +
  geom_smooth(method="lm") + geom_point() + myGgTheme


# Export ------------------------------------------------------------------
ggsave("EDA 1 experiment.png", plot=eda.experiment, device="png", width=1920/300, height=1080/300, dpi=300)
ggsave("EDA 2 UCR.png", plot=eda.ucr, device="png", width=1920/300, height=1080/300, dpi=300)
ggsave("EDA 3 UCR BL.png", plot=eda.ucr.bl, device="png", width=1920/300, height=1080/300, dpi=300)
ggsave("EDA 4 SCL.png", plot=eda.scl, device="png", width=1920/300, height=1080/300, dpi=300)
ggsave("EDA 5 SCR.png", plot=eda.scr, device="png", width=1920/300, height=1080/300, dpi=300)

cowplot::plot_grid(eda.experiment, eda.scl,
                   eda.ucr, eda.scr,
                   ncol = 2, labels="AUTO", align="hv", axis="l") %>% 
  ggsave("EDA.png", plot=., device="png", width=1920/300*2, height=1080/300*2, dpi=300)
