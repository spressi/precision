library(tidyverse)

sampling.rate = 500
threshold1 = .7
threshold2 = .5

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

myGgTheme.pip = myGgTheme + theme(axis.text.x = element_text(size=10, color="black"),
                                  axis.text.y = element_text(size=10, color="black"),
                                  axis.title = element_text(size=10, color="black"),
                                  legend.position="none")


# Read and prepare data ---------------------------------------------------
ecg = read.table("ecg.txt", sep="\t", col.names=c("min", "EDA", "ECG", "Trigger", "empty"), na.strings="", skip=15) %>% 
  select(ECG) %>% tibble() %>% 
  mutate(sample = 1:n(), time = (sample-1)/500) %>% 
  filter(time > 1820, time < 1850) %>% mutate(time = time - min(time))
  #filter(time > 1820, time < 1850+10) %>% mutate(time = time - min(time))

# check data
# ecg %>% ggplot(aes(x=time, y=ECG)) + 
#   geom_hline(yintercept = threshold1, linetype="dashed", color="blue") +
#   geom_hline(yintercept = threshold2, linetype="dashed", color="orange") +
#   geom_line() + 
#   ylab("ECG (mV)") + xlab("Time (sec)") + myGgTheme


# Primitive Peak Detection ------------------------------------------------
# this is just for a quick illustration and should not be used for regular R peak detection
peakHelper = function(df, st, en) {
  peak.index = df %>% filter(index >= st, index < en) %>% filter(ECG==max(ECG)) %>% pull(index) %>% first()
  return(df %>% filter(index==peak.index) %>% pull(time))
}
ecg.peaks = ecg %>% mutate(peak = ECG > threshold2) %>% filter(peak) %>% select(-peak) %>% mutate(index = 1:n())
peak.pos = ecg.peaks %>% pull(sample) %>% diff() %>% {. > 1} %>% which()
peaks = tibble(peak.start = c(0, peak.pos), peak.end = c(peak.pos+1, ecg.peaks %>% nrow())) %>% 
  rowwise() %>% mutate(peak.time = peakHelper(ecg.peaks, peak.start, peak.end)) %>% ungroup()
peaks = peaks[setdiff(1:nrow(peaks), 25),] #manually delete beat 25 (counted twice due to ectopic beat)
peaks = peaks %>% mutate(peak.time2 = peak.time); peaks[24, "peak.time2"] = peaks[c(23, 25), "peak.time2"] %>% colMeans()

#shorten the plot
ecg = ecg %>% filter(time > 10) %>% mutate(time = time - min(time)); peaks = peaks %>% filter(peak.time > 10, peak.time2 > 10) %>% mutate(peak.time = peak.time - 10, peak.time2 = peak.time2 - 10)
#ecg = ecg %>% filter(time > 10, time < 20) %>% mutate(time = time - min(time)); peaks = peaks %>% filter(peak.time > 10, peak.time < 20, peak.time2 > 10, peak.time2 < 20) %>% mutate(peak.time = peak.time - 10, peak.time2 = peak.time2 - 10)

peakDiff = peaks %>% filter(peak.time != peak.time2)
original = peakDiff$peak.time
interpolated = peakDiff$peak.time2

#plot of raw ECG signal
ecg.plot = ecg %>% ggplot(aes(x=time, y=ECG)) + 
  # geom_hline(yintercept = threshold1, linetype="dashed", color="blue") +
  # geom_hline(yintercept = threshold2, linetype="dashed", color="orange") +
  geom_vline(xintercept = peaks$peak.time, linetype="dashed", color="#F8766D") +
  geom_vline(xintercept = original, linetype="dashed", color="#F8766D") +
  geom_vline(xintercept = interpolated, linetype="dashed", color="#00BFC4") +
  geom_line() + 
  scale_x_continuous(expand = c(0, 0)) +
  ylab("ECG (mV)") + xlab("Time (sec)") + myGgTheme

#zoomed in plot
# peaks.zoom = peaks %>% filter(peak.time <= interpolated) %>% tail(3) %>% bind_rows(
#   peaks %>% filter(peak.time >= interpolated) %>% head(1))
peaks.zoom = peaks %>% transmute(time1 = peak.time, time2 = lead(peak.time)) %>% transmute(peak.time = rowMeans(.)) %>% 
  filter(peak.time < 10) %>% tail(2)
ecg.plot.zoom = ecg %>% filter(time > peaks.zoom$peak.time %>% min(), 
                               time < peaks.zoom$peak.time %>% max()) %>% 
                               #time < interpolated) %>% 
  ggplot(aes(x=time, y=ECG)) + geom_line() + 
  #scale_x_continuous(expand = c(0, 0)) +
  theme_void() + theme(plot.background = element_rect(fill='white', color="purple", size=2))
ecg.plot = ecg.plot + geom_rect(xmin=peaks.zoom$peak.time %>% min(), xmax=peaks.zoom$peak.time %>% max(), ymin=-1, ymax=1, color="purple", size=1, fill=NA)

# calculate IBIs & HRV ----------------------------------------------------
ibi = peaks %>% select(contains("peak.time")) %>% 
  mutate(original = peak.time %>% diff() %>%  {. * 1000} %>% c(NA, .), #first ibi exists at the second R peak => prepend NA
         interpol = peak.time2 %>% diff() %>%  {. * 1000} %>% c(NA, .),
         intervall = 1:n()) %>% 
  pivot_longer(c("original", "interpol"), values_to="ibi") %>% mutate(name = name %>% factor(levels=c("original", "interpol"))) %>% 
  mutate(peak.time = ifelse(name=="original", peak.time, peak.time2)) %>% select(-peak.time2)

ibi.plot.count = ibi %>% ggplot(aes(x=intervall, y=ibi, color=name)) +
  geom_line(size=2) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("IBI (ms)") + xlab("Intervall (#)") + myGgTheme

ibi.plot.time = ibi %>% ggplot(aes(x=peak.time, y=ibi, color=name)) + 
  geom_line(size=2) + 
  scale_x_continuous(expand = c(0, 0)) +
  ylab("IBI (ms)") + xlab("Time (sec)") + myGgTheme


hrv = ibi %>% group_by(name) %>% summarise(sdnn = ibi %>% sd(na.rm=T),
                                           avnn = ibi %>% mean(na.rm=T),
                                           rmssd = ibi %>% diff() %>% {. ^ 2} %>% mean(na.rm=T) %>% sqrt())
hrv

hrv.plot = hrv %>% ggplot(aes(x=name, y=rmssd, fill=name)) + geom_col(width=.75) + 
  ylab("RMSSD (ms)") + xlab("") + myGgTheme + theme(legend.position = "none") +
  scale_y_continuous(breaks=hrv$rmssd %>% round())

inlay = cowplot::ggdraw(ibi.plot.time + theme(legend.position="none") + scale_y_continuous(limits = c(NA, 1000))) +
  cowplot::draw_plot(hrv.plot + theme(plot.background = element_rect(fill='transparent', color=NA)), .5, .5, .5, .5) +
  cowplot::draw_plot_label("C", x=.5)
  

{cowplot::plot_grid(ecg.plot, 
                    ibi.plot.time + theme(legend.position="none") + scale_y_continuous(limits = c(NA, 1000)), 
                    ncol = 1, labels="AUTO", align="hv", axis="l") %>% 
    cowplot::ggdraw() + #prepare adding overlays
    #PQRST complex zoom
    cowplot::draw_plot(ecg.plot.zoom, 
                       x=.60, width=.25, y=.61, height=.2) +
    #Panel C overlay
    cowplot::draw_plot(hrv.plot + theme(plot.background = element_rect(fill='transparent', color=NA)), 
                       x=.55, width=.45, y=.25, height=.2) +
    cowplot::draw_plot_label("C", x=.55, y=.5, hjust=1)} %>% 
  ggsave("ECG.png", plot=., device="png", width=1920/300, height=2*1080/300, dpi=300)


# cowplot::plot_grid(ecg.plot, 
#                    ibi.plot.time + theme(legend.position="none") + scale_y_continuous(limits = c(NA, 1000)), 
#                    #cowplot::plot_grid(hrv.plot+myGgTheme.pip, ggplot(), nrow=1, rel_widths = c(.5, .5)),
#                    cowplot::plot_grid(hrv.plot, ggplot(), nrow=1, rel_widths = c(.5, .5)),
#                    rel_heights = c(1, 1, .6),
#                    ncol = 1, labels="AUTO", align="hv", axis="l") %>% 
# ggsave("ECG.png", plot=., device="png", width=1920/300, height=1080/300*2.6, dpi=300)
