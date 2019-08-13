library("ggplot2")
library("plyr")

#loading data
bhs <- read.csv("bhs.csv")

#indices
index.baseline <- bhs$benchmark == "baseline"
index.noBlackhole <- bhs$benchmark == "noBlackhole"
index.valueSum <- bhs$benchmark == "valueSum"
index.valueBlackhole <- bhs$benchmark == "valueBlackhole"

#processing values
bhs$value_proc <- bhs$value / (2 ^ 14)

#sampling
baseline <- bhs[index.baseline, 3:4]
noBlackhole <- bhs[index.noBlackhole, 3:4]
valueSum <- bhs[index.valueSum, 3:4]
valueBlackhole <- bhs[index.valueBlackhole, 3:4]

sample.size <- 10000

sampled.baseline <- sample(baseline$value_proc, size = sample.size, replace = TRUE, prob = baseline$count)
sampled.noBlackhole <- sample(noBlackhole$value_proc, size = sample.size, replace = TRUE, prob = noBlackhole$count)
sampled.valueSum <- sample(valueSum$value_proc, size = sample.size, replace = TRUE, prob = valueSum$count)
sampled.valueBlackhole <- sample(valueBlackhole$value_proc, size = sample.size, replace = TRUE, prob = valueBlackhole$count)

sampled.df <- data.frame(
  benchmark = c(
    rep("Baseline", sample.size),
    rep("No Blackhole", sample.size),
    rep("Value Sum", sample.size),
    rep("Value Blackhole", sample.size)
  ),
  value = c(sampled.baseline, sampled.noBlackhole, sampled.valueSum, sampled.valueBlackhole)
)

sampled.medians <- data.frame(
  benchmark = c("Baseline", "No Blackhole", "Value Sum", "Value Blackhole"),
  value = c(
    median(sampled.baseline), 
    median(sampled.noBlackhole), 
    median(sampled.valueSum), 
    median(sampled.valueBlackhole)
  )
)

# Plots

plotBox <- ggplot(sampled.df, aes(x=benchmark, y=value, colour = benchmark)) + 
  geom_boxplot() +
  geom_text(data = sampled.medians, aes(label = round(value, 2)), nudge_y = 1) +
  ggtitle("Blackhole Sum") +
  ylab("Normalized Runtime (ns)") +
  xlab("Variant") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.position = "none"
  )

plotBar <- ggplot(sampled.medians, aes(x=benchmark, y=value, fill = benchmark)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, 2)), nudge_y = 0.9) +
  ggtitle("Blackhole Sum") +
  ylab("Normalized Runtime (ns)") +
  xlab("Variant") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.position = "none"
  ) 
