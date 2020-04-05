library("ggplot2")
library("plyr")

#loading data
multiseq <- read.csv("multiseq.csv")

#indices
index.baseline10 <- multiseq$benchmark == "baseline10"
index.baseline12 <- multiseq$benchmark == "baseline12"
index.baseline14 <- multiseq$benchmark == "baseline14"
index.baseline16 <- multiseq$benchmark == "baseline16"
index.baseline18 <- multiseq$benchmark == "baseline18"
index.jet10 <- multiseq$benchmark == "jet10"
index.jet12 <- multiseq$benchmark == "jet12"
index.jet14 <- multiseq$benchmark == "jet14"
index.jet16 <- multiseq$benchmark == "jet16"
index.jet18 <- multiseq$benchmark == "jet18"

#processing values
multiseq$value_proc = multiseq$value # copying over the single values
multiseq$value_proc[index.baseline10 | index.jet10] = multiseq$value[index.baseline10 | index.jet10] / (2 ^ 10)
multiseq$value_proc[index.baseline12 | index.jet12] = multiseq$value[index.baseline12 | index.jet12] / (2 ^ 12)
multiseq$value_proc[index.baseline14 | index.jet14] = multiseq$value[index.baseline14 | index.jet14] / (2 ^ 14)
multiseq$value_proc[index.baseline16 | index.jet16] = multiseq$value[index.baseline16 | index.jet16] / (2 ^ 16)
multiseq$value_proc[index.baseline18 | index.jet18] = multiseq$value[index.baseline18 | index.jet18] / (2 ^ 18)

#sampling
baseline10 <- multiseq[index.baseline10, 3:4]
baseline12 <- multiseq[index.baseline12, 3:4]
baseline14 <- multiseq[index.baseline14, 3:4]
baseline16 <- multiseq[index.baseline16, 3:4]
baseline18 <- multiseq[index.baseline18, 3:4]
jet10 <- multiseq[index.jet10, 3:4]
jet12 <- multiseq[index.jet12, 3:4]
jet14 <- multiseq[index.jet14, 3:4]
jet16 <- multiseq[index.jet16, 3:4]
jet18 <- multiseq[index.jet18, 3:4]

sample.size <- 10000

sampled.baseline10 <- sample(baseline10$value_proc, size = sample.size, replace = TRUE, prob = baseline10$count)
sampled.baseline12 <- sample(baseline12$value_proc, size = sample.size, replace = TRUE, prob = baseline12$count)
sampled.baseline14 <- sample(baseline14$value_proc, size = sample.size, replace = TRUE, prob = baseline14$count)
sampled.baseline16 <- sample(baseline16$value_proc, size = sample.size, replace = TRUE, prob = baseline16$count)
sampled.baseline18 <- sample(baseline18$value_proc, size = sample.size, replace = TRUE, prob = baseline18$count)
sampled.jet10 <- sample(jet10$value_proc, size = sample.size, replace = TRUE, prob = jet10$count)
sampled.jet12 <- sample(jet12$value_proc, size = sample.size, replace = TRUE, prob = jet12$count)
sampled.jet14 <- sample(jet14$value_proc, size = sample.size, replace = TRUE, prob = jet14$count)
sampled.jet16 <- sample(jet16$value_proc, size = sample.size, replace = TRUE, prob = jet16$count)
sampled.jet18 <- sample(jet18$value_proc, size = sample.size, replace = TRUE, prob = jet18$count)

sampled.baseline.df <- data.frame(
  benchmark = c(
    rep("Baseline - 10", sample.size),
    rep("Baseline - 12", sample.size),
    rep("Baseline - 14", sample.size),
    rep("Baseline - 16", sample.size),
    rep("Baseline - 18", sample.size)
  ),
  value = c(sampled.baseline10, sampled.baseline12, sampled.baseline14, sampled.baseline16, sampled.baseline18)
)

sampled.baseline.medians <- data.frame(
  benchmark = c("Baseline - 10", "Baseline - 12", "Baseline - 14", "Baseline - 16", "Baseline - 18"),
  value = c(
    median(sampled.baseline10), 
    median(sampled.baseline12), 
    median(sampled.baseline14), 
    median(sampled.baseline16),
    median(sampled.baseline18)
  )
)

sampled.jet.df <- data.frame(
  benchmark = c(
    rep("Jet - 10", sample.size),
    rep("Jet - 12", sample.size),
    rep("Jet - 14", sample.size),
    rep("Jet - 16", sample.size),
    rep("Jet - 18", sample.size)
  ),
  value = c(sampled.jet10, sampled.jet12, sampled.jet14, sampled.jet16, sampled.jet18)
)

sampled.jet.medians <- data.frame(
  benchmark = c("Jet - 10", "Jet - 12", "Jet - 14", "Jet - 16", "Jet - 18"),
  value = c(
    median(sampled.jet10), 
    median(sampled.jet12), 
    median(sampled.jet14), 
    median(sampled.jet16), 
    median(sampled.jet18)
  )
)

#plots

plotBaseline <- ggplot(sampled.baseline.df, aes(x=benchmark, y=value)) + 
  geom_boxplot() +
  geom_label(data = sampled.baseline.medians, aes(label = round(value, 2)), nudge_x = 0.5) +
  ggtitle("Multi Baseline") +
  ylab("Normalized Runtime (ns)") +
  xlab("Variant") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = 0.5)
  ) 

plotJet <- ggplot(sampled.jet.df, aes(x=benchmark, y=value)) + 
  geom_boxplot() +
  geom_label(data = sampled.jet.medians, aes(label = round(value, 2)), nudge_x = 0.5) +
  ggtitle("Multi Value") +
  ylab("Normalized Runtime (ns)") +
  xlab("Variant") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = 0.5)
  ) 
