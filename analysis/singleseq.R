library("ggplot2")
library("plyr")

#loading data
singleseq <- read.csv("singleseq.csv")

#indices
index.baselineSeq <- singleseq$benchmark == "baselineSequential"
index.baselineSingle <- singleseq$benchmark == "baselineSingle"
index.seqValue <- singleseq$benchmark == "sequenceValue"
index.singleValue <- singleseq$benchmark == "singleValue"

# processing values
singleseq$value_proc = singleseq$value # copying over the single values
normalizationidx = index.baselineSeq | index.seqValue
singleseq$value_proc[normalizationidx] = singleseq$value_proc[normalizationidx] / (2 ^ 14)

# sampling

baselineSeq <- singleseq[index.baselineSeq, 3:4]
baselineSingle <- singleseq[index.baselineSingle, 3:4]
seqValue <- singleseq[index.seqValue, 3:4]
singleValue <- singleseq[index.singleValue, 3:4]

sample.size <- 10000

sampled.baselineSeq <- sample(baselineSeq$value_proc, size = sample.size, replace=TRUE, prob=baselineSeq$count)
sampled.baselineSingle <- sample(baselineSingle$value_proc, size = sample.size, replace=TRUE, prob=baselineSingle$count)
sampled.seqValue <- sample(seqValue$value_proc, size = sample.size, replace=TRUE, prob=seqValue$count)
sampled.singleValue <- sample(singleValue$value_proc, size = sample.size, replace=TRUE, prob=singleValue$count)

deltaSingle <- sampled.singleValue - sampled.baselineSingle
deltaSeq <- sampled.seqValue - sampled.baselineSeq

sampled.df <- data.frame(
  benchmark = c(
    rep("Baseline\nSequence", sample.size),
    rep("Baseline\nSingle", sample.size),
    rep("Value\nSequence", sample.size),
    rep("Value\nSingle", sample.size)
    ),
  value = c(
    sampled.baselineSeq, 
    sampled.baselineSingle, 
    sampled.seqValue,
    sampled.singleValue)
)

sampled.medians <- data.frame(
  benchmark = c("Baseline\nSequence", "Baseline\nSingle", "Value\nSequence", "Value\nSingle"),
  value = c(median(sampled.baselineSeq), median(sampled.baselineSingle), median(sampled.seqValue), median(sampled.singleValue))
)

delta.df <- data.frame(
  benchmark = c(
    rep("Single", sample.size),
    rep("Sequence", sample.size)
  ),
  value = c(
    deltaSingle,
    deltaSeq
  )
)

delta.medians <- data.frame(
  benchmark = c("Single", "Sequence"),
  value = c(median(deltaSingle), median(deltaSeq))
)

# Plots

plotRaw <- ggplot(sampled.df, aes(x=benchmark, y=value)) + 
  geom_boxplot() +
  geom_label(data = sampled.medians, aes(label = round(value, 2)), nudge_x = 0.47) +
  ggtitle("Single Evaluation vs Sequential Evaluation") +
  ylab("Normalized Runtime (ns)") +
  xlab("Variant") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = 0.5)
  )

plotDelta <- ggplot(delta.df, aes(x=benchmark, y=value)) + 
  geom_boxplot() +
  geom_label(data = delta.medians, aes(label = round(value, 2)), nudge_x = 0.47) +
  ggtitle("Differential Evaluation Time") +
  ylab("Normalized Runtime (ns)") +
  xlab("Variant") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = 0.5)
  ) 

