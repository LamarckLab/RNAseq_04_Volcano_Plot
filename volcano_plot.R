library(DESeq2)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggrepel)

# 火山图的制作
df <- read.csv("C:/Users/Lamarck/Desktop/volcano_plot_data.csv", header = T, stringsAsFactors = F)

# 对不同logFC的点进行分类：Up Down Not sig
df$group <- ifelse(df$logFC>=1 & df$P.Value<=0.05,"UP", ifelse(df$logFC<=-1 & df$P.Value<=0.05, "DOWN", "Not sig"))
table(df$group)

df$pvalue_log10 <- (-log10(df$P.Value))

# 显著点的判定
df1 <- df[df$pvalue_log10 > 2 & (df$logFC > 3 | df$logFC < -3),]
# 检查筛选出的基因数量
dim(df1)

# 绘制火山图
ggplot(df, aes(x = logFC, y = -log10(P.Value))) +
  geom_point(aes(color = group)) +  # 绘制散点，根据 group 着色
  scale_color_manual(values = c("dodgerblue", "grey", "firebrick")) +  # 手动设置颜色
  geom_label_repel(data = df1,  # 使用 geom_label_repel 添加标签
                   aes(x = logFC, y = -log10(P.Value), label = gene_name),
                   box.padding = 1,   # 调整标签与点之间的距离
                   point.padding = 0.3,  # 调整点与线之间的距离
                   segment.color = 'grey50')  # 设置指向散点的线的颜色


























