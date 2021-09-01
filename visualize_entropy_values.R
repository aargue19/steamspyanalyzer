setwd("C:/Users/gaoan/Desktop/steamspyanalyzer")
rm(list=ls())

df = read.csv("step12_result.csv")
df = df[rowSums(df[542:966])>0,]
df$num_of_tags = rowSums(df[542:966]!=0)

colnames(df)[115:539]

ent_change_vec = vector()


for (i in 1: nrow(df)){
  
  g1 = df[,542:966]
  g1 = g1[i,0:425]
  
  # summary(df$t2_entropy) # FROM 0 ~ 3.9
  
  # low_ent_game_t2_tags = df[df$t2_entropy<2 & df$num_of_tags>4, 542:966]
  # rand_game_num = round(runif(1, 1, nrow(low_ent_game_t2_tags)),0)
  # g1 = low_ent_game_t2_tags[rand_game_num,0:425]
  
  # high_ent_game_t2_tags = df[df$t2_entropy>3 & df$num_of_tags>5, 542:966]
  # rand_game_num = round(runif(1, 1, nrow(high_ent_game_t2_tags)),0)
  # g1 = high_ent_game_t2_tags[rand_game_num,0:425]
  
  
  df_t = as.data.frame(t(g1))
  colnames(df_t) = "count"
  df_t$count = as.integer(df_t$count)
  df_t$tag_names = rownames(df_t)
  t2_pos_tag_df = df_t[df_t$count > 0,]
  
  require("entropy")
  freqs = freqs.empirical(t2_pos_tag_df$count)
  ent_t2 = entropy.empirical(freqs, unit="log2")
  
  g2 = df[,115:539]
  g2 = g2[i,0:425]
  
  # summary(df$t2_entropy) # FROM 0 ~ 3.9
  
  # low_ent_game_t2_tags = df[df$t2_entropy<2 & df$num_of_tags>4, 542:966]
  # rand_game_num = round(runif(1, 1, nrow(low_ent_game_t2_tags)),0)
  # g1 = low_ent_game_t2_tags[rand_game_num,0:425]
  
  # high_ent_game_t2_tags = df[df$t2_entropy>3 & df$num_of_tags>5, 542:966]
  # rand_game_num = round(runif(1, 1, nrow(high_ent_game_t2_tags)),0)
  # g1 = high_ent_game_t2_tags[rand_game_num,0:425]
  
  df_t = as.data.frame(t(g2))
  colnames(df_t) = "count"
  df_t$count = as.integer(df_t$count)
  df_t$tag_names = rownames(df_t)
  t2_pos_tag_df = df_t[df_t$count > 0,]
  
  require("entropy")
  freqs = freqs.empirical(t2_pos_tag_df$count)
  ent_t1 = entropy.empirical(freqs, unit="log2")

  entropy_change = ent_t2 - ent_t1
  
  ent_change_vec = c(ent_change_vec, entropy_change)
  
}






library(ggplot2)
g = ggplot(t2_pos_tag_df, aes(x=tag_names, y=count)) + 
  geom_bar(stat="identity", fill="lightgreen", color="grey50") +
  ggtitle(ent_val) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

g
