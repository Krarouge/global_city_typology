# > R.version
# _                           
# platform       aarch64-apple-darwin20      
# arch           aarch64                     
# os             darwin20                    
# system         aarch64, darwin20           
# status                                     
# major          4                           
# minor          3.2                         
# year           2023                        
# month          10                          
# day            31                          
# svn rev        85441                       
# language       R                           
# version.string R version 4.3.2 (2023-10-31)
# nickname       Eye Holes 

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

setwd("/Users/simon/Downloads/dataverse_files")
documents_main_topic <- read.csv("documents_main_topic.csv")
clusters <- read.csv("variables_clusters_small_medium.csv")

library(ggplot2)

theme_plotly <- function() {
  theme_minimal(base_size = 12) +
    theme(
      # Background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Gridlines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Axis lines and ticks
      axis.line = element_line(color = "gray50"),
      axis.ticks = element_line(color = "gray50", size = 0.5),
      axis.ticks.length = unit(5, "pt"), # Adjust length of ticks
      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(color = "black", size = 14, face = "bold"),
      
      # Legend
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.text = element_text(color = "black", size = 12),
      legend.title = element_text(color = "black", size = 12, face = "bold"),
      
      # Plot Title
      text = element_text(),
      plot.title = element_text(
        color = "black", size = 16, face = "bold", hjust = 0.5
      ),
      plot.subtitle = element_text(color = "gray50", size = 14, hjust = 0.5),
      plot.caption = element_text(color = "gray50", size = 10, hjust = 1)
    )
}


clusters_topic <- left_join(clusters, documents_main_topic, by = c("ID_HDC_G0" = "city_id_per_doc")) %>% as_tibble()

clusters_topic <- clusters_topic %>% 
  group_by(ID_HDC_G0) %>% 
  mutate(n_pub = sum(!is.na(main_topic))/n(),
         `Number of Studies` = ifelse(n_pub != 0, ">=1", "0")) %>% 
  mutate_all(., .funs = function(x){ifelse(x == "<NA>", NA, x)}) %>% 
  rename(Cluster = y_pred_dec) %>% 
  mutate(Cluster = paste("Cluster", Cluster)) %>% 
  mutate(main_topic = gsub("...", ", ", main_topic, fixed = T)) %>% 
  mutate(main_topic = gsub("..", ", ", main_topic, fixed = T)) %>%
  mutate(main_topic = gsub(".", "-", main_topic, fixed = T)) %>% 
  mutate(main_topic = ifelse(main_topic == "Behavioural, institutional", "Behavioural & institutional", main_topic),
         main_topic = ifelse(main_topic == "Coastal, marine", "Coastal & marine", main_topic),
         main_topic = ifelse(main_topic == "Cross−sectoral, adaptation−", "Cross−sectoral (adaptation)", main_topic),
         main_topic = ifelse(main_topic == "Cross−sectoral, impacts−", "Cross−sectoral (impacts)", main_topic),
         main_topic = ifelse(main_topic == "Cross−sectoral, mitigation−", "Cross−sectoral (mitigation)", main_topic),
         main_topic = ifelse(main_topic == "Mountains, snow, ice", "Mountains, snow & ice", main_topic),
         main_topic = ifelse(main_topic == "Negative−emissions", "Negative emissions", main_topic),
         main_topic = ifelse(main_topic == "Policy, governance", "Policy & governance", main_topic),
         main_topic = ifelse(main_topic == "Rivers, lakes, −soil−moisture", "Rivers, lakes, & soil−moisture", main_topic),
         WG_topic = case_when(main_topic %in% c("Coastal & marine", "Human-managed", "Mountains, snow & ice", "Rivers, lakes, & soil moisture", "Terrestrial", "Cross-sectoral (impacts)") ~ "Impacts",
                              main_topic %in% c("Technological", "Management", "Behavioural & institutional", "Ecosystem-based", "Cross-sectoral (adaptation)") ~ "Adaptation",
                              main_topic %in% c("Emissions", "Negative emissions", "Energy", "Buildings", "Transport", "Industry", "Waste", "AFOLU", "Cross-sectoral (mitigation)") ~ "Mitigation",
                              main_topic %in% c("Policy & governance", "Health", "Water") ~ "Cross-cutting"),
         main_topic = factor(main_topic, levels = c("Coastal & marine", "Human-managed", "Mountains, snow & ice", "Rivers, lakes, & soil moisture", "Terrestrial", "Cross-sectoral (impacts)",
                                                         "Technological", "Management", "Behavioural & institutional", "Ecosystem-based", "Cross-sectoral (adaptation)",
                                                         "Emissions", "Negative emissions", "Energy", "Buildings", "Transport", "Industry", "Waste", "AFOLU", "Cross-sectoral (mitigation)",
                                                         "Policy & governance", "Health", "Water")),
         WG_topic = factor(WG_topic, levels = c("Impacts", "Adaptation", "Mitigation", "Cross-cutting")),
         
  )

p_learning_potential <- clusters_topic %>% 
  group_by(Cluster, `Number of Studies`) %>% 
  summarise(`Number of cities` = n()) %>% 
  ggplot(aes(`Number of Studies`, `Number of cities`)) + 
  facet_grid(~`Cluster`) +
  geom_col(fill = "lightblue", col = "black") + 
  theme_plotly() + 
  theme(
    axis.text.x = element_text(
      # angle = 45,
      # vjust=1,
      # hjust=1
    )
  )

ggsave(p_learning_potential, file = "plots/p_learning_potential.pdf", width = 10, height = 4)


p_topics_per_cluster <- clusters_topic %>% 
  group_by(Cluster, `Number of Studies`, main_topic, WG_topic) %>% 
  count() %>% 
  filter(!is.na(main_topic)) %>% 
  group_by(Cluster) %>% # Group by Cluster for row-wise normalization
  mutate(normalized_n = n / sum(n)) %>% # Normalize n within each row (Cluster)

  #   WG_group = ifelse())
  ggplot(aes(main_topic, Cluster, fill = normalized_n)) + 
  facet_grid(~WG_topic, scales = "free", space = "free") +
  scale_fill_continuous(high = "#132B43", low = "#11B1F9") + 
  geom_tile() +
  geom_text(aes(label = n), col = "white") +
  # geom_col(fill = "lightblue", col = "black") + 
  theme_plotly() + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      vjust=1,
      hjust=1
    )
  )

ggsave(p_topics_per_cluster, file = "plots/p_topics_per_cluster.pdf", width = 10, height = 6)

  
