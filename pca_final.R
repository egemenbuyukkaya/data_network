library(tidyverse)
library(unvotes)
library(recipes)
library(embed)
library(countrycode)
library(ggplot2)
library(cowplot)

#between 75 and 70 for period 2014-19
rcid_year14.19 <- subset(un_roll_calls, select = "rcid", subset = (session<75)&(session>68))
rcid_year14.19 <- as.array(rcid_year14.19$rcid)
vote_year14.19 <- subset(un_votes, select = c("rcid","country","vote"), subset = rcid %in% rcid_year14.19)

unvotes_df_year14.19 <- vote_year14.19 %>%
  select(country, rcid, vote) %>%
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>%
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

regi14.19 <- as.factor(countrycode(unvotes_df_year14.19$country, "country.name", "region"))

pca_rec_year14.19 <- recipe(~., data = unvotes_df_year14.19) %>%
    update_role(country, new_role = "id") %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors(), num_comp = 5)
  
  pca_prep14.19 <- prep(pca_rec_year14.19)
  
pca14.19 <- bake(pca_prep14.19, new_data = NULL) %>%
    ggplot(aes(PC1, PC2, label = country, color = regi14.19)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_text(check_overlap = TRUE, color = "black", size = 2) +
  ggtitle("PCA for unvotes 2014-2019") +
    labs(color = NULL)

  umap_rec14.19 <- recipe(~., data = unvotes_df_year14.19) %>%
    update_role(country, new_role = "id") %>%
    step_normalize(all_predictors()) %>%
    step_umap(all_predictors())
  
  umap_prep14.19 <- prep(umap_rec14.19)
  
umap14.19 <- bake(umap_prep14.19, new_data = NULL) %>%
    ggplot(aes(umap_1, umap_2, label = country, color = regi14.19)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_text(check_overlap = TRUE, color = "black", size = 2) +
  ggtitle("UMAP for unvotes 2014-2019") +
    labs(color = NULL)

prow14.19 <- plot_grid(
  pca14.19 + theme(legend.position="none"),
  umap14.19 + theme(legend.position="none"))

legend14.19 <- get_legend(
  pca14.19 + 
    theme(legend.box.margin = margin(0, 0, 0, 12)))

plot_grid(prow14.19, legend14.19, rel_widths = c(3, .8))

#between 75 and 70 for period 1980-85
rcid_year80.85 <- subset(un_roll_calls, select = "rcid", subset = (session<41)&(session>34))
rcid_year80.85 <- as.array(rcid_year$rcid)
vote_year80.85 <- subset(un_votes, select = c("rcid","country","vote"), subset = rcid %in% rcid_year)

unvotes_df_year80.85 <- vote_year80.85 %>%
  select(country, rcid, vote) %>%
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>%
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

regi80.85 <- as.factor(countrycode(unvotes_df_year80.85$country, "country.name", "region"))

pca_rec_year80.85 <- recipe(~., data = unvotes_df_year80.85) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 5)

pca_prep80.85 <- prep(pca_rec_year80.85)

pca80.85 <- bake(pca_prep80.85, new_data = NULL) %>%
  ggplot(aes(PC1, PC2, label = country, color = regi80.85)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, color = "black", size = 2) +
  ggtitle("PCA for unvotes 1980-85") +
  labs(color = NULL)

umap_rec80.85 <- recipe(~., data = unvotes_df_year80.85) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep80.85 <- prep(umap_rec80.85)

umap80.85 <- bake(umap_prep80.85, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country, color = regi80.85)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, color = "black", size = 2) +
  ggtitle("UMAP for unvotes 1980-85") +
  labs(color = NULL)

prow80.85 <- plot_grid(
  pca80.85 + theme(legend.position="none"),
  umap80.85 + theme(legend.position="none"))

legend80.85 <- get_legend(
  pca80.85 + 
    theme(legend.box.margin = margin(0, 0, 0, 12)))

plot_grid(prow80.85, legend80.85, rel_widths = c(3, .8))
