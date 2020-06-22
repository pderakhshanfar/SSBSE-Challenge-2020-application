source('RScripts/dataclean.R')

library(tidyverse)
library(ggplot2)
library(questionr)
library(effsize)
library(moments)
library(knitr) 
library(kableExtra)

# ------------------------------
# Constants
# ------------------------------

COLOR_PALETTE = "Spectral" # Use photocopy friendly colors (http://colorbrewer2.org/)

SIGNIFICANCE_LEVEL = 0.05

# ------------------------------
# Functions definition
# ------------------------------

# function for number of observations
give.n <- function(x) {
  return(c(y = 1e-01, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

# ------------------------------
# Program
# ------------------------------

coverage <- getModelSeedingResult() %>%
  bind_rows(getPureResult()) %>%
  mutate(random_abstract_test_selection = case_when(random_abstract_test_selection == TRUE ~ 'TRUE',
                                                    random_abstract_test_selection == FALSE ~ 'FALSE',
                                                    TRUE ~ '-'),
         clone_seed_p = replace_na(clone_seed_p, 0.0),
         p_object_pool = replace_na(p_object_pool, 0.0),)

mutationscore <- getMutationScores()

results <- coverage %>%
  inner_join(mutationscore, by = c("execution_idx", "application", "class", 
                                   "clone_seed_p", "p_object_pool", "random_abstract_test_selection")) %>%
  mutate(conf = case_when(random_abstract_test_selection == 'TRUE' ~ 'model s.',
                               random_abstract_test_selection == 'FALSE' ~ 'trial',
                               TRUE ~ 'default'),
         class_name = gsub('[a-z0-9]+\\.', '', class))

rm(coverage)
rm(mutationscore)

# Boxplots 

p <- results %>%
  filter(conf != 'trial') %>%
  ggplot(aes(x=class_name, y=BranchCoverage_value, fill=conf)) +
  geom_boxplot() +
  xlab('') +
  ylab('branch coverage') +
  coord_flip() +
  scale_fill_brewer(palette = COLOR_PALETTE)
ggsave('images/branch-coverage.pdf', p, width = 180, height = 90, units = 'mm')

p <- results %>%
  filter(conf != 'trial') %>%
  ggplot(aes(x=class_name, y=score, fill=conf)) +
  geom_boxplot()+
  xlab('') +
  coord_flip() +
  scale_fill_brewer(palette = COLOR_PALETTE)
ggsave('images/mutation-score.pdf', p, width = 180, height = 90, units = 'mm')


df <- results %>%
  filter(conf != 'trial') %>%
  pivot_longer(cols = c(BranchCoverage_value, score), names_to = "Criteria", values_to = "Coverage") %>%
  mutate(Criteria = recode(Criteria, BranchCoverage_value = 'Branch coverage', 
                           score = 'Mutation score')) %>%
  select(Class = class_name, `Conf.` = conf, Criteria, Coverage)

p <- df %>%
  ggplot(aes(x=Class, y=Coverage, fill=`Conf.`)) +
  geom_boxplot() +
  xlab('') +
  scale_fill_brewer(palette = COLOR_PALETTE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95 , vjust = 0.2)) +
  facet_wrap(Criteria~.)

ggsave('images/coverage.pdf', p, width = 200, height = 130, units = 'mm')

rm(df)

# Summary

data_analysis <- results  %>% 
  full_join(results, by = c('class', 'class_name'), suffix = c('.conf1', '.conf2')) %>%
  filter(conf.conf1 != conf.conf2) %>%
  group_by(conf.conf1, conf.conf2, class, class_name) %>%
  summarise(
    # Branch coverage
    BranchCoverage.conf1.mean = mean(BranchCoverage_value.conf1),
    BranchCoverage.default.mean = mean(BranchCoverage_value.conf2),
    BranchCoverage.conf1.median = median(BranchCoverage_value.conf1),
    BranchCoverage.default.median = median(BranchCoverage_value.conf2),
    BranchCoverage.conf1.IQR = IQR(BranchCoverage_value.conf1),
    BranchCoverage.default.IQR = IQR(BranchCoverage_value.conf2),
    BranchCoverage.conf1.sd = sd(BranchCoverage_value.conf1),
    BranchCoverage.default.sd = sd(BranchCoverage_value.conf2),
    # Mutation score
    score.conf1.mean = mean(score.conf1),
    score.default.mean = mean(score.conf2),
    score.conf1.median = median(score.conf1),
    score.default.median = median(score.conf2),
    score.conf1.IQR = IQR(score.conf1),
    score.default.IQR = IQR(score.conf2),
    score.conf1.sd = sd(score.conf1),
    score.default.sd = sd(score.conf2),
    # Statistical analysis
    BranchCoverage_value.VD.magnitude = VD.A(BranchCoverage_value.conf1, BranchCoverage_value.conf2)$magnitude,
    BranchCoverage_value.VD.estimate = VD.A(BranchCoverage_value.conf1, BranchCoverage_value.conf2)$estimate,
    BranchCoverage_value.wilcox.test.pvalue = wilcox.test(BranchCoverage_value.conf1, BranchCoverage_value.conf2)$p.value,
    score.VD.magnitude = VD.A(score.conf1, score.conf2)$magnitude,
    score.VD.estimate = VD.A(score.conf1, score.conf2)$estimate,
    score.wilcox.test.pvalue = wilcox.test(score.conf1, score.conf2)$p.value,
  ) %>%
  mutate(
    BranchCoverage_value.VD.estimate.category = case_when(
      BranchCoverage_value.VD.estimate < 0.5 ~ '< 0.5',
      BranchCoverage_value.VD.estimate > 0.5 ~ '> 0.5',
      TRUE ~ '= 0.5'
    ),
    score.VD.estimate.category = case_when(
      score.VD.estimate < 0.5 ~ '< 0.5',
      score.VD.estimate > 0.5 ~ '> 0.5',
      TRUE ~ '= 0.5'
    )
  )

table_coverage <- data_analysis %>%
  data.frame() %>%
  filter(conf.conf1 == 'model s.', conf.conf2 == 'default') %>%
  select(Class = class_name, 
         bc.mean = BranchCoverage.conf1.mean,
         bc.median = BranchCoverage.conf1.median, 
         bc.IQR = BranchCoverage.conf1.IQR, 
         default.bc.mean = BranchCoverage.default.mean, 
         default.bc.median = BranchCoverage.default.median, 
         default.bc.IQR = BranchCoverage.default.IQR,
         bc.pval = BranchCoverage_value.wilcox.test.pvalue, 
         bc.VD = BranchCoverage_value.VD.estimate, 
         bc.VD.magnitude = BranchCoverage_value.VD.magnitude,
         bc.VD.category = BranchCoverage_value.VD.estimate.category) %>%
  arrange(Class)

kable(table_coverage, "latex", booktabs = T, digits = 3, format.args = list(scientific = FALSE)) %>% 
  add_header_above(c(" ", "model s." = 3, "default" = 3))

table_coverage %>%
  filter(bc.pval < SIGNIFICANCE_LEVEL) %>%
  group_by(bc.VD.category, bc.VD.magnitude) %>%
  count()

table_mutation <- data_analysis %>%
  data.frame() %>%
  filter(conf.conf1 == 'model s.', conf.conf2 == 'default') %>%
  select(Class = class_name, 
         score.mean = score.conf1.mean, 
         score.median = score.conf1.median, 
         score.IQR = score.conf1.IQR,
         default.score.mean = score.default.mean, 
         default.score.median = score.default.median, 
         default.score.IQR = score.default.IQR,
         score.pval = score.wilcox.test.pvalue, 
         score.VD = score.VD.estimate, 
         score.VD.magnitude = score.VD.magnitude,
         score.VD.category = score.VD.estimate.category) %>%
  arrange(Class)

kable(table_mutation, "latex", booktabs = T, digits = 3, format.args = list(scientific = FALSE)) %>% 
  add_header_above(c(" ", "model s." = 3, "default" = 3))

table_mutation %>%
  filter(score.pval < SIGNIFICANCE_LEVEL) %>%
  group_by(score.VD.category, score.VD.magnitude) %>%
  count()

