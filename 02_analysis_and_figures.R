# ============================================================
# PACKAGES
# ============================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)

# ============================================================
# ADHERENCE SUMMARY
# ============================================================

adherence_summary <-
  
  training_diary %>%
  
  group_by(fp) %>%
  
  summarise(
    
    adherence_sets =
      mean(sets_completed / sets_max),
    
    adherence_reps =
      mean(reps_completed / reps_max),
    
    adherence =
      mean(c(adherence_sets,
             adherence_reps)),
    
    .groups = "drop"
  ) %>%
  
  left_join(
    participants %>%
      select(fp, group),
    by = "fp"
  )

# ============================================================
# BASELINE COMPARISONS
# ============================================================

cat("\n")
cat("========================================\n")
cat("BASELINE COMPARISONS\n")
cat("========================================\n")

participants %>%
  group_by(group) %>%
  summarise(
    n = n(),
    age_mean = mean(age),
    age_sd = sd(age)
  ) %>%
  print()

t.test(age ~ group,
       data = participants)

chisq.test(
  table(
    participants$group,
    participants$sex
  )
)

chisq.test(
  table(
    participants$group,
    participants$cancer_treat
  )
)

# ============================================================
# ADHERENCE COMPARISON
# ============================================================

cat("\n")
cat("========================================\n")
cat("ADHERENCE\n")
cat("========================================\n")

adherence_summary %>%
  group_by(group) %>%
  summarise(
    mean_adherence =
      mean(adherence),
    sd =
      sd(adherence)
  ) %>%
  print()

adherence_ttest <-
  
  t.test(
    adherence ~ group,
    data = adherence_summary
  )

print(adherence_ttest)

# ============================================================
# FUNCTION TO RUN MIXED MODELS
# ============================================================

run_mixed_model <- function(outcome){
  
  dat <-
    
    measurements %>%
    
    filter(
      test_name == outcome
    ) %>%
    
    left_join(
      participants %>%
        select(fp, group),
      by = "fp"
    )
  
  model <-
    
    lmer(
      test_value ~
        group *
        test +
        (1|fp),
      data = dat
    )
  
  cat("\n")
  cat("========================================\n")
  cat(outcome,"\n")
  cat("========================================\n")
  
  print(
    anova(model)
  )
  
  invisible(model)
}

# ============================================================
# MIXED MODELS
# ============================================================

model_pushups  <- run_mixed_model("push-ups")
model_step     <- run_mixed_model("6-min-step")
model_legpress <- run_mixed_model("legpress")
model_glucose  <- run_mixed_model("glucose")
model_phq9     <- run_mixed_model("phq-9")

# ============================================================
# ANCOVA FUNCTION
# ============================================================

run_ancova <- function(outcome){
  
  dat <-
    
    measurements %>%
    
    filter(
      test_name == outcome
    ) %>%
    
    select(
      fp,
      test,
      test_value
    ) %>%
    
    pivot_wider(
      names_from = test,
      values_from = test_value
    ) %>%
    
    left_join(
      participants %>%
        select(fp, group),
      by = "fp"
    )
  
  model <-
    
    lm(
      post ~ pre + group,
      data = dat
    )
  
  cat("\n")
  cat("========================================\n")
  cat("ANCOVA:", outcome,"\n")
  cat("========================================\n")
  
  print(summary(model))
  
  invisible(model)
}

# ============================================================
# ANCOVA ANALYSES
# ============================================================

ancova_pushups  <- run_ancova("push-ups")
ancova_step     <- run_ancova("6-min-step")
ancova_legpress <- run_ancova("legpress")
ancova_glucose  <- run_ancova("glucose")
ancova_phq9     <- run_ancova("phq-9")

# ============================================================
# CHANGE SCORES
# ============================================================

change_scores <-
  
  measurements %>%
  
  select(
    fp,
    test_name,
    test,
    test_value
  ) %>%
  
  pivot_wider(
    names_from = test,
    values_from = test_value
  ) %>%
  
  mutate(
    delta = post - pre
  ) %>%
  
  left_join(
    participants %>%
      select(fp, group),
    by = "fp"
  ) %>%
  
  left_join(
    adherence_summary %>%
      select(fp, adherence),
    by = "fp"
  )

# ============================================================
# ADHERENCE EFFECTS
# ============================================================

cat("\n")
cat("========================================\n")
cat("ADHERENCE EFFECTS\n")
cat("========================================\n")

unique(change_scores$test_name)

for(outcome in unique(change_scores$test_name)){
  
  cat("\n")
  cat("Outcome:", outcome,"\n")
  
  model <-
    
    lm(
      delta ~ adherence,
      data =
        filter(
          change_scores,
          test_name == outcome
        )
    )
  
  print(summary(model))
}

# ============================================================
# FIGURE 1
# ADHERENCE BOXPLOT
# ============================================================

fig1 <-
  
  ggplot(
    adherence_summary,
    aes(
      x = group,
      y = adherence,
      fill = group
    )
  ) +
  
  geom_boxplot() +
  
  theme_classic() +
  
  labs(
    title = "Exercise adherence",
    x = "",
    y = "Adherence"
  )

fig1

# ============================================================
# FIGURE 2
# PRE-POST SPAGHETTI
# ============================================================

plot_spaghetti <- function(outcome){
  
  measurements %>%
    
    filter(
      test_name == outcome
    ) %>%
    
    left_join(
      participants %>%
        select(fp, group),
      by = "fp"
    ) %>%
    
    ggplot(
      aes(
        x = test,
        y = test_value,
        group = fp,
        colour = group
      )
    ) +
    
    geom_line(
      alpha = .20
    ) +
    
    stat_summary(
      aes(group = group),
      fun = mean,
      geom = "line",
      linewidth = 2
    ) +
    
    theme_classic() +
    
    labs(
      title = outcome
    )
}

plot_spaghetti("push-ups")
plot_spaghetti("6-min-step")
plot_spaghetti("legpress")
plot_spaghetti("glucose")
plot_spaghetti("phq-9")

# ============================================================
# FIGURE 3
# CHANGE SCORES
# ============================================================

plot_change <- function(outcome){
  
  change_scores %>%
    
    filter(
      test_name == outcome
    ) %>%
    
    ggplot(
      aes(
        x = group,
        y = delta,
        fill = group
      )
    ) +
    
    geom_violin(
      alpha = .50
    ) +
    
    geom_boxplot(
      width = .15
    ) +
    
    theme_classic() +
    
    labs(
      title = paste(
        "Change score:",
        outcome
      ),
      y = "Post - Pre"
    )
}

plot_change("push-ups")
plot_change("6-min-step")
plot_change("legpress")
plot_change("glucose")
plot_change("phq-9")

# ============================================================
# FIGURE 4
# ADHERENCE VS RESPONSE
# ============================================================

plot_adherence_response <- function(outcome){
  
  change_scores %>%
    
    filter(
      test_name == outcome
    ) %>%
    
    ggplot(
      aes(
        adherence,
        delta
      )
    ) +
    
    geom_point() +
    
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    
    theme_classic() +
    
    labs(
      title =
        paste(
          "Adherence vs response:",
          outcome
        )
    )
}

plot_adherence_response("push-ups")
plot_adherence_response("6-min-step")
plot_adherence_response("legpress")
plot_adherence_response("glucose")
plot_adherence_response("phq-9")

# ============================================================
# TABLE OF CHANGE SCORES
# ============================================================

change_scores %>%
  
  group_by(
    test_name,
    group
  ) %>%
  
  summarise(
    
    n = n(),
    
    mean_change =
      mean(delta),
    
    sd_change =
      sd(delta),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    test_name,
    group
  ) %>%
  
  print()

# ============================================================
# OPTIONAL EXPORTS
# ============================================================

write_csv(
  adherence_summary,
  "adherence_summary.csv"
)

write_csv(
  change_scores,
  "change_scores.csv"
)