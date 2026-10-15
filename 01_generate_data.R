library(tidyverse)

set.seed(20261014)

# ============================================================
# DATASET 1: PARTICIPANTS
# ============================================================

n <- 150

participants <- tibble(
  fp = sprintf("FP%03d", 1:n),
  
  group = rep(c("digital", "onsite"), each = n/2),
  
  city = sample(
    c("Lillehammer", "Hamar", "Gjovik"),
    size = n,
    replace = TRUE,
    prob = c(0.30, 0.20, 0.50)
  ),
  
  age = round(pmin(
    pmax(rnorm(n, mean = 60, sd = 12), 20),
    90
  )),
  
  sex = sample(
    c("male", "female"),
    size = n,
    replace = TRUE,
    prob = c(0.45, 0.55)
  )
)

participants <- participants %>%
  mutate(
    cancer_treat = ifelse(
      fp %in% sprintf("FP%03d", 1:45),
      "active",
      "completed"
    )
  )

participants$cancer_diag_group <- sample(
  paste0("group",1:10),
  n,
  replace = TRUE,
  prob = c(
    0.35,
    0.25,
    0.08,
    0.07,
    0.06,
    0.05,
    0.05,
    0.04,
    0.03,
    0.02
  )
)

participants$dropout <- "no"

dropouts <- sample(participants$fp, size = round(n*0.10))

participants$dropout[
  participants$fp %in% dropouts
] <- "yes"

participants <- participants %>%
  mutate(
    dropout = factor(dropout),
    group = factor(group),
    city = factor(city),
    cancer_treat = factor(cancer_treat),
    cancer_diag_group = factor(cancer_diag_group)
  )

# optional dummy names
participants$name <- paste(
  sample(LETTERS, n, replace = TRUE),
  sample(LETTERS, n, replace = TRUE)
)

# ============================================================
# DATASET 3: TRAINING DIARY
# ============================================================

adherence_tbl <- participants %>%
  mutate(
    
    completion_rate_sets =
      ifelse(
        group == "onsite",
        rnorm(n(), 0.92, 0.07),
        rnorm(n(), 0.88, 0.07)
      ),
    
    completion_rate_reps =
      ifelse(
        group == "onsite",
        rnorm(n(), 0.83, 0.09),
        rnorm(n(), 0.78, 0.10)
      ),
    
    completion_rate_sets =
      pmin(pmax(completion_rate_sets,0.50),1),
    
    completion_rate_reps =
      pmin(pmax(completion_rate_reps,0.40),1)
  )

training_diary <-
  
  adherence_tbl %>%
  
  select(
    fp,
    group,
    completion_rate_sets,
    completion_rate_reps,
    dropout
  ) %>%
  
  crossing(
    week = 1:12,
    session = 1:24,
    exercise = paste0("exc_",1:8)
  ) %>%
  
  filter(
    ceiling(session/2) == week
  ) %>%
  
  mutate(
    
    sets_max =
      case_when(
        week <=4 ~ 2,
        week <=8 ~ 3,
        TRUE ~ 4
      ),
    
    reps_max =
      case_when(
        week <=4 ~ 15,
        week <=8 ~ 10,
        TRUE ~ 8
      ),
    
    sets_completed =
      pmax(
        0,
        round(sets_max * completion_rate_sets)
      ),
    
    reps_completed =
      pmax(
        0,
        round(reps_max * completion_rate_reps)
      )
  )

# remove sessions after dropout

dropout_session <- tibble(
  fp = dropouts,
  dropout_session = sample(8:22,
                           length(dropouts),
                           replace = TRUE)
)

training_diary <- training_diary %>%
  left_join(dropout_session, by = "fp") %>%
  filter(
    is.na(dropout_session) |
      session <= dropout_session
  ) %>%
  select(-dropout_session)

# participant adherence summary

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
  )

# ============================================================
# DATASET 2: PRE/POST MEASUREMENTS
# ============================================================

measure_participants <-
  
  participants %>%
  
  left_join(
    adherence_summary,
    by = "fp"
  )

measure_participants$adherence[
  is.na(measure_participants$adherence)
] <- 0.80

# ---------- PRE VALUES ----------

pre <- measure_participants %>%
  transmute(
    
    fp,
    
    pushups =
      pmin(
        pmax(round(rnorm(n(),5,3)),0),
        21
      ),
    
    step6 =
      pmin(
        pmax(round(rnorm(n(),140,25)),90),
        240
      ),
    
    legpress =
      pmin(
        pmax(round(rnorm(n(),250,70)),150),
        500
      ),
    
    glucose =
      pmin(
        pmax(
          round(
            rlnorm(
              n(),
              log(7),
              0.35
            ),1
          ),
          3
        ),
        13
      ),
    
    phq9 =
      pmin(
        pmax(round(rnorm(n(),14,5)),0),
        27
      )
  )

# ---------- POST VALUES ----------

post <- pre %>%
  left_join(
    measure_participants %>%
      select(fp, group, adherence, dropout),
    by = "fp"
  ) %>%
  mutate(
    
    pushups =
      pmin(
        21,
        round(
          pushups +
            2 +
            4*adherence +
            ifelse(group=="onsite",1,0) +
            rnorm(n(),0,1.5)
        )
      ),
    
    step6 =
      round(
        step6 +
          5 +
          12*adherence +
          ifelse(group=="onsite",3,0) +
          rnorm(n(),0,10)
      ),
    
    legpress =
      round(
        legpress +
          15 +
          70*adherence +
          ifelse(group=="onsite",20,0) +
          rnorm(n(),0,20)
      ),
    
    glucose =
      pmax(
        3,
        round(
          glucose -
            1.8*adherence +
            rnorm(n(),0,0.5),
          1
        )
      ),
    
    phq9 =
      pmax(
        0,
        round(
          phq9 -
            (0.25*phq9) -
            2*adherence +
            rnorm(n(),0,1.5)
        )
      )
  )

# remove post values for dropouts

post <- post %>%
  filter(dropout == "no")

# ---------- LONG FORMAT ----------

make_long <- function(df,timepoint){
  
  bind_rows(
    
    tibble(
      fp=df$fp,
      test=timepoint,
      test_type="phys_test",
      test_name="push-ups",
      test_value=df$pushups
    ),
    
    tibble(
      fp=df$fp,
      test=timepoint,
      test_type="phys_test",
      test_name="6-min-step",
      test_value=df$step6
    ),
    
    tibble(
      fp=df$fp,
      test=timepoint,
      test_type="phys_test",
      test_name="legpress",
      test_value=df$legpress
    ),
    
    tibble(
      fp=df$fp,
      test=timepoint,
      test_type="blood",
      test_name="glucose",
      test_value=df$glucose
    ),
    
    tibble(
      fp=df$fp,
      test=timepoint,
      test_type="question",
      test_name="phq-9",
      test_value=df$phq9
    )
  )
}

measurements <- bind_rows(
  make_long(pre,"pre"),
  make_long(post,"post")
)

measurements <- measurements %>%
  mutate(
    test = factor(
      test, 
      levels = c("pre", "post")
    )
  )


participants <- participants %>%
  mutate(
    dropout_bin = 
      ifelse(dropout == "yes", 1, 0)
  )


# ============================================================
# OUTPUT
# ============================================================

write_csv(participants, "participants.csv")
write_csv(training_diary, "training_diary.csv")
write_csv(measurements, "measurements.csv")

# ============================================================
# QUICK CHECKS
# ============================================================

participants %>%
  count(group)

participants %>%
  count(dropout)

adherence_summary %>%
  left_join(
    participants %>%
      select(fp, group),
    by="fp"
  ) %>%
  group_by(group) %>%
  summarise(
    mean_adherence = mean(adherence)
  )

measurements %>%
  group_by(test,test_name) %>%
  summarise(
    median = median(test_value)
  )