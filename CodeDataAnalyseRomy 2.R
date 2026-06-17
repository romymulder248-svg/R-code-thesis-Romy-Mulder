library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(psych)
library(car)
library(effsize)


#data inladen en eerste rij weghalen (dubbele kolomnamen)
df <- read_csv(file.choose(), skip = 1)
 rawdf <- df
df <- df[-1, ]

#hernoemen kolommen
new_names <- c("start_date", "end_date", "response_type", "ip_address", "progress_pct", "duration_seconds", "is_finished", "recorded_date", "response_id", "last_name", "first_name", "email", "external_reference", "latitude", "longitude", "distribution_channel", "user_language", "consent_information", "participant_number", "age", "gender", "screen_hours_today", "minutes_since_screen_break", "wears_vision_correction", "vision_correction_type", "vision_correction_other", "has_used_vr_before", "pre_vr_unhappy_vs_happy", "pre_vr_annoyed_vs_pleased", "pre_vr_unsatisfied_vs_satisfied", "pre_vr_melancholic_vs_controlled", "pre_vr_bored_vs_relaxed", "pre_vr_relaxed_vs_stimulated", "pre_vr_calm_vs_excited", "pre_vr_sluggish_vs_frenzied", "pre_vr_sleepy_vs_wide_awake", "pre_vr_unaroused_vs_aroused", "pre_vr_dull_vs_jittery", "pre_vr_feeling_controlled_vs_controlling", "pre_vr_influenced_vs_influential", "pre_vr_cared_for_vs_in_control", "pre_vr_awed_vs_important", "pre_vr_submissive_vs_dominant","pre_guided", "pre_vr_symptom_general_discomfort", "pre_vr_symptom_fatigue", "pre_vr_symptom_difficulty_focusing", "pre_vr_symptom_eyestrain", "pre_vr_symptom_headache", "coping_relax_after_work", "coping_spare_time_relax", "coping_under_stress", "coping_tasks_accumulate", "coping_make_mistake", "coping_unsure_social", "coping_criticized_choice", "coping_others_criticize", "vr_scene_start_notification", "post_vr_unhappy_vs_happy", "post_vr_annoyed_vs_pleased", "post_vr_unsatisfied_vs_satisfied", "post_vr_melancholic_vs_controlled", "post_vr_bored_vs_relaxed", "post_vr_relaxed_vs_stimulated", "post_vr_calm_vs_excited", "post_vr_sluggish_vs_frenzied", "post_vr_sleepy_vs_wide_awake", "post_vr_unaroused_vs_aroused", "post_vr_dull_vs_jittery", "post_vr_feeling_controlled_vs_controlling", "post_vr_influenced_vs_influential", "post_vr_cared_for_vs_in_control", "post_vr_awed_vs_important", "post_vr_submissive_vs_dominant","post_guided", "post_vr_symptom_general_discomfort", "post_vr_symptom_fatigue", "post_vr_symptom_difficulty_focusing", "post_vr_symptom_eyestrain", "post_vr_symptom_headache")
names(df) <- new_names

#participant nummer naar getal zetten en dataset beginnen bij participant nummer
df$participant_number <- as.numeric(as.character(df$participant_number))
start_col <- which(names(df) == "participant_number")
df_clean <- df[, start_col:ncol(df)]


#welke Items horen bij valentie, pre en post
pre_valentie_cols <- c(
  "pre_vr_unhappy_vs_happy", "pre_vr_annoyed_vs_pleased",
  "pre_vr_unsatisfied_vs_satisfied", "pre_vr_melancholic_vs_controlled",
  "pre_vr_bored_vs_relaxed"
)
post_valentie_cols <- c(
  "post_vr_unhappy_vs_happy", "post_vr_annoyed_vs_pleased",
  "post_vr_unsatisfied_vs_satisfied", "post_vr_melancholic_vs_controlled",
  "post_vr_bored_vs_relaxed"
)

#Welke items horen bij arousal, pre en post
pre_arousal_cols <- c(
  "pre_vr_relaxed_vs_stimulated", "pre_vr_calm_vs_excited",
  "pre_vr_sluggish_vs_frenzied", "pre_vr_dull_vs_jittery",
  "pre_vr_sleepy_vs_wide_awake", "pre_vr_unaroused_vs_aroused"
)
post_arousal_cols <- c(
  "post_vr_relaxed_vs_stimulated", "post_vr_calm_vs_excited",
  "post_vr_sluggish_vs_frenzied", "post_vr_dull_vs_jittery",
  "post_vr_sleepy_vs_wide_awake", "post_vr_unaroused_vs_aroused"
)

#Welke items horen bij dominance, pre en post
pre_dominance_cols <- c(
  "pre_vr_feeling_controlled_vs_controlling", "pre_vr_influenced_vs_influential",
  "pre_vr_cared_for_vs_in_control", "pre_vr_awed_vs_important",
  "pre_vr_submissive_vs_dominant"
)
post_dominance_cols <- c(
  "post_vr_feeling_controlled_vs_controlling", "post_vr_influenced_vs_influential",
  "post_vr_cared_for_vs_in_control", "post_vr_awed_vs_important",
  "post_vr_submissive_vs_dominant"
)

#een grote lijst van alle kolomnamen die bij de SAM-schalen horen 
sd_cols <- c(pre_valentie_cols, post_valentie_cols,
             pre_arousal_cols,  post_arousal_cols,
             pre_dominance_cols, post_dominance_cols)

#NA omzetten naar 0 — maar NIET voor de SAM-items.
#Lege SAM-velden blijven NA, zodat ze niet als een echte 0 meetellen in de gemiddeldes.
fill_cols <- setdiff(names(df_clean), sd_cols)
df_clean[fill_cols] <- lapply(df_clean[fill_cols], function(x) {
  x[is.na(x)] <- 0
  x
})

#langs elke kolom en maak numeric
df_clean[sd_cols] <- lapply(df_clean[sd_cols], as.numeric)

#Voor elke deelnemer: Het gemiddelde van de pre-items → pre-score
#Het gemiddelde van de post-items → post-score
#Post minus pre → delta (veranderingsscore)
sd_summary <- df_clean %>%
  select(participant_number, age, gender) %>%
  mutate(
    pre_valentie   = rowMeans(df_clean[, pre_valentie_cols],  na.rm = TRUE),
    pre_arousal    = rowMeans(df_clean[, pre_arousal_cols],   na.rm = TRUE),
    pre_dominance  = rowMeans(df_clean[, pre_dominance_cols], na.rm = TRUE),
    post_valentie  = rowMeans(df_clean[, post_valentie_cols],  na.rm = TRUE),
    post_arousal   = rowMeans(df_clean[, post_arousal_cols],   na.rm = TRUE),
    post_dominance = rowMeans(df_clean[, post_dominance_cols], na.rm = TRUE),
    delta_valentie  = post_valentie  - pre_valentie,
    delta_arousal   = post_arousal   - pre_arousal,
    delta_dominance = post_dominance - pre_dominance
  )

#participant 19 verwijderen n.a.v. analyse
sd_summary <- sd_summary %>% filter(participant_number != 19)
df_clean <- df_clean %>% filter(participant_number != 19)

#controlerend, administratief 
cat("\n--- SEMANTIC DIFFERENTIAL DATASET GEMAAKT ---\n")
cat("Kolommen:", ncol(sd_summary), "\n")
cat("Rijen:", nrow(sd_summary), "\n\n")
head(sd_summary, 10)
cat("\n--- SAMENVATTING STATISTIEKEN ---\n")
summary(sd_summary[, -c(1:3)])
write_csv(sd_summary, "sd_summary.csv")
cat("\n✓ SD dataset opgeslagen als 'sd_summary.csv'\n")

#elke schaal mini-dataframe, voor elke schaal apart de betrouwbaarheid berekenen.
scales <- list(
  Pre_Valence   = df_clean[, pre_valentie_cols],
  Pre_Arousal   = df_clean[, pre_arousal_cols],
  Pre_Dominance = df_clean[, pre_dominance_cols],
  Post_Valence   = df_clean[, post_valentie_cols],
  Post_Arousal   = df_clean[, post_arousal_cols],
  Post_Dominance = df_clean[, post_dominance_cols]
)

#Cronbachs alpha voor elke schaal berekenen
alphas <- lapply(scales, function(x) {
  alpha(x, check.keys = TRUE)$total$raw_alpha
})

#Cronbachs alpha's laten zien en afronden op 3 decimalen
alphas <- round(unlist(alphas), 3)
print(alphas)

#tabel met twee kolommen: de naam van de schaal en de bijbehorende alpha-waarde
reliability_summary <- data.frame(
  Scale = names(alphas),
  Alpha = alphas
)

#lay-out van het plotvenster
par(mfrow = c(3, 3), mar = c(3, 3, 2, 1), mgp = c(2, 0.7, 0))

# histogram per schaal. visuele controle van data én van de normaalverdelingsassumptie voor ANOVA's.
hist(sd_summary$pre_valentie,   main = "Pre Valentie",   xlab = "Score")
hist(sd_summary$pre_arousal,    main = "Pre Arousal",    xlab = "Score")
hist(sd_summary$pre_dominance,  main = "Pre Dominance",  xlab = "Score")
hist(sd_summary$post_valentie,  main = "Post Valentie",  xlab = "Score")
hist(sd_summary$post_arousal,   main = "Post Arousal",   xlab = "Score")
hist(sd_summary$post_dominance, main = "Post Dominance", xlab = "Score")
hist(sd_summary$delta_valentie,  main = "Delta Valentie",  xlab = "Verschil")
hist(sd_summary$delta_arousal,   main = "Delta Arousal",   xlab = "Verschil")
hist(sd_summary$delta_dominance, main = "Delta Dominance", xlab = "Verschil")

#Reset de plotinstellingen terug naar standaard
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))  # reset to defaults

#kolommenlijst voor datakwaliteitscontrole
cols <- c(
  "pre_vr_unhappy_vs_happy", "pre_vr_annoyed_vs_pleased", "pre_vr_unsatisfied_vs_satisfied",
  "pre_vr_melancholic_vs_controlled", "pre_vr_bored_vs_relaxed", "pre_vr_relaxed_vs_stimulated",
  "pre_vr_calm_vs_excited", "pre_vr_sluggish_vs_frenzied", "pre_vr_sleepy_vs_wide_awake",
  "pre_vr_unaroused_vs_aroused", "pre_vr_dull_vs_jittery", "pre_vr_feeling_controlled_vs_controlling",
  "pre_vr_influenced_vs_influential", "pre_vr_cared_for_vs_in_control", "pre_vr_awed_vs_important",
  "pre_vr_submissive_vs_dominant", "post_vr_unhappy_vs_happy", "post_vr_annoyed_vs_pleased",
  "post_vr_unsatisfied_vs_satisfied", "post_vr_melancholic_vs_controlled", "post_vr_bored_vs_relaxed",
  "post_vr_relaxed_vs_stimulated", "post_vr_calm_vs_excited", "post_vr_sluggish_vs_frenzied",
  "post_vr_sleepy_vs_wide_awake", "post_vr_unaroused_vs_aroused", "post_vr_dull_vs_jittery",
  "post_vr_feeling_controlled_vs_controlling", "post_vr_influenced_vs_influential",
  "post_vr_cared_for_vs_in_control", "post_vr_awed_vs_important", "post_vr_submissive_vs_dominant"
)

#hoeveel antwoorden precies 0 zijn, en hoeveel precies 50
df_clean$nr_of_zeros <- rowSums(df_clean[, intersect(cols, names(df_clean))] == 0, na.rm = TRUE)
df_clean$nr_of_50s   <- rowSums(df_clean[, intersect(cols, names(df_clean))] == 50, na.rm = TRUE)
df_clean$variance_per_person <- apply(df_clean[, cols], 1, var, na.rm = TRUE)

#data van breed naar lang formaat + voor de zekerheid omzetten naar numeriek
long_df <- df_clean %>%
  select(participant_number, all_of(cols)) %>%
  pivot_longer(cols = all_of(cols), names_to = "item", values_to = "score")
long_df$score <- as.numeric(as.character(long_df$score))

#histogram per participant met antwoorden
ggplot(long_df, aes(x = score)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ participant_number) +
  coord_cartesian(xlim = c(0, 100))

#participanten toewijzen aan scenes
df_clean$Scene <- NA_character_
df_clean$Scene[df_clean$participant_number %in% c(1,16,11,13,15,18,26,35,9,38,29,43,23,33)] <- "Natuur, zonder"
df_clean$Scene[df_clean$participant_number %in% c(2,7,3,25,5,10,6,39,21,28,36,24,32,41)]               <- "Natuur, met"
df_clean$Scene[df_clean$participant_number %in% c(8,17,4,12,20,34,22,30,31,42,27,40,14)]         <- "Bus"

sd_summary <- sd_summary %>%
  left_join(df_clean[, c("participant_number", "Scene")],       by = "participant_number") %>%
  left_join(df_clean[, c("participant_number", "nr_of_zeros")], by = "participant_number") %>%
  left_join(df_clean[, c("participant_number", "nr_of_50s")],   by = "participant_number")

#controle data kwaliteit
#deel 1: missende data
cat("\n--- Missing Data ---\n")
missing_per_item <- colSums(is.na(df_clean[, sd_cols]))
if (sum(missing_per_item > 0) > 0) {
  print(missing_per_item[missing_per_item > 0])
} else {
  cat("No missing data detected.\n")
}

#deel 2: uitbijters
cat("\n--- Potential Outliers (±3SD from mean) ---\n")
outlier_list <- list()
for (col in c("pre_valentie", "pre_arousal", "pre_dominance",
              "post_valentie", "post_arousal", "post_dominance")) {
  outliers <- which(abs(scale(sd_summary[[col]])) > 3)
  if (length(outliers) > 0) {
    cat(col, ": Participant(s)", paste(outliers, collapse = ", "), "\n")
    outlier_list[[col]] <- outliers
  }
}
sd_summary[1, c("participant_number", "pre_valentie")]

if (length(outlier_list) == 0) cat("No extreme outliers detected (±3SD).\n")

#gemiddeld aantal nullen, vijtigens en >10 0 of 50
cat("\n--- Response Pattern Summary ---\n")
cat("Mean zeros per person:", round(mean(sd_summary$nr_of_zeros), 2), "\n")
cat("Mean 50s per person:",  round(mean(sd_summary$nr_of_50s),   2), "\n")
cat("Participants with >10 zeros:",        sum(sd_summary$nr_of_zeros > 10), "\n")
cat("Participants with >10 middle (50s):", sum(sd_summary$nr_of_50s   > 10), "\n")
if (sum(sd_summary$nr_of_zeros > 10) > 0) {
  cat("⚠ WARNING: many zeros:\n")
  print(sd_summary$participant_number[sd_summary$nr_of_zeros > 10])
}
if (sum(sd_summary$nr_of_50s > 10) > 0) {
  cat("⚠ NOTE: many middle values:\n")
  print(sd_summary$participant_number[sd_summary$nr_of_50s > 10])
}
#variantie checken
cat("\nMean variance per person:", round(mean(df_clean$variance_per_person, na.rm = TRUE), 2), "\n")
cat("Participants with variance <100:", sum(df_clean$variance_per_person < 100, na.rm = TRUE), "\n")

#demografische gegevens checken
sd_summary$age <- as.numeric(sd_summary$age)

demo_check <- sd_summary %>%
  group_by(Scene) %>%
  summarise(n = n(), mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE), .groups = 'drop')
print(demo_check)

gender_table <- table(sd_summary$Scene, sd_summary$gender)
cat("\n--- Gender Distribution ---\n"); print(gender_table)

#toetst of de drie groepen vergelijkbaar zijn op geslacht
chi_gender <- chisq.test(gender_table)
print(chi_gender)
if (chi_gender$p.value < 0.05) cat("⚠ Gender differs across scenes\n") else cat("✓ Gender balanced\n")

#toetst of de drie groepen vergelijkbaar zijn op leeftijd
anova_age <- aov(age ~ Scene, data = sd_summary)
print(summary(anova_age))
p_age <- summary(anova_age)[[1]]["Scene", "Pr(>F)"]
if (p_age < 0.05) cat("⚠ Age differs across scenes\n") else cat("✓ Age balanced\n")

#samenvatting pre-vr
pre_vr_summary <- sd_summary %>%
  group_by(Scene) %>%
  summarise(
    n = n(),
    mean_pre_valentie  = mean(pre_valentie,  na.rm = TRUE), sd_pre_valentie  = sd(pre_valentie,  na.rm = TRUE),
    mean_pre_arousal   = mean(pre_arousal,   na.rm = TRUE), sd_pre_arousal   = sd(pre_arousal,   na.rm = TRUE),
    mean_pre_dominance = mean(pre_dominance, na.rm = TRUE), sd_pre_dominance = sd(pre_dominance, na.rm = TRUE),
    .groups = 'drop'
  )
pre_vr_summary %>% mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print()

#ANOVA uitvoeren op de voormeting per dimensie.
anova_pre_valentie  <- aov(pre_valentie  ~ Scene, data = sd_summary)
anova_pre_arousal   <- aov(pre_arousal   ~ Scene, data = sd_summary)
anova_pre_dominance <- aov(pre_dominance ~ Scene, data = sd_summary)

print(summary(anova_pre_valentie))
p_pre_valentie <- summary(anova_pre_valentie)[[1]]["Scene", "Pr(>F)"]
cat(sprintf("Pre Valence   p = %.4f %s\n", p_pre_valentie,
            ifelse(p_pre_valentie < 0.05, "Groups differ at baseline", "✓ Equivalent at baseline")))

print(summary(anova_pre_arousal))
p_pre_arousal <- summary(anova_pre_arousal)[[1]]["Scene", "Pr(>F)"]
cat(sprintf("Pre Arousal   p = %.4f %s\n", p_pre_arousal,
            ifelse(p_pre_arousal < 0.05, "Groups differ at baseline", "✓ Equivalent at baseline")))

print(summary(anova_pre_dominance))
p_pre_dominance <- summary(anova_pre_dominance)[[1]]["Scene", "Pr(>F)"]
cat(sprintf("Pre Dominance p = %.4f %s\n", p_pre_dominance,
            ifelse(p_pre_dominance < 0.05, "Groups differ at baseline", "✓ Equivalent at baseline")))

#post-vr ANOVA
#Shapiro-Wilk: toetst of de scores normaal verdeeld zijn binnen elke groep
for (scene in unique(na.omit(sd_summary$Scene))) {
  cat(sprintf("  Post Valence   - %s: p = %.4f\n", scene,
              shapiro.test(sd_summary$post_valentie[sd_summary$Scene == scene])$p.value))
}
for (scene in unique(na.omit(sd_summary$Scene))) {
  cat(sprintf("  Post Arousal   - %s: p = %.4f\n", scene,
              shapiro.test(sd_summary$post_arousal[sd_summary$Scene == scene])$p.value))
}
for (scene in unique(na.omit(sd_summary$Scene))) {
  cat(sprintf("  Post Dominance - %s: p = %.4f\n", scene,
              shapiro.test(sd_summary$post_dominance[sd_summary$Scene == scene])$p.value))
}

#Levene's test: toetst of de varianties gelijk zijn tussen groepen (homoscedasticiteit)
levene_valentie  <- leveneTest(post_valentie  ~ Scene, data = sd_summary)
levene_arousal   <- leveneTest(post_arousal   ~ Scene, data = sd_summary)
levene_dominance <- leveneTest(post_dominance ~ Scene, data = sd_summary)
cat("Post Valence:\n");   print(levene_valentie)
cat("Post Arousal:\n");   print(levene_arousal)
cat("Post Dominance:\n"); print(levene_dominance)

#helper functies
calc_eta_squared <- function(aov_obj) {
  ss_effect <- summary(aov_obj)[[1]]["Scene", "Sum Sq"]
  ss_total  <- sum(summary(aov_obj)[[1]][, "Sum Sq"])
  ss_effect / ss_total
}

print_bonferroni <- function(outcome_var, data) {
  result <- pairwise.t.test(
    x   = data[[outcome_var]],
    g   = data$Scene,
    p.adjust.method = "bonferroni",
    paired = FALSE
  )
  cat("\n  Bonferroni-corrected pairwise t-tests:\n")
  print(result)
  
  p_mat <- result$p.value
  pairs <- which(!is.na(p_mat), arr.ind = TRUE)
  for (k in 1:nrow(pairs)) {
    r <- pairs[k, 1]; col_k <- pairs[k, 2]
    grp1 <- rownames(p_mat)[r]
    grp2 <- colnames(p_mat)[col_k]
    p_adj <- p_mat[r, col_k]
    sig   <- ifelse(p_adj < 0.05, "* SIGNIFICANT", "(ns)")
    cat(sprintf("    %s vs %s: p_adj = %.4f  %s\n", grp1, grp2, p_adj, sig))
  }
}

report_anova <- function(label, aov_obj, outcome_var, data) {
  print(summary(aov_obj))
  p   <- summary(aov_obj)[[1]]["Scene", "Pr(>F)"]
  eta <- calc_eta_squared(aov_obj)
  cat(sprintf("\np-value: %.4f", p))
  if (p < 0.05) {
    cat(" *** SIGNIFICANT ***\n")
    cat(sprintf("Effect size (eta^2): %.4f", eta))
    if (eta < 0.01) cat(" (small)") else if (eta < 0.06) cat(" (medium)") else cat(" (large)")

    # NB: Tukey HSD verwijderd n.a.v. feedback (rStudio-meeting 07-05):
    #     kies Bonferroni OF Tukey, niet allebei. Wij houden Bonferroni aan,
    #     in lijn met de methode-sectie.
    print_bonferroni(outcome_var, data)

  } else {
    cat(" (not significant)\n")
    cat("  (Post-hoc tests not run - overall ANOVA not significant)\n")
  }
  invisible(list(p = p, eta = eta))
}

#post-vr one-way ANOVA: vergelijkt de drie scènes op hun post-VR scores.
cat("\n\n=== POST-VR ONE-WAY ANOVA (between scenes) ===\n")
cat("Post-hoc correction: Bonferroni\n")

cat("\n--- POST VALENTIE BY SCENE ---\n")
anova_post_valentie <- aov(post_valentie ~ Scene, data = sd_summary)
res_post_valentie   <- report_anova("Post Valentie",  anova_post_valentie, "post_valentie",  sd_summary)
p_post_valentie     <- res_post_valentie$p
eta_post_valentie   <- res_post_valentie$eta

cat("\n--- POST AROUSAL BY SCENE ---\n")
anova_post_arousal <- aov(post_arousal ~ Scene, data = sd_summary)
res_post_arousal   <- report_anova("Post Arousal",   anova_post_arousal,  "post_arousal",   sd_summary)
p_post_arousal     <- res_post_arousal$p
eta_post_arousal   <- res_post_arousal$eta

cat("\n--- POST DOMINANCE BY SCENE ---\n")
anova_post_dominance <- aov(post_dominance ~ Scene, data = sd_summary)
res_post_dominance   <- report_anova("Post Dominance", anova_post_dominance, "post_dominance", sd_summary)
p_post_dominance     <- res_post_dominance$p
eta_post_dominance   <- res_post_dominance$eta

# Per conditie toetsen of de scores binnen die groep significant veranderden van voor naar na de VR.
cat("\n\n")
cat("================================================================================\n")
cat("=== WITHIN-SCENE PRE-POST TESTS                                              ===\n")
cat("=== Paired t-test + Wilcoxon + Bonferroni correction (9 comparisons)         ===\n")
cat("================================================================================\n")
cat("Note: Bonferroni α = 0.05 / 9 = 0.0056 (3 scenes × 3 dimensions)\n")
cat("  Paired t-test:  parametric, assumes normality of differences\n")
cat("  Wilcoxon:       non-parametric, robust alternative\n")
cat("  Cohen's d:      effect size (negligible<0.2, small<0.5, medium<0.8, large≥0.8)\n\n")

n_comparisons  <- 9
alpha_bonf     <- 0.05 / n_comparisons
cat(sprintf("Bonferroni-corrected alpha: %.4f\n\n", alpha_bonf))

scenes     <- c("Bus", "Natuur, met", "Natuur, zonder")
dimensions <- list(
  Valence   = list(pre = "pre_valentie",  post = "post_valentie"),
  Arousal   = list(pre = "pre_arousal",   post = "post_arousal"),
  Dominance = list(pre = "pre_dominance", post = "post_dominance")
)

within_results <- data.frame()

for (scene in scenes) {
  cat(sprintf("\n%s\n", paste(rep("─", 70), collapse = "")))
  cat(sprintf("SCENE: %s\n", scene))
  cat(sprintf("%s\n", paste(rep("─", 70), collapse = "")))
  
  scene_data <- sd_summary %>% filter(Scene == scene)
  n_scene    <- nrow(scene_data)
  cat(sprintf("n = %d participants\n\n", n_scene))
  
  for (dim_name in names(dimensions)) {
    pre_col  <- dimensions[[dim_name]]$pre
    post_col <- dimensions[[dim_name]]$post
    
    pre_vals  <- scene_data[[pre_col]]
    post_vals <- scene_data[[post_col]]
    diffs     <- post_vals - pre_vals
    
    mean_pre  <- round(mean(pre_vals,  na.rm = TRUE), 2)
    mean_post <- round(mean(post_vals, na.rm = TRUE), 2)
    mean_diff <- round(mean(diffs,     na.rm = TRUE), 2)
    sd_diff   <- round(sd(diffs,       na.rm = TRUE), 2)
    
    shap   <- shapiro.test(diffs)
    ttest  <- t.test(post_vals, pre_vals, paired = TRUE)
    wilcox <- wilcox.test(post_vals, pre_vals, paired = TRUE, exact = FALSE)
    
    t_p_bonf <- min(ttest$p.value  * n_comparisons, 1)
    w_p_bonf <- min(wilcox$p.value * n_comparisons, 1)
    
    cohens_d_val <- mean(diffs, na.rm = TRUE) / sd(diffs, na.rm = TRUE)
    d_interp <- ifelse(abs(cohens_d_val) < 0.2, "negligible",
                       ifelse(abs(cohens_d_val) < 0.5, "small",
                              ifelse(abs(cohens_d_val) < 0.8, "medium", "large")))
    
    direction <- ifelse(mean_diff > 0, "↑ increased",
                        ifelse(mean_diff < 0, "↓ decreased", "→ no change"))
    
    cat(sprintf("  [ %s ]\n", dim_name))
    cat(sprintf("    Pre mean: %.2f  |  Post mean: %.2f  |  Δ = %.2f (%s)\n",
                mean_pre, mean_post, mean_diff, direction))
    cat(sprintf("    SD of differences: %.2f\n", sd_diff))
    cat(sprintf("    Normality of diffs (Shapiro-Wilk): p = %.4f %s\n",
                shap$p.value, ifelse(shap$p.value < 0.05, "⚠ not normal", "✓ normal")))
    cat(sprintf("    Paired t-test:  t(%d) = %.3f\n", ttest$parameter, ttest$statistic))
    cat(sprintf("      Raw p        = %.4f  %s\n",
                ttest$p.value,  ifelse(ttest$p.value  < 0.05, "* (p < .05)", "(ns)")))
    cat(sprintf("      Bonferroni p = %.4f  %s\n",
                t_p_bonf,       ifelse(t_p_bonf        < 0.05, "* (p < .05)", "(ns)")))
    cat(sprintf("    Wilcoxon:       W = %.1f\n", wilcox$statistic))
    cat(sprintf("      Raw p        = %.4f  %s\n",
                wilcox$p.value, ifelse(wilcox$p.value  < 0.05, "* (p < .05)", "(ns)")))
    cat(sprintf("      Bonferroni p = %.4f  %s\n",
                w_p_bonf,       ifelse(w_p_bonf         < 0.05, "* (p < .05)", "(ns)")))
    cat(sprintf("    Cohen's d = %.3f (%s effect)\n\n", cohens_d_val, d_interp))
    
    within_results <- rbind(within_results, data.frame(
      Scene              = scene,
      Dimension          = dim_name,
      n                  = n_scene,
      Mean_Pre           = mean_pre,
      Mean_Post          = mean_post,
      Mean_Delta         = mean_diff,
      SD_Delta           = sd_diff,
      Direction          = direction,
      Shapiro_p          = round(shap$p.value,    4),
      Normal_diffs       = shap$p.value >= 0.05,
      t_statistic        = round(ttest$statistic,  3),
      df                 = ttest$parameter,
      t_p_raw            = round(ttest$p.value,    4),
      t_p_bonferroni     = round(t_p_bonf,         4),
      t_sig_raw          = ttest$p.value  < 0.05,
      t_sig_bonferroni   = t_p_bonf        < 0.05,
      W_statistic        = wilcox$statistic,
      W_p_raw            = round(wilcox$p.value,   4),
      W_p_bonferroni     = round(w_p_bonf,          4),
      W_sig_raw          = wilcox$p.value  < 0.05,
      W_sig_bonferroni   = w_p_bonf         < 0.05,
      Cohens_d           = round(cohens_d_val,     3),
      Effect_size        = d_interp,
      stringsAsFactors   = FALSE
    ))
  }
}


#post-vr omschrijvende statistieken
cat("\n\n=== POST-VR DESCRIPTIVE STATISTICS BY SCENE ===\n\n")

post_vr_summary <- sd_summary %>%
  group_by(Scene) %>%
  summarise(
    n = n(),
    mean_post_valentie  = mean(post_valentie,  na.rm = TRUE), sd_post_valentie  = sd(post_valentie,  na.rm = TRUE),
    mean_post_arousal   = mean(post_arousal,   na.rm = TRUE), sd_post_arousal   = sd(post_arousal,   na.rm = TRUE),
    mean_post_dominance = mean(post_dominance, na.rm = TRUE), sd_post_dominance = sd(post_dominance, na.rm = TRUE),
    .groups = 'drop'
  )
post_vr_summary %>% mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print()


#delta anova
cat("\n\n=== DELTA (CHANGE SCORES) ANOVA RESULTS ===\n")
cat("Post-hoc correction: Bonferroni\n\n")

cat("\n--- DELTA VALENTIE BY SCENE ---\n")
anova_delta_valentie <- aov(delta_valentie ~ Scene, data = sd_summary)
res_delta_valentie   <- report_anova("Delta Valentie",  anova_delta_valentie,  "delta_valentie",  sd_summary)
p_delta_valentie     <- res_delta_valentie$p
eta_delta_valentie   <- res_delta_valentie$eta

cat("\n--- DELTA AROUSAL BY SCENE ---\n")
anova_delta_arousal <- aov(delta_arousal ~ Scene, data = sd_summary)
res_delta_arousal   <- report_anova("Delta Arousal",   anova_delta_arousal,   "delta_arousal",   sd_summary)
p_delta_arousal     <- res_delta_arousal$p
eta_delta_arousal   <- res_delta_arousal$eta

cat("\n--- DELTA DOMINANCE BY SCENE ---\n")
anova_delta_dominance <- aov(delta_dominance ~ Scene, data = sd_summary)
res_delta_dominance   <- report_anova("Delta Dominance", anova_delta_dominance, "delta_dominance", sd_summary)
p_delta_dominance     <- res_delta_dominance$p
eta_delta_dominance   <- res_delta_dominance$eta

#Delta omschijvende statistieken
cat("\n\n=== DELTA DESCRIPTIVE STATISTICS BY SCENE ===\n\n")

delta_summary <- sd_summary %>%
  group_by(Scene) %>%
  summarise(
    n = n(),
    mean_delta_valentie  = mean(delta_valentie,  na.rm = TRUE), sd_delta_valentie  = sd(delta_valentie,  na.rm = TRUE),
    mean_delta_arousal   = mean(delta_arousal,   na.rm = TRUE), sd_delta_arousal   = sd(delta_arousal,   na.rm = TRUE),
    mean_delta_dominance = mean(delta_dominance, na.rm = TRUE), sd_delta_dominance = sd(delta_dominance, na.rm = TRUE),
    .groups = 'drop'
  )
delta_summary %>% mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print()

#visualisaties
sd_long <- sd_summary %>%
  select(participant_number, Scene,
         pre_valentie, post_valentie,
         pre_arousal,  post_arousal,
         pre_dominance, post_dominance) %>%
  pivot_longer(
    cols = -c(participant_number, Scene),
    names_to  = c("time", "dimension"),
    names_sep = "_",
    values_to = "score"
  ) %>%
  mutate(
    time = case_when(time == "pre" ~ "Pre-VR", time == "post" ~ "Post-VR", TRUE ~ time),
    time = factor(time, levels = c("Pre-VR", "Post-VR")),
    dimension = case_when(
      dimension == "valentie"  ~ "Valence",
      dimension == "arousal"   ~ "Arousal",
      dimension == "dominance" ~ "Dominance",
      TRUE ~ dimension
    )
  )

ggplot(sd_long, aes(x = time, y = score, color = Scene,
                    group = interaction(Scene, participant_number))) +
  geom_line(alpha = 0.2, linewidth = 0.5) +
  stat_summary(aes(group = Scene), fun = mean, geom = "line", size = 1.2, alpha = 1) +
  stat_summary(aes(group = Scene), fun = mean, geom = "point", size = 3,  alpha = 1) +
  facet_wrap(~ dimension, scales = "free_y") +
  labs(title    = "Pre-Post Changes in Emotional Dimensions by Scene",
       subtitle = "Light lines = individual participants | Bold lines = group means",
       x = "Time", y = "Score", color = "Scene") +
  theme_minimal() +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        strip.text    = element_text(face = "bold", size = 11))
ggsave("pre_post_trajectories.png", width = 12, height = 5, dpi = 300)
cat("\n✓ Pre-post line plot saved as 'pre_post_trajectories.png'\n")

# --- Post-VR boxplots ---
# FIX: small margins for 1x3 layout
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
boxplot(post_valentie  ~ Scene, data = sd_summary, main = "Post-VR Valentie",
        ylab = "Score", col = c("lightblue","lightgreen","lightcoral"), ylim = c(0,100))
boxplot(post_arousal   ~ Scene, data = sd_summary, main = "Post-VR Arousal",
        ylab = "Score", col = c("lightblue","lightgreen","lightcoral"), ylim = c(0,100))
boxplot(post_dominance ~ Scene, data = sd_summary, main = "Post-VR Dominance",
        ylab = "Score", col = c("lightblue","lightgreen","lightcoral"), ylim = c(0,100))
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))  # reset

# --- Delta boxplots ---
# FIX: small margins for 1x3 layout
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
boxplot(delta_valentie  ~ Scene, data = sd_summary, main = "Delta Valentie",
        ylab = "Change (Post-Pre)", col = c("lightblue","lightgreen","lightcoral"))
abline(h = 0, lty = 2, col = "gray")
boxplot(delta_arousal   ~ Scene, data = sd_summary, main = "Delta Arousal",
        ylab = "Change (Post-Pre)", col = c("lightblue","lightgreen","lightcoral"))
abline(h = 0, lty = 2, col = "gray")
boxplot(delta_dominance ~ Scene, data = sd_summary, main = "Delta Dominance",
        ylab = "Change (Post-Pre)", col = c("lightblue","lightgreen","lightcoral"))
abline(h = 0, lty = 2, col = "gray")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))  # reset

#Overzichtstabel p-waarde

cat("\n\n=== COMPREHENSIVE P-VALUE OVERVIEW ===\n\n")

eta_label <- function(eta) {
  if (eta < 0.01) "Small" else if (eta < 0.06) "Medium" else "Large"
}

eta_pre_valentie  <- calc_eta_squared(anova_pre_valentie)
eta_pre_arousal   <- calc_eta_squared(anova_pre_arousal)
eta_pre_dominance <- calc_eta_squared(anova_pre_dominance)

pvalue_overview <- data.frame(
  Analysis_Type = c(rep("PRE-VR",3), rep("POST-VR",3), rep("DELTA",3)),
  Dimension     = rep(c("Valence","Arousal","Dominance"), 3),
  P_Value = round(c(
    p_pre_valentie,  p_pre_arousal,  p_pre_dominance,
    p_post_valentie, p_post_arousal, p_post_dominance,
    p_delta_valentie, p_delta_arousal, p_delta_dominance
  ), 4),
  Effect_Size_EtaSq = round(c(
    eta_pre_valentie,  eta_pre_arousal,  eta_pre_dominance,
    eta_post_valentie, eta_post_arousal, eta_post_dominance,
    eta_delta_valentie, eta_delta_arousal, eta_delta_dominance
  ), 4),
  Effect_Interpretation = c(
    eta_label(eta_pre_valentie),  eta_label(eta_pre_arousal),  eta_label(eta_pre_dominance),
    eta_label(eta_post_valentie), eta_label(eta_post_arousal), eta_label(eta_post_dominance),
    eta_label(eta_delta_valentie), eta_label(eta_delta_arousal), eta_label(eta_delta_dominance)
  ),
  Significant_p05 = c(
    p_pre_valentie  < 0.05, p_pre_arousal  < 0.05, p_pre_dominance  < 0.05,
    p_post_valentie < 0.05, p_post_arousal < 0.05, p_post_dominance < 0.05,
    p_delta_valentie < 0.05, p_delta_arousal < 0.05, p_delta_dominance < 0.05
  ),
  Significant_p01 = c(
    p_pre_valentie  < 0.01, p_pre_arousal  < 0.01, p_pre_dominance  < 0.01,
    p_post_valentie < 0.01, p_post_arousal < 0.01, p_post_dominance < 0.01,
    p_delta_valentie < 0.01, p_delta_arousal < 0.01, p_delta_dominance < 0.01
  ),
  Desired_Outcome = c(rep("p > 0.05 (equiv)",3), rep("p < 0.05 (diff)",3), rep("p < 0.05 (diff)",3))
)

for (i in 1:nrow(pvalue_overview)) {
  achieved <- ifelse(
    pvalue_overview$Analysis_Type[i] == "PRE-VR",
    ifelse(pvalue_overview$P_Value[i] > 0.05, "✓", "✗"),
    ifelse(pvalue_overview$P_Value[i] < 0.05, "✓", "✗")
  )
  cat(sprintf("%-10s | %-10s | %.4f | %-10s | %-8s | %-8s | %s %s\n",
              pvalue_overview$Analysis_Type[i], pvalue_overview$Dimension[i],
              pvalue_overview$P_Value[i], pvalue_overview$Effect_Interpretation[i],
              ifelse(pvalue_overview$Significant_p05[i], "YES *","NO"),
              ifelse(pvalue_overview$Significant_p01[i], "YES **","NO"),
              achieved, pvalue_overview$Desired_Outcome[i]))
}


#pre- post per scene overzicht

item_long <- df_clean %>%
  select(participant_number, Scene, all_of(sd_cols)) %>%
  pivot_longer(
    cols = all_of(sd_cols),
    names_to = "item",
    values_to = "score"
  ) %>%
  mutate(
    time = ifelse(str_detect(item, "^pre"), "Pre", "Post"),
    item_clean = str_replace(item, "^(pre_vr_|post_vr_)", "")
  )

item_wide <- item_long %>%
  select(participant_number, Scene, item_clean, time, score) %>%
  pivot_wider(names_from = time, values_from = score) %>%
  mutate(delta = Post - Pre)

item_summary <- item_wide %>%
  group_by(Scene, item_clean) %>%
  summarise(
    n = n(),
    mean_pre   = mean(Pre,   na.rm = TRUE),
    mean_post  = mean(Post,  na.rm = TRUE),
    mean_delta = mean(delta, na.rm = TRUE),
    sd_delta   = sd(delta,   na.rm = TRUE),
    .groups = "drop"
  )

item_summary <- item_summary %>%
  mutate(
    Dimension = case_when(
      item_clean %in% str_replace(pre_valentie_cols,  "pre_vr_", "") ~ "Valence",
      item_clean %in% str_replace(pre_arousal_cols,   "pre_vr_", "") ~ "Arousal",
      item_clean %in% str_replace(pre_dominance_cols, "pre_vr_", "") ~ "Dominance",
      TRUE ~ "Other"
    )
  )

dimension_summary <- item_summary %>%
  group_by(Scene, Dimension) %>%
  summarise(
    n_items    = n(),
    mean_pre   = mean(mean_pre),
    mean_post  = mean(mean_post),
    mean_delta = mean(mean_delta),
    .groups = "drop"
  )

item_summary_clean <- item_summary %>%
  mutate(across(c(mean_pre, mean_post, mean_delta, sd_delta), ~ round(.x, 2))) %>%
  arrange(Scene, Dimension, item_clean)

dimension_summary_clean <- dimension_summary %>%
  mutate(across(c(mean_pre, mean_post, mean_delta), ~ round(.x, 2))) %>%
  arrange(Scene, Dimension)

#Delta boxplots voor in het verslag
delta_long <- sd_summary %>%
  select(participant_number, Scene, delta_valentie, delta_arousal, delta_dominance) %>%
  pivot_longer(c(delta_valentie, delta_arousal, delta_dominance),
               names_to = "Dimension", values_to = "delta") %>%
  mutate(
    Dimension = dplyr::recode(Dimension,
                       delta_valentie  = "Valence",
                       delta_arousal   = "Arousal",
                       delta_dominance = "Dominance"),
    Dimension = factor(Dimension, levels = c("Valence", "Arousal", "Dominance")),
    Scene = dplyr::recode(Scene,
                   "Natuur, zonder" = "Natuur\nzonder geluid",
                   "Natuur, met"    = "Natuur\nmet geluid",
                   "Bus"            = "Busrit"),
    Scene = factor(Scene, levels = c("Natuur\nzonder geluid",
                                     "Natuur\nmet geluid", "Busrit"))
  )

scene_cols <- c("Natuur\nzonder geluid" = "#A6D9B8",   # lichtgroen
                "Natuur\nmet geluid"    = "#4DA373",   # groen
                "Busrit"                = "#B0B4B2")   # grijs

ggplot(delta_long, aes(Scene, delta, fill = Scene)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_boxplot(width = 0.6, colour = "grey25", outlier.shape = 1) +
  facet_wrap(~ Dimension, scales = "free_y") +
  scale_fill_manual(values = scene_cols) +
  labs(x = NULL, y = "Verandering (post − pre)") +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        strip.text         = element_text(face = "bold", size = 14, colour = "#2E7D5B"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("delta_boxplots.png", width = 11, height = 4.5, dpi = 300)
