# Paper Calculations
# 13.09.2026

libraries <- c('dplyr','tidyr')
invisible(lapply(libraries, function(x) library(x, character.only = TRUE)))

data <- data.frame(
  CZ       = c('Linton', 'Gordon', 'Trenton', 'Jordan', 'Albany'),
  ThetaCap = c(0.608, 0.443, 0.433, 0.413, 0.331),
  SE       = c(0.014, 0.01, 0.01, 0.05, 0.002)
)

# Figure 1.A --------------------------------------------------------------

data_1A <- data |>
  dplyr::mutate(CI_lower = ThetaCap - (qnorm(1 - 0.05/2) * SE),
                CI_upper = ThetaCap + (qnorm(1 - 0.05/2) * SE))

data_1A

# Figure 1.B --------------------------------------------------------------

trenton_theta <- data$ThetaCap[data$CZ == "Trenton"]
trenton_se    <- data$SE[data$CZ == "Trenton"]

data_1B <- data |>
  filter(CZ != "Trenton") |>
  mutate(
    Diff     = trenton_theta - ThetaCap,                # Trenton - k
    SE_diff  = sqrt(trenton_se^2 + SE^2),               # combined SE
    z        = qnorm(1 - (0.05/4/2)),                   # Bonferroni, 4 two-sided tests
    CI_lower = Diff - z * SE_diff,
    CI_upper = Diff + z * SE_diff,
    Verdict  = case_when(
      CI_lower > 0 ~ "Trenton beats",
      CI_upper < 0 ~ "Trenton beaten",
      TRUE ~ "Tie"))

data_1B

# Figure 2.A --------------------------------------------------------------

all_pairs <- expand.grid(CZ_j = data$CZ, CZ_k = data$CZ) |>
  filter(CZ_j != CZ_k) |>
  left_join(data, by = c("CZ_j" = "CZ")) |>
  rename(Theta_j = ThetaCap, SE_j = SE) |>
  left_join(data, by = c("CZ_k" = "CZ")) |>
  rename(Theta_k = ThetaCap, SE_k = SE)

n_tests_2sided <- choose(5, 2)  # 10 unordered pairs
alpha_per_tail <- 0.05 / (2 * n_tests_2sided)
z_2A <- qnorm(1 - alpha_per_tail)

data_2A <- all_pairs |>
  mutate(
    Diff     = Theta_j - Theta_k,
    SE_diff  = sqrt(SE_j^2 + SE_k^2),
    CI_lower = Diff - z_2A * SE_diff,
    CI_upper = Diff + z_2A * SE_diff,
    Verdict  = case_when(
      CI_lower > 0 ~ "j beats k",
      CI_upper < 0 ~ "j beaten by k",
      TRUE ~ "Tie"
    )
  )
data_2A

# Figure 2.B --------------------------------------------------------------

p <- nrow(data)

rank_intervals <- data_2A |>
  group_by(CZ_j) |>
  summarise(
    N_minus = sum(Verdict == "j beaten by k"),   # # of CZs proven ABOVE j
    N_plus  = sum(Verdict == "j beats k")        # # of CZs proven BELOW j
  ) |>
  mutate(
    rank_lower = N_minus + 1,
    rank_upper = p - N_plus
  )

rank_intervals

# Figure 3.A --------------------------------------------------------------

n_tests_1sided <- p * (p - 1)  # 20 ordered pairs
z_3A <- qnorm(1 - 0.05 / n_tests_1sided)

data_3A <- all_pairs |>
  mutate(
    Diff        = Theta_j - Theta_k,
    SE_diff     = sqrt(SE_j^2 + SE_k^2),
    upper_bound = Diff + z_3A * SE_diff,      # one-sided: how high could Diff be
    k_proven_above_j = upper_bound < 0        # j - k proven negative => k above j
  )

rank_floor <- data_3A |>
  group_by(CZ_j) |>
  summarise(N_minus = sum(k_proven_above_j)) |>
  mutate(rank_floor = N_minus + 1,
         rank_ceiling = p)

tau_best <- sapply(1:p, function(tau) {
  sum(rank_floor$rank_floor <= tau)
})

names(tau_best) <- paste0("tau=", 1:p)

data_3A
tau_best




