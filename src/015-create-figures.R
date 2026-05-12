# ==============================================================================
# 015-create-figures.R
#
# Purpose:
#   Generate the data-derived paper figures as PNGs:
#     Figure 2a: Earnings-related media mentions by FDP (Nexis Uni)
#     Figure 2b: Earnings-related media mentions by FDP, excluding I/B/E/S
#     Figure 3a: FDP coverage by year (CCM sample)
#     Figure 3b: FDP coverage by size quintile
#     Figure 3c: FDP coverage by Fama-French 12 industry
#     Figure 4 : Mean number of contributing analysts (Following) by year
#     Figure 7a: Cumulative event-time returns by FDP identity
#     Figure 7b: Cumulative event-time returns by FDP Avg Quality rank
#
# Inputs (from RAW_DATA_DIR):
#   NexisUniArticlesByYear.xlsx              (hand-collected by Stephannie
#                                            Larocque from Nexis Uni; one
#                                            search per FDP combining the
#                                            FDP name with "EPS" -- see
#                                            Figure 2 section below for the
#                                            search-term details)
#
# Inputs (from DATA_DIR):
#   firm_qtr_2020andearlier_CCM.parquet      (output of 011; CCM sample for
#                                            the coverage figures)
#   2025_12_16_firm_qtr_regdata.parquet      (output of 011; for Figure 4)
#   2025_12_16_fdp_firm_qtr_regdata.parquet  (output of 012; M&B sample for
#                                            Figure 7)
#   wrds_crsp_returns.parquet                (output of 014; daily CRSP
#                                            returns for the event windows)
#
# Outputs (to OUTPUT_DIR):
#   nexis_articles_by_year_with_IBES.png     (Figure 2a)
#   nexis_articles_by_year_no_IBES.png       (Figure 2b)
#   coverage_time_plot.png                   (Figure 3a)
#   coverage_size_plot.png                   (Figure 3b)
#   coverage_industry_plot.png               (Figure 3c)
#   following_time_plot.png                  (Figure 4)
#   fdp_profit_chart.png                     (Figure 7a)
#   quality_profit_chart.png                 (Figure 7b)
#
# Notes:
#   - Reads RAW_DATA_DIR, DATA_DIR, and OUTPUT_DIR from .env via dotenv.
#   - Pure plotting -- no WRDS, no networking. Source data must already be
#     produced by 011 / 012 / 014, except the Nexis Uni xlsx which is
#     hand-collected and ships with the package.
# ==============================================================================

options(scipen = 999)

library(haven)
library(glue)
library(arrow)
library(readxl)
library(ggrepel)
library(scales)
library(forcats)
library(tidyverse)
library(dotenv)

load_dot_env()
raw_data_path <- Sys.getenv("RAW_DATA_DIR")
data_path     <- Sys.getenv("DATA_DIR")
output_path   <- Sys.getenv("OUTPUT_DIR")

source("src/utils.R")  # provides assign_FF12()


# Plot styling (Okabe-Ito palette + FDP factor levels) ------------------------

okabe_ito <- c("#0072B2", "#56B4E9", "#009E73", "#E69F00", "#D55E00", "#CC79A7")
desired_order  <- c("IBES", "ZACKS", "CIQ", "FSET", "BB", "ALL")
desired_order2 <- c("ALL",  "BB",   "FSET", "ZACKS", "CIQ", "IBES")


# Figure 3a: Coverage Over Time -----------------------------------------------

ccm <- read_parquet(glue("{data_path}/firm_qtr_2020andearlier_CCM.parquet"))

cov_summary <- function(df, group_var) {
  df |>
    group_by({{ group_var }}) |>
    summarize(
      IBES  = mean(!is.na(ibes_mean_a)),
      ZACKS = mean(!is.na(zacks_mean_a)),
      CIQ   = mean(!is.na(ciq_mean_a)),
      FSET  = mean(!is.na(fset_mean_a)),
      BB    = mean(!is.na(bb_mean_a)),
      ALL   = mean(complete.cases(ibes_mean_a, zacks_mean_a, ciq_mean_a,
                                  fset_mean_a, bb_mean_a)),
      .groups = "drop"
    )
}

time_plot_data <- ccm |>
  mutate(calyear = year(yearqtr)) |>
  cov_summary(calyear) |>
  pivot_longer(cols = -calyear, names_to = "fdp", values_to = "coverage") |>
  mutate(FDP = fct_relevel(factor(fdp), desired_order))

grand_means <- ccm |>
  cov_summary(NULL) |>
  pivot_longer(cols = everything(), names_to = "fdp", values_to = "mean_coverage") |>
  mutate(FDP = fct_relevel(factor(fdp), desired_order))

legend_labels <- grand_means |>
  mutate(label = glue("{FDP} ({percent(mean_coverage, accuracy = 0.1)})")) |>
  arrange(FDP) |>
  pull(label)

endpoints <- time_plot_data |> filter(calyear == min(calyear))

p <- time_plot_data |>
  ggplot(aes(x = calyear, y = coverage, color = FDP, shape = FDP, linetype = FDP)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text_repel(data = endpoints, aes(label = FDP),
                  color = "black", hjust = 1, direction = "y",
                  nudge_x = -0.3, segment.color = "gray50", segment.size = 0.3,
                  size = 3, show.legend = FALSE) +
  scale_color_manual(values = okabe_ito, labels = legend_labels) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 4), labels = legend_labels) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash",
                                   "longdash", "twodash"),
                        labels = legend_labels) +
  scale_x_continuous(breaks = breaks_width(width = 1),
                     expand = expansion(mult = c(0.06, 0.02))) +
  scale_y_continuous(breaks = breaks_pretty(n = 10), labels = percent) +
  labs(x = "Year", y = "Coverage",
       color = "Data Provider", shape = "Data Provider", linetype = "Data Provider") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = c(0.85, 0.25),
    legend.justification = c(0.5, 0.5),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(glue("{output_path}/coverage_time_plot.png"),
       plot = p, width = 8, height = 5, dpi = 300)
message(glue("    ✓ Wrote Figure 3a: {output_path}/coverage_time_plot.png"))


# Figure 3b: Coverage by Size Quintile ----------------------------------------

size_plot_data <- ccm |>
  group_by(yearqtr) |>
  mutate(qnt_size = ntile(atq, 5)) |>
  ungroup() |>
  cov_summary(qnt_size) |>
  pivot_longer(cols = -qnt_size, names_to = "fdp", values_to = "coverage") |>
  mutate(
    Size = factor(qnt_size,
                  labels = c("Q1 (Smallest)", "Q2", "Q3", "Q4", "Q5 (Largest)")),
    FDP  = fct_relevel(factor(fdp), desired_order2)
  )

pos          <- position_dodge(width = 0.8)
n_sizes      <- length(unique(size_plot_data$Size))
label_offset <- 0.08

p_size <- size_plot_data |>
  ggplot(aes(x = Size, y = coverage, color = FDP, shape = FDP, linetype = FDP)) +
  geom_vline(xintercept = seq(1.5, n_sizes - 0.5, by = 1),
             color = "gray80", linewidth = 0.3) +
  geom_linerange(aes(ymin = label_offset, ymax = coverage),
                 position = pos, linewidth = 0.5) +
  geom_point(position = pos, size = 3) +
  geom_text(aes(label = FDP, y = label_offset / 2),
            position = pos, size = 2.5, color = "black",
            fontface = "bold", show.legend = FALSE) +
  geom_text(aes(label = percent(coverage, accuracy = 0.1)),
            position = pos, hjust = -0.3, size = 3,
            color = "black", show.legend = FALSE) +
  coord_flip() +
  scale_color_manual(values = rev(okabe_ito)) +
  scale_shape_manual(values = rev(c(16, 17, 15, 18, 8, 4))) +
  scale_linetype_manual(values = rev(c("solid", "dashed", "dotted", "dotdash",
                                        "longdash", "twodash"))) +
  scale_y_continuous(labels = percent,
                     limits = c(0, max(size_plot_data$coverage) * 1.1)) +
  labs(x = "Size Quintile", y = "Coverage") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(glue("{output_path}/coverage_size_plot.png"),
       plot = p_size, width = 8, height = 6, dpi = 300)
message(glue("    ✓ Wrote Figure 3b: {output_path}/coverage_size_plot.png"))


# Figure 3c: Coverage by Industry ---------------------------------------------

ind_plot_data <- ccm |>
  filter(fic == "USA") |>
  mutate(ff12 = assign_FF12(as.numeric(sic))) |>
  cov_summary(ff12) |>
  pivot_longer(cols = -ff12, names_to = "fdp", values_to = "coverage") |>
  mutate(FDP = fct_relevel(factor(fdp), desired_order2)) |>
  group_by(ff12) |>
  mutate(all_coverage = coverage[FDP == "ALL"]) |>
  ungroup() |>
  mutate(Industry = fct_reorder(factor(ff12), all_coverage))

n_industries <- length(unique(ind_plot_data$Industry))

p_ind <- ind_plot_data |>
  ggplot(aes(x = Industry, y = coverage, color = FDP, shape = FDP, linetype = FDP)) +
  geom_vline(xintercept = seq(1.5, n_industries - 0.5, by = 1),
             color = "gray80", linewidth = 0.3) +
  geom_linerange(aes(ymin = label_offset, ymax = coverage),
                 position = pos, linewidth = 0.5) +
  geom_point(position = pos, size = 3) +
  geom_text(aes(label = FDP, y = label_offset / 2),
            position = pos, size = 2.5, color = "black",
            fontface = "bold", show.legend = FALSE) +
  geom_text(aes(label = percent(coverage, accuracy = 0.1)),
            position = pos, hjust = -0.3, size = 3,
            color = "black", show.legend = FALSE) +
  coord_flip() +
  scale_color_manual(values = rev(okabe_ito)) +
  scale_shape_manual(values = rev(c(16, 17, 15, 18, 8, 4))) +
  scale_linetype_manual(values = rev(c("solid", "dashed", "dotted", "dotdash",
                                        "longdash", "twodash"))) +
  scale_y_continuous(labels = percent,
                     limits = c(0, max(ind_plot_data$coverage) * 1.1)) +
  labs(x = "Industry", y = "Coverage") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(glue("{output_path}/coverage_industry_plot.png"),
       plot = p_ind, width = 8, height = 10, dpi = 300)
message(glue("    ✓ Wrote Figure 3c: {output_path}/coverage_industry_plot.png"))


# Figure 4: Mean Following by FDP Over Time -----------------------------------

regdata <- read_parquet(glue("{data_path}/2025_12_16_firm_qtr_regdata.parquet"))

following_summary <- function(df, group_var) {
  df |>
    group_by({{ group_var }}) |>
    summarize(
      IBES  = mean(ibes_following),
      ZACKS = mean(zacks_following),
      CIQ   = mean(ciq_following),
      FSET  = mean(fset_following),
      BB    = mean(bb_following),
      .groups = "drop"
    )
}

f_time_data <- regdata |>
  mutate(calyear = year(yearqtr)) |>
  following_summary(calyear) |>
  pivot_longer(cols = -calyear, names_to = "fdp", values_to = "following") |>
  mutate(FDP = fct_relevel(factor(fdp), desired_order[1:5]))

f_grand <- regdata |>
  following_summary(NULL) |>
  pivot_longer(cols = everything(), names_to = "fdp", values_to = "mean_following") |>
  mutate(FDP = fct_relevel(factor(fdp), desired_order[1:5]))

f_legend <- f_grand |>
  mutate(label = glue("{FDP} ({number(mean_following, accuracy = 0.1)})")) |>
  arrange(FDP) |>
  pull(label)

f_endpoints <- f_time_data |> filter(calyear == min(calyear))

p_f <- f_time_data |>
  ggplot(aes(x = calyear, y = following, color = FDP, shape = FDP, linetype = FDP)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text_repel(data = f_endpoints, aes(label = FDP),
                  color = "black", hjust = 1, direction = "y",
                  nudge_x = -0.3, segment.color = "gray50", segment.size = 0.3,
                  size = 3, show.legend = FALSE) +
  scale_color_manual(values = okabe_ito[1:5], labels = f_legend) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8), labels = f_legend) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash"),
                        labels = f_legend) +
  scale_x_continuous(breaks = breaks_width(width = 1),
                     expand = expansion(mult = c(0.06, 0.02))) +
  scale_y_continuous(breaks = breaks_pretty(n = 10), labels = number) +
  labs(x = "Year", y = "Mean Following",
       color = "Forecast Data Provider", shape = "Forecast Data Provider",
       linetype = "Forecast Data Provider") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = c(0.85, 0.45),
    legend.justification = c(0.5, 0.5),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(glue("{output_path}/following_time_plot.png"),
       plot = p_f, width = 8, height = 5, dpi = 300)
message(glue("    ✓ Wrote Figure 4: {output_path}/following_time_plot.png"))


# Figure 7: Cumulative event-time returns -------------------------------------

fdp_data <- read_parquet(glue("{data_path}/2025_12_16_fdp_firm_qtr_regdata.parquet")) |>
  filter(mbe_2 != 0) |>
  select(permno, best_anndats_adj, fdp_num, rank_avg_quality, mbe_2) |>
  mutate(
    fdp_num = unclass(fdp_num),
    mbe_2   = unclass(mbe_2)
  )

crsp_rets <- read_parquet(glue("{data_path}/wrds_crsp_returns.parquet"))

# Cumulative BHAR over the full chart window [-2, +30] -- keep all offsets
# so we can plot the trajectory.
cumret_chart <- crsp_rets |>
  filter(offset >= -2, offset <= 30) |>
  group_by(permno, best_anndats_adj) |>
  arrange(permno, best_anndats_adj, date) |>
  mutate(
    cum_ret   = cumprod(1 + ret) - 1,
    cum_vwret = cumprod(1 + vwretd) - 1,
    bhar      = cum_ret - cum_vwret
  ) |>
  ungroup()

# Figure 7a: by FDP identity --------------------------------------------------

fdp_chart <- cumret_chart |>
  inner_join(fdp_data, by = c("permno", "best_anndats_adj")) |>
  mutate(
    profit   = if_else(mbe_2 == 1, bhar, -bhar) * 100,
    fdp_name = case_when(
      fdp_num == 1 ~ "IBES",
      fdp_num == 2 ~ "Zacks",
      fdp_num == 3 ~ "CIQ",
      fdp_num == 4 ~ "Bloomberg",
      fdp_num == 5 ~ "FactSet"
    )
  ) |>
  group_by(fdp_name, offset) |>
  summarize(mean_profit = mean(profit, na.rm = TRUE), .groups = "drop") |>
  mutate(fdp_name = factor(fdp_name,
                           levels = c("IBES", "Zacks", "CIQ", "FactSet", "Bloomberg")))

fdp_colors <- c(IBES = okabe_ito[1], Zacks = okabe_ito[3], CIQ = okabe_ito[2],
                FactSet = okabe_ito[4], Bloomberg = okabe_ito[5])

p_fdp <- fdp_chart |>
  ggplot(aes(x = offset, y = mean_profit, color = fdp_name)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(linetype = fdp_name), linewidth = 0.8) +
  geom_point(aes(shape = fdp_name), size = 2) +
  scale_color_manual(values = fdp_colors) +
  scale_linetype_manual(values = c(IBES = "solid", Zacks = "dotted",
                                   CIQ = "dashed", FactSet = "dotdash",
                                   Bloomberg = "longdash")) +
  scale_shape_manual(values = c(IBES = 16, Zacks = 15, CIQ = 17,
                                FactSet = 18, Bloomberg = 8)) +
  labs(x = "Trading Days After Announcement",
       y = "Cumulative Return (%)",
       color = "Signal", linetype = "Signal", shape = "Signal") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "gray80"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1),
         shape = guide_legend(nrow = 1))

ggsave(glue("{output_path}/fdp_profit_chart.png"),
       plot = p_fdp, width = 12, height = 9, dpi = 300)
message(glue("    ✓ Wrote Figure 7a: {output_path}/fdp_profit_chart.png"))

# Figure 7b: by Quality Rank --------------------------------------------------

qual_chart <- cumret_chart |>
  inner_join(fdp_data, by = c("permno", "best_anndats_adj")) |>
  mutate(profit = if_else(mbe_2 == 1, bhar, -bhar) * 100) |>
  group_by(rank_avg_quality, offset) |>
  summarize(mean_profit = mean(profit, na.rm = TRUE), .groups = "drop") |>
  mutate(rank_label = factor(rank_avg_quality,
                             labels = c("0 (Worst)", "0.25", "0.5",
                                        "0.75", "1 (Best)")))

rank_colors <- c("0 (Worst)" = okabe_ito[5], "0.25" = okabe_ito[4],
                 "0.5" = okabe_ito[3], "0.75" = okabe_ito[2],
                 "1 (Best)" = okabe_ito[1])

p_qual <- qual_chart |>
  ggplot(aes(x = offset, y = mean_profit, color = rank_label, group = rank_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = rank_label), size = 2) +
  scale_color_manual(values = rank_colors) +
  scale_shape_manual(values = c("0 (Worst)" = 16, "0.25" = 17, "0.5" = 15,
                                "0.75" = 18, "1 (Best)" = 8)) +
  labs(x = "Trading Days After Announcement",
       y = "Cumulative Return (%)",
       color = "Quality Rank", shape = "Quality Rank") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "gray80"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))

ggsave(glue("{output_path}/quality_profit_chart.png"),
       plot = p_qual, width = 12, height = 9, dpi = 300)
message(glue("    ✓ Wrote Figure 7b: {output_path}/quality_profit_chart.png"))


# Figure 2: Earnings-related media mentions by FDP (Nexis Uni) ----------------
# Hand-collected by Stephannie Larocque. Each FDP search combines the FDP's
# name (and known aliases, accounting for FDP ownership / rebranding) with
# "EPS":
#   "EPS and Bloomberg"
#   "EPS and Capital IQ" or "EPS and S&P Capital IQ" or "EPS and S&P Global"
#     or "EPS and McGraw Hill" / "EPS and McGraw-Hill" / "...Financial",
#     and not "SPGI" (the public company that owns S&P Capital IQ)
#   "EPS and FactSet", and not "FDS" (the public company that owns FactSet)
#   "EPS and I/B/E/S" or "EPS and IBES" or "EPS and First Call" or
#     "EPS and Thomson" or "EPS and Reuters", and not "TRI" (the public
#     company that owned I/B/E/S during our sample period)
#   "EPS and Zacks" or "EPS and Zack's"

provider_levels <- c("IBES", "Zacks", "CIQ", "FactSet", "Bloomberg")
provider_colors <- c(IBES = "#0072B2", Zacks = "#56B4E9", CIQ = "#009E73",
                     FactSet = "#E69F00", Bloomberg = "#D55E00")

nexis_raw <- read_excel(glue("{raw_data_path}/NexisUniArticlesByYear.xlsx"),
                        sheet = 1)
colnames(nexis_raw)[1:6] <- c("year", "FactSet", "Bloomberg", "Zacks",
                              "CIQ", "IBES")

articles <- nexis_raw |>
  filter(!is.na(year)) |>
  mutate(year = as.integer(year)) |>
  filter(year <= 2020) |>
  pivot_longer(cols = FactSet:IBES, names_to = "provider", values_to = "articles") |>
  mutate(provider = factor(provider, levels = provider_levels)) |>
  drop_na(articles)

plot_nexis <- function(data) {
  ggplot(data, aes(x = year, y = articles, fill = provider, color = provider)) +
    geom_area(alpha = 0.9, linewidth = 0.4) +
    scale_fill_manual(values = provider_colors[levels(data$provider)]) +
    scale_color_manual(values = provider_colors[levels(data$provider)]) +
    scale_x_continuous(breaks = breaks_width(2)) +
    scale_y_continuous(labels = label_comma()) +
    labs(x = "Year", y = "Nexis Uni Articles",
         fill = NULL, color = NULL, title = NULL) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = "gray80"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 1), color = "none")
}

p_nexis_with    <- plot_nexis(articles)
p_nexis_without <- plot_nexis(articles |> filter(provider != "IBES"))

ggsave(glue("{output_path}/nexis_articles_by_year_with_IBES.png"),
       plot = p_nexis_with, width = 10, height = 5, dpi = 300)
message(glue("    ✓ Wrote Figure 2a: ",
             "{output_path}/nexis_articles_by_year_with_IBES.png"))

ggsave(glue("{output_path}/nexis_articles_by_year_no_IBES.png"),
       plot = p_nexis_without, width = 10, height = 5, dpi = 300)
message(glue("    ✓ Wrote Figure 2b: ",
             "{output_path}/nexis_articles_by_year_no_IBES.png"))


message("\n=== Script completed successfully ===")
