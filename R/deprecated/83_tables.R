
# Dependencies ------------------------------------------------------------

stopifnot(
  exists("variables"), # 55
  file.exists("txt/result-f_base_qu.csv"), # 56
  require("memisc")
)


# Produce LaTeX -----------------------------------------------------------

# c("base", "base_lags", "no_pop", "contemp", "no_yields", "no_land")
weights <- "qu"
model <- "base"

for(model in names(variables)) {

tbl <- read.csv(paste0("txt/result-f_", model, "_", weights, ".csv"),
  stringsAsFactors = FALSE)

tbl$X <- NULL
tbl[, -1] <- round(tbl[, -1], 2)

idx_ind <- grepl(".*_ind", tbl$vars)
tbl_ind <- tbl[idx_ind, ]
tbl <- tbl[!idx_ind, ]

for(col in c("sdm", "sar", "slx"))
  tbl[[paste0(col, "_ind")]] <- c(tbl_ind[[col]], rep(NA, 5))

tbl[is.na(tbl)] <- ""

tbl <- tbl[, c("vars", "sdm", "sdm_ind", "sar", "sar_ind", "slx", "slx_ind", "clm")]

tbl <- rbind(c("", rep(c("Direct", "Indirect"), 3), ""), tbl)
colnames(tbl) <- c("", "SDM", "", "SAR", "", "SLX", "", "CLM")

# Add *
stars <- read.csv(paste0("txt/result_", model, "_", weights, ".csv"),
  stringsAsFactors = FALSE)

stars$X <- NULL
stars[seq(grep("rho", stars$vars) + 1, nrow(stars)), ] <- NA

stars_ind <- grepl(".*_ind", stars$vars)
stars_ind <- stars[idx_ind, ]
stars <- stars[!idx_ind, ]

for(col in c("sdm", "sar", "slx"))
  stars[[paste0(col, "_ind")]] <- c(stars_ind[[col]], rep(NA, 5))

stars <- stars[, c("vars", "sdm", "sdm_ind", "sar", "sar_ind", "slx", "slx_ind", "clm")]
stars <- sapply(stars, function(x) gsub(".*([0-3])\\*$", "\\1", x))
stars[is.na(stars)] <- 0

for(j in seq(2, ncol(tbl))) {
  for(i in seq(nrow(stars))) {
    tbl[i + 1, j] <- paste0(tbl[i + 1, j], " ",
      paste0(rep("*", as.numeric(stars[i, j])), collapse = ""))
  }
}

out <- c("\\begin{table}[ht]", "\\resizebox{\\textwidth}{!}{%",
  toLatex(tbl, row.names = FALSE), "} \\caption{\\label{tab:}}", "\\end{table}")

writeLines(out, paste0("txt/latex_tbl/tbl_", model, "_", weights, ".txt"))

}
