# ---------------------------------------------------------------------------
# Drop-in fix for MIMICbook + dplyr >= 0.7 / ggplot2 >= 3.0
#
# MIMICbook (last updated 2016) builds its plots with dplyr's underscore verbs
# --- mutate_(), group_by_(), rename_() --- and with group_indices() and
# ggplot2::aes_string(). All of these are defunct in current dplyr/ggplot2, so
# plot_prop_by_level() and plot_OR_by_level() error out.
#
# Sourcing this file AFTER library(MIMICbook) defines corrected versions in the
# global environment, which shadow the package's broken ones. No need to
# downgrade dplyr and no need to reinstall MIMICbook.
#
#     library(MIMICbook)
#     source("mimicbook_dplyr_fix.R")
#
# The other MIMICbook functions the workshop uses (convert.bin.fac,
# produce.table1) do not touch the deprecated API and are left alone.
# ---------------------------------------------------------------------------

# These replacements live in the global environment rather than inside the
# MIMICbook namespace, so they do not inherit the package's imports. library()
# on a package attaches it but not its dependencies, so %>% is not necessarily
# on the search path at this point. Bind it explicitly.
`%>%` <- dplyr::`%>%`

plot_prop_by_level <- function(dat, factor.var1, prop.var, alpha = 0.05,
                               factor.var2 = NULL, ylab = "Proportion") {

  group.vars <- c(factor.var1, factor.var2)

  toplot <- dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group.vars))) %>%
    dplyr::mutate(prop.var2 = as.numeric(as.character(.data[[prop.var]]))) %>%
    dplyr::summarise(n = dplyr::n(),
                     prop = mean(prop.var2, na.rm = TRUE),
                     LL = prop - qnorm(1 - alpha/2) * sqrt(prop * (1 - prop)/n),
                     UL = prop + qnorm(1 - alpha/2) * sqrt(prop * (1 - prop)/n),
                     .groups = "drop")

  if (is.null(factor.var2)) {
    ggplot2::ggplot(toplot, ggplot2::aes(x = .data[[factor.var1]], y = .data$prop)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$UL, ymin = .data$LL), width = 0.1) +
      ggplot2::ylab(ylab)
  } else {
    ggplot2::ggplot(toplot, ggplot2::aes(x = .data[[factor.var1]], y = .data$prop,
                                         col = .data[[factor.var2]],
                                         group = .data[[factor.var2]])) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$UL, ymin = .data$LL), width = 0.1,
                             alpha = 0.6, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::ylab(ylab)
  }
}

plot_odds_by_level <- function(dat, factor.var1, prop.var, alpha = 0.05,
                               factor.var2 = NULL, ylab = "Odds") {

  group.vars <- c(factor.var1, factor.var2)

  toplot <- dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group.vars))) %>%
    dplyr::mutate(prop.var2 = as.numeric(as.character(.data[[prop.var]]))) %>%
    dplyr::summarise(n = dplyr::n(),
                     prop = mean(prop.var2, na.rm = TRUE),
                     odds = prop/(1 - prop),
                     LL = if (is.null(factor.var2)) prop - qnorm(1 - alpha/2) * sqrt(prop/(1 - prop)^3/n) else prop - qnorm(1 - alpha/2) * sqrt(prop * (1 - prop)/n),
                     UL = if (is.null(factor.var2)) prop + qnorm(1 - alpha/2) * sqrt(prop/(1 - prop)^3/n) else prop + qnorm(1 - alpha/2) * sqrt(prop * (1 - prop)/n),
                     .groups = "drop")

  if (is.null(factor.var2)) {
    ggplot2::ggplot(toplot, ggplot2::aes(x = .data[[factor.var1]], y = .data$prop)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$UL, ymin = .data$LL), width = 0.1) +
      ggplot2::ylab(ylab)
  } else {
    ggplot2::ggplot(toplot, ggplot2::aes(x = .data[[factor.var1]], y = .data$prop,
                                         col = .data[[factor.var2]],
                                         group = .data[[factor.var2]])) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$UL, ymin = .data$LL), width = 0.1,
                             alpha = 0.6, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::ylab(ylab)
  }
}

plot_OR_by_level <- function(dat, factor.var1, prop.var, alpha = 0.05,
                             factor.var2 = NULL, ylab = "Proportion",
                             ref.group = 1, include.ref.group.effect = TRUE) {

  # stands in for the defunct dplyr::rename_()
  rename_col <- function(dat, old, new) {
    names(dat)[names(dat) == old] <- new
    dat
  }
  # per-row group index, as the defunct dplyr::group_indices() produced for a
  # single grouping variable (groups ordered by sorted key, unused levels dropped)
  group_index <- function(x) as.integer(droplevels(as.factor(x)))

  if (is.null(factor.var2)) {
    toplot <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(factor.var1))) %>%
      dplyr::mutate(prop.var2 = as.numeric(as.character(.data[[prop.var]]))) %>%
      dplyr::summarise(n = dplyr::n(), prop = mean(prop.var2, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(dummy = 1)
    toplot <- toplot %>%
      # the all-pairs cross join is deliberate: every level is compared to the
      # reference level, so declare it rather than tripping dplyr's warning
      dplyr::full_join(rename_col(toplot, factor.var1, "levelname1"), by = "dummy",
                       relationship = "many-to-many") %>%
      dplyr::select(-dummy)
    toplot <- toplot %>%
      dplyr::filter(group_index(toplot[[factor.var1]]) == ref.group) %>%
      dplyr::mutate(c = n.x*prop.x, d = n.x*(1-prop.x), a = n.y*prop.y, b = n.y*(1-prop.y),
                    OR = a*d/(b*c), logOR = log(OR),
                    selogOR = sqrt(1/a + 1/b + 1/c + 1/d),
                    UL = exp(log(OR) + qnorm(1-alpha/2)*selogOR),
                    LL = exp(log(OR) - qnorm(1-alpha/2)*selogOR))
    toplot <- rename_col(toplot, factor.var1, "tmp") %>%
      dplyr::mutate(UL = ifelse(tmp != levelname1, UL, 1),
                    LL = ifelse(tmp != levelname1, LL, 1))
    if (!include.ref.group.effect) {
      toplot <- toplot %>% dplyr::filter(levelname1 != tmp)
    }
    ggplot2::ggplot(toplot, ggplot2::aes(x = .data$levelname1, y = .data$OR)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$UL, ymin = .data$LL), width = 0.1) +
      ggplot2::xlab(factor.var1) + ggplot2::ylab("OR") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1), linetype = 3, alpha = 0.5)
  } else {
    toplot <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(factor.var1, factor.var2)))) %>%
      dplyr::mutate(prop.var2 = as.numeric(as.character(.data[[prop.var]]))) %>%
      dplyr::summarise(n = dplyr::n(), prop = mean(prop.var2, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(dummy = 1)
    ccc <- c("dummy", "levelname1")
    names(ccc) <- c("dummy", factor.var1)
    toplot <- rename_col(toplot, factor.var2, "var2") %>%
      dplyr::inner_join(rename_col(toplot, factor.var1, "levelname1"), by = ccc,
                        relationship = "many-to-many") %>%
      dplyr::select(-dummy)
    keep <- group_index(toplot$var2) == ref.group
    toplot <- toplot %>% dplyr::ungroup() %>% dplyr::filter(keep) %>%
      dplyr::mutate(c = n.x*prop.x, d = n.x*(1-prop.x), a = n.y*prop.y, b = n.y*(1-prop.y),
                    OR = a*d/(b*c), logOR = log(OR),
                    selogOR = sqrt(1/a + 1/b + 1/c + 1/d),
                    UL = exp(log(OR) + qnorm(1-alpha/2)*selogOR),
                    LL = exp(log(OR) - qnorm(1-alpha/2)*selogOR))
    toplot <- rename_col(toplot, factor.var2, "tmp") %>%
      dplyr::mutate(UL = ifelse(tmp != var2, UL, 1), LL = ifelse(tmp != var2, LL, 1))
    if (!include.ref.group.effect) {
      toplot <- toplot %>% dplyr::filter(tmp != var2)
    }
    ggplot2::ggplot(toplot, ggplot2::aes(x = .data$tmp, y = .data$OR,
                                         col = .data[[factor.var1]])) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$UL, ymin = .data$LL), width = 0.1,
                             position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::xlab(factor.var2) + ggplot2::ylab("OR") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1), linetype = 3, alpha = 0.5)
  }
}
