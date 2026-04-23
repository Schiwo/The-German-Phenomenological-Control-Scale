# Overview

This notebook analyses the German PC scale validation dataset.

<table>
<colgroup>
<col style="width: 8%" />
<col style="width: 91%" />
</colgroup>
<thead>
<tr>
<th>Variable</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td>SK01</td>
<td>Hand lowering (0 = Normal severity, 5 = Very severe)</td>
</tr>
<tr>
<td>SK02</td>
<td>Moving hands together (0 = No force, 5 = Strong force)</td>
</tr>
<tr>
<td>SK03</td>
<td>Mosquito hallucination (0 = No mosquito, 5 = Like a real
mosquito)</td>
</tr>
<tr>
<td>SK04</td>
<td>Sweet taste (0 = No taste, 5 = Strong taste)</td>
</tr>
<tr>
<td>SK05</td>
<td>Sour taste (0 = No taste, 5 = Strong taste)</td>
</tr>
<tr>
<td>SK04SK05</td>
<td>Taste mean (calculated in the script)</td>
</tr>
<tr>
<td>SK06</td>
<td>Arm rigidity (0 = Normal / no stiffness, 5 = Very stiff)</td>
</tr>
<tr>
<td>SK07</td>
<td>Arm immobilization (0 = Normal / no heaviness, 5 = Very heavy)</td>
</tr>
<tr>
<td>SK08</td>
<td>Music hallucination (0 = No music, 5 = Clear music)</td>
</tr>
<tr>
<td>SK09</td>
<td>Negative visual hallucination (0 = Three balls seen, 5 = Two balls
seen)</td>
</tr>
<tr>
<td>SK10</td>
<td>Amnesia (0 = Normal memory, 5 = No memory)</td>
</tr>
<tr>
<td>SK12</td>
<td>Urge to press space bar (0 = No urge, 5 = Clear urge)</td>
</tr>
<tr>
<td>SK13</td>
<td>Space bar amnesia (0 = Normal memory of instructions, 5 = No memory
of instructions)</td>
</tr>
<tr>
<td>SK12SK13</td>
<td>Space bar mean (calculated in the script)</td>
</tr>
<tr>
<td>KK02_01</td>
<td>Items remembered before being asked again</td>
</tr>
<tr>
<td>KK03_01</td>
<td>Items remembered after being asked again</td>
</tr>
<tr>
<td>SO01</td>
<td>Gender (1 = female, 2 = male, 3 = diverse)</td>
</tr>
<tr>
<td>SO02_01</td>
<td>Age</td>
</tr>
<tr>
<td>SC02</td>
<td>Answered all questions seriously (0 = yes, 2 = no)</td>
</tr>
<tr>
<td>TIME005</td>
<td>Time needed to complete the experiment (seconds)</td>
</tr>
</tbody>
</table>

## Import libraries

    knitr::opts_chunk$set(
      echo = TRUE,
      warning = FALSE,
      message = FALSE,
      fig.width = 8,
      fig.height = 5,
      fig.align = "center"
    )

    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(Hmisc)

    ## 
    ## Attache Paket: 'Hmisc'
    ## 
    ## Die folgenden Objekte sind maskiert von 'package:dplyr':
    ## 
    ##     src, summarize
    ## 
    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     format.pval, units

    library(psych)

    ## 
    ## Attache Paket: 'psych'
    ## 
    ## Das folgende Objekt ist maskiert 'package:Hmisc':
    ## 
    ##     describe
    ## 
    ## Die folgenden Objekte sind maskiert von 'package:ggplot2':
    ## 
    ##     %+%, alpha

    library(ggdist)
    library(patchwork)
    library(ggthemr)

    ggthemr("flat")

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the ggthemr package.
    ##   Please report the issue to the authors.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the ggthemr package.
    ##   Please report the issue to the authors.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

## Read data

    dat <- read.csv("GitHub/german_pc_scale_validation_data.CSV",
                    sep = ";")

## Exclusion Criteria

    # Participant answered all questions meaningfully
    dat |> count(SC02)

    ##   SC02   n
    ## 1    1 355
    ## 2    2   2

    outlierMeaningful <- dat[dat$SC02 == 2 ,]$CASE


    # Exclude participants how have taken less than 15min for the study (indicating that they skipped parts of the videos)
    timeTaken <- dat$TIME005
    timeTaken <- timeTaken[!is.na(timeTaken)]
    lower_bound <- min(timeTaken, 800) - 1e-8
    upper_bound <- max(timeTaken, 1500) + 1e-8


    breaks <- c(lower_bound, seq(800, 1500, by = 100), upper_bound)

    ggplot(data.frame(x = timeTaken), aes(x = x)) +
      geom_histogram(breaks = breaks,
                     closed = "left") +
      geom_vline(xintercept = 900, color = "red", size = 1) +
      coord_cartesian(xlim = c(800, 1500)) +
      labs(x = "Time taken",
           y = "Count")

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/exclusions-1.png" style="display: block; margin: auto;" />

    outlierTime <- dat[dat$TIME005 < 900 ,]$CASE

    # remove outlier from dataset
    dat <- dat[! dat$CASE %in% outlierMeaningful, ]
    dat <- dat[! dat$CASE %in% outlierTime, ]

## Prepare dataframe

    # Refactor columns to factors 
    cols <- c("SK01","SK02","SK03","SK04","SK05", "SK06","SK07","SK08","SK09","SK10", "SK12","SK13") 
    dat[cols] <- lapply(dat[cols], function(x) as.numeric(x) - 1)

    # Label data
    label(dat$SK01) <- "Hand lowering"
    label(dat$SK02) <- "Moving hands together"
    label(dat$SK03) <- "Mosquito hallucination"
    label(dat$SK04) <- "Sweet taste"
    label(dat$SK05) <- "Sour taste"
    label(dat$SK06) <- "Arm rigidity"
    label(dat$SK07) <- "Arm immobilization"
    label(dat$SK08) <- "Music hallucination"
    label(dat$SK09) <- "Negative visual hallucination"
    label(dat$SK10) <- "Amnesia"
    label(dat$SK12) <- "Urge to press space bar"
    label(dat$SK13) <- "Space bar amnesia"

    label(dat$KK02_01) <- "Items remembered before being asked again"
    label(dat$KK03_01) <- "Items remembered after being asked again"

    label(dat$SO01) <- "Gender"
    label(dat$SO02_01) <- "Age"

    label(dat$SC02) <- "Answered all questions seriously"

    label(dat$TIME005) <- "Completion time (experiment duration)"

    # calculate item 4 (arithmetic mean of taste sweet and sour) and 10 (geometric mean of Urge to press space bar and space bar amnesia)
    dat <- dat |> mutate(SK04SK05 = (SK04 + SK05) / 2)
    label(dat$SK04SK05) <- "Taste Halluctionation"

    dat <- dat |> mutate(SK12SK13 = sqrt(SK12 * SK13))
    label(dat$SK12SK13) <- "Post-session suggestion"

## Single item descriptives

    #function to get bootstrapped drop item cronbach's alpha

    alpha_drop_boot_ci <- function(df, B = 2000, conf = 0.95, seed = NULL, warnings = FALSE, ...) {
      if (!is.null(seed)) {
        set.seed(seed)
      }

      if (!requireNamespace("psych", quietly = TRUE)) {
        stop("Package 'psych' must be installed.")
      }

      if (!is.data.frame(df) && !is.matrix(df)) {
        stop("'df' must be a data frame or matrix containing only the scale items.")
      }

      df <- as.data.frame(df)

      if (ncol(df) < 2) {
        stop("At least 2 items are required.")
      }

      if (!all(vapply(df, is.numeric, logical(1)))) {
        stop("All columns in 'df' must be numeric.")
      }

      if (!is.numeric(B) || length(B) != 1 || B < 1) {
        stop("'B' must be a positive integer.")
      }
      B <- as.integer(B)

      if (!is.numeric(conf) || length(conf) != 1 || conf <= 0 || conf >= 1) {
        stop("'conf' must be a single number between 0 and 1.")
      }

      alpha_level <- 1 - conf
      probs <- c(alpha_level / 2, 1 - alpha_level / 2)

      # Original estimates
      a0 <- psych::alpha(df, warnings = warnings, ...)
      item_names <- rownames(a0$alpha.drop)

      drop_est <- a0$alpha.drop$raw_alpha
      names(drop_est) <- item_names

      # Bootstrap matrix: rows = resamples, cols = items
      boot_drop <- matrix(NA_real_, nrow = B, ncol = length(drop_est))
      colnames(boot_drop) <- item_names

      for (b in seq_len(B)) {
        idx <- sample.int(nrow(df), size = nrow(df), replace = TRUE)

        boot_drop[b, ] <- tryCatch({
          ab <- psych::alpha(df[idx, , drop = FALSE], warnings = warnings, ...)
          vals <- ab$alpha.drop$raw_alpha
          names(vals) <- rownames(ab$alpha.drop)
          vals <- vals[item_names]
          as.numeric(vals)
        }, error = function(e) {
          rep(NA_real_, length(drop_est))
        })
      }

      # Percentile bootstrap CIs
      cis <- t(apply(boot_drop, 2, function(x) {
        if (all(is.na(x))) {
          c(NA_real_, NA_real_)
        } else {
          quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
        }
      }))

      out <- data.frame(
        item_dropped = item_names,
        alpha_if_dropped = as.numeric(drop_est[item_names]),
        ci_lower = cis[, 1],
        ci_upper = cis[, 2],
        row.names = NULL
      )

      out
    }

    # Items to include
    items <- c("SK01","SK02","SK03","SK04SK05","SK06","SK07","SK08","SK09","SK10","SK12SK13")
      # c("SK01","SK02","SK03","SK04","SK05",
      #          "SK06","SK07","SK08","SK09","SK10",
      #          "SK12","SK13","SK04SK05","SK12SK13")

    # Subset data
    dat_items <- dat[, items]

    # Cronbach's alpha
    alpha_res <- psych::alpha(dat_items, n.iter=5000)

    # 95% C for # Cronbach's alpha
    alpha_res$boot.ci

    ##      2.5%       50%     97.5% 
    ## 0.6672365 0.7248508 0.7696223

    # Extract stats
    means <- colMeans(dat_items, na.rm = TRUE)
    sds   <- apply(dat_items, 2, sd, na.rm = TRUE)

    # Skewness and Kurtosis
    skew  <- apply(dat_items, 2, psych::skew, na.rm = TRUE)
    kurt  <- apply(dat_items, 2, psych::kurtosi, na.rm = TRUE)


    # Alpha if item removed
    alpha_drop <- alpha_res$alpha.drop

    # Bootstrap 95% CI for alpha-if-item-dropped
    drop_boot <- alpha_drop_boot_ci(dat_items, B = 5000, seed = 123)
    drop_boot

    ##    item_dropped alpha_if_dropped  ci_lower  ci_upper
    ## 1          SK01        0.6813914 0.6189381 0.7315794
    ## 2          SK02        0.6985764 0.6396859 0.7477490
    ## 3          SK03        0.6991423 0.6371924 0.7476545
    ## 4      SK04SK05        0.7065044 0.6491760 0.7537262
    ## 5          SK06        0.6617993 0.5928352 0.7180531
    ## 6          SK07        0.6730306 0.6069757 0.7270142
    ## 7          SK08        0.7248590 0.6704752 0.7681473
    ## 8          SK09        0.7283308 0.6735217 0.7730515
    ## 9          SK10        0.7089222 0.6527672 0.7535490
    ## 10     SK12SK13        0.7393035 0.6890395 0.7799319

    # Calculate PC score per person
    PC <- dat |>
      mutate(
        PC = rowMeans(
          across(c(SK01, SK02, SK03,
                   SK06, SK07, SK08, SK09, SK10,
                   SK04SK05, SK12SK13))
        )
      ) |>
      select(CASE, PC)

    # Item-total correlation: each item correlated with PC
    item_total <- sapply(items, function(x) {
      cor(dat[[x]], PC$PC, use = "pairwise.complete.obs" , method = "pearson")
    })

    result <- data.frame(
      item_code  = items,
      Item       = sapply(items, function(x) Hmisc::label(dat[[x]])),
      Mean       = round(means, 2),
      SD         = round(sds, 2),
      Skew       = round(skew, 2),
      Kurt       = round(kurt, 2),
      Alpha      = round(alpha_drop$raw_alpha, 2),
      CI_Low     = round(drop_boot$ci_lower, 2),
      CI_Up      = round(drop_boot$ci_upper, 2),
      ItemTotal  = round(item_total, 2)
    )

    result 

    ##          item_code                          Item Mean   SD  Skew  Kurt Alpha
    ## SK01          SK01                 Hand lowering 3.56 1.28 -0.91  0.30  0.68
    ## SK02          SK02         Moving hands together 3.10 1.46 -0.53 -0.59  0.70
    ## SK03          SK03        Mosquito hallucination 1.54 1.44  0.62 -0.67  0.70
    ## SK04SK05  SK04SK05         Taste Halluctionation 1.87 1.46  0.31 -1.09  0.71
    ## SK06          SK06                  Arm rigidity 2.92 1.59 -0.38 -0.97  0.66
    ## SK07          SK07            Arm immobilization 2.52 1.51 -0.08 -1.05  0.67
    ## SK08          SK08           Music hallucination 0.58 1.16  2.02  2.96  0.72
    ## SK09          SK09 Negative visual hallucination 0.34 1.00  3.30 10.50  0.73
    ## SK10          SK10                       Amnesia 1.23 1.28  0.79 -0.42  0.71
    ## SK12SK13  SK12SK13       Post-session suggestion 0.81 1.31  1.53  1.46  0.74
    ##          CI_Low CI_Up ItemTotal
    ## SK01       0.62  0.73      0.65
    ## SK02       0.64  0.75      0.57
    ## SK03       0.64  0.75      0.57
    ## SK04SK05   0.65  0.75      0.54
    ## SK06       0.59  0.72      0.73
    ## SK07       0.61  0.73      0.69
    ## SK08       0.67  0.77      0.38
    ## SK09       0.67  0.77      0.32
    ## SK10       0.65  0.75      0.50
    ## SK12SK13   0.69  0.78      0.32

    a0 <- psych::alpha(dat_items, warnings = TRUE)
    a0

    ## 
    ## Reliability analysis   
    ## Call: psych::alpha(x = dat_items, warnings = TRUE)
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
    ##       0.73      0.71    0.73       0.2 2.5 0.023  1.8 0.73     0.16
    ## 
    ##     95% confidence boundaries 
    ##          lower alpha upper
    ## Feldt     0.68  0.73  0.77
    ## Duhachek  0.68  0.73  0.77
    ## 
    ##  Reliability if an item is dropped:
    ##          raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ## SK01          0.68      0.67    0.68      0.18 2.0    0.027 0.017  0.15
    ## SK02          0.70      0.69    0.70      0.20 2.2    0.025 0.019  0.17
    ## SK03          0.70      0.68    0.71      0.19 2.2    0.025 0.027  0.15
    ## SK04SK05      0.71      0.69    0.71      0.20 2.2    0.024 0.028  0.15
    ## SK06          0.66      0.65    0.67      0.17 1.9    0.029 0.017  0.15
    ## SK07          0.67      0.66    0.68      0.18 2.0    0.028 0.020  0.15
    ## SK08          0.72      0.71    0.73      0.22 2.5    0.023 0.026  0.18
    ## SK09          0.73      0.72    0.74      0.22 2.6    0.023 0.026  0.18
    ## SK10          0.71      0.69    0.71      0.20 2.3    0.024 0.029  0.14
    ## SK12SK13      0.74      0.73    0.74      0.23 2.7    0.022 0.024  0.18
    ## 
    ##  Item statistics 
    ##            n raw.r std.r r.cor r.drop mean  sd
    ## SK01     299  0.65  0.64  0.62   0.53 3.56 1.3
    ## SK02     299  0.57  0.55  0.50   0.42 3.10 1.5
    ## SK03     299  0.57  0.56  0.48   0.41 1.54 1.4
    ## SK04SK05 299  0.54  0.53  0.43   0.37 1.87 1.5
    ## SK06     299  0.73  0.70  0.70   0.60 2.92 1.6
    ## SK07     299  0.69  0.66  0.64   0.55 2.52 1.5
    ## SK08     299  0.38  0.41  0.29   0.23 0.58 1.2
    ## SK09     299  0.32  0.38  0.23   0.19 0.34 1.0
    ## SK10     299  0.50  0.52  0.42   0.35 1.23 1.3
    ## SK12SK13 299  0.32  0.34  0.19   0.14 0.81 1.3

    a0$alpha.drop

    ##          raw_alpha std.alpha   G6(smc) average_r      S/N   alpha se      var.r
    ## SK01     0.6813914 0.6689793 0.6824866 0.1833742 2.020959 0.02679747 0.01714752
    ## SK02     0.6985764 0.6868869 0.7007779 0.1959787 2.193734 0.02531953 0.01939261
    ## SK03     0.6991423 0.6849270 0.7065579 0.1945492 2.173867 0.02508316 0.02716684
    ## SK04SK05 0.7065044 0.6915271 0.7148184 0.1994148 2.241776 0.02444783 0.02788187
    ## SK06     0.6617993 0.6545918 0.6706435 0.1739425 1.895125 0.02883849 0.01679203
    ## SK07     0.6730306 0.6631079 0.6826215 0.1794542 1.968309 0.02768388 0.01958177
    ## SK08     0.7248590 0.7133952 0.7309529 0.2166505 2.489125 0.02300136 0.02631075
    ## SK09     0.7283308 0.7195297 0.7378468 0.2218195 2.565440 0.02286468 0.02621202
    ## SK10     0.7089222 0.6931656 0.7137543 0.2006457 2.259087 0.02409787 0.02903124
    ## SK12SK13 0.7393035 0.7260554 0.7396661 0.2274926 2.650374 0.02169691 0.02368096
    ##              med.r
    ## SK01     0.1547769
    ## SK02     0.1724158
    ## SK03     0.1547769
    ## SK04SK05 0.1548755
    ## SK06     0.1547769
    ## SK07     0.1547769
    ## SK08     0.1780790
    ## SK09     0.1838921
    ## SK10     0.1439322
    ## SK12SK13 0.1838921

    a0$total

    ##  raw_alpha std.alpha   G6(smc) average_r      S/N        ase     mean        sd
    ##  0.7254278 0.7134321 0.7340166 0.1993322 2.489574 0.02285121 1.846533 0.7304389
    ##   median_r
    ##  0.1579657

    # Calculacte PC Score per person
    PC <- dat |> mutate(PC = rowMeans(across(c(SK01,SK02,SK03,
               SK06,SK07,SK08,SK09,SK10,
               SK04SK05,SK12SK13)))) |> 
      select(CASE, PC)

    PC

    ##     CASE        PC
    ## 1      1 1.8000000
    ## 2      2 1.6000000
    ## 3      3 1.3000000
    ## 5      5 2.0000000
    ## 6      6 1.7500000
    ## 7      7 1.6500000
    ## 9      9 2.1500000
    ## 10    10 1.4000000
    ## 11    11 2.2828427
    ## 13    13 2.4000000
    ## 14    14 2.4472136
    ## 15    15 1.9732051
    ## 16    16 2.5949490
    ## 17    17 1.5500000
    ## 18    18 1.7000000
    ## 19    19 2.5000000
    ## 20    20 2.4500000
    ## 21    21 1.7472136
    ## 22    22 1.5500000
    ## 23    23 2.4500000
    ## 24    24 1.2500000
    ## 28    28 1.8500000
    ## 31    31 1.5500000
    ## 32    32 1.8500000
    ## 33    33 1.5500000
    ## 34    34 2.4500000
    ## 35    35 1.9414214
    ## 36    36 2.3000000
    ## 37    37 1.7662278
    ## 38    38 2.3500000
    ## 39    39 1.8000000
    ## 41    41 2.3000000
    ## 42    42 2.0500000
    ## 46    46 2.2232051
    ## 47    47 1.4000000
    ## 48    48 2.3000000
    ## 50    50 1.9000000
    ## 51    51 2.6000000
    ## 52    52 1.5500000
    ## 54    54 3.7162278
    ## 55    55 2.7000000
    ## 57    57 2.2000000
    ## 58    58 2.4500000
    ## 59    59 2.5232051
    ## 61    61 2.0500000
    ## 63    63 2.4000000
    ## 64    64 1.6000000
    ## 65    65 0.2000000
    ## 67    67 1.1000000
    ## 68    68 0.9500000
    ## 70    70 2.3000000
    ## 71    71 0.8732051
    ## 72    72 1.6500000
    ## 73    73 1.6000000
    ## 74    74 1.4000000
    ## 75    75 0.4500000
    ## 76    76 2.4000000
    ## 77    77 0.7000000
    ## 78    78 1.3162278
    ## 82    82 2.8000000
    ## 83    83 1.0732051
    ## 84    84 1.0000000
    ## 85    85 1.9000000
    ## 86    86 2.8328427
    ## 87    87 0.3500000
    ## 88    88 1.1500000
    ## 89    89 2.1000000
    ## 90    90 1.8000000
    ## 91    91 0.8000000
    ## 92    92 2.3500000
    ## 93    93 1.2000000
    ## 94    94 4.1872983
    ## 95    95 2.0914214
    ## 96    96 2.9500000
    ## 97    97 3.4464102
    ## 98    98 2.1000000
    ## 99    99 0.3000000
    ## 100  100 2.5414214
    ## 101  101 0.8949490
    ## 102  102 0.9500000
    ## 103  103 2.8500000
    ## 104  104 1.6500000
    ## 105  105 1.6000000
    ## 106  106 2.5000000
    ## 107  107 2.1500000
    ## 108  108 1.4000000
    ## 109  109 0.0000000
    ## 110  110 1.2500000
    ## 112  112 1.9500000
    ## 113  113 2.4000000
    ## 114  114 2.3000000
    ## 115  115 1.7000000
    ## 116  116 1.9000000
    ## 117  117 1.3162278
    ## 119  119 2.3736068
    ## 121  121 2.3000000
    ## 122  122 0.9000000
    ## 123  123 2.5500000
    ## 124  124 1.3000000
    ## 125  125 2.2500000
    ## 126  126 2.0500000
    ## 127  127 1.4000000
    ## 128  128 2.3000000
    ## 129  129 2.4964102
    ## 130  130 2.6828427
    ## 131  131 0.7500000
    ## 132  132 2.0000000
    ## 133  133 1.9236068
    ## 134  134 1.4000000
    ## 135  135 2.3662278
    ## 136  136 2.3500000
    ## 137  137 2.3000000
    ## 138  138 1.4236068
    ## 139  139 3.4500000
    ## 140  140 2.0000000
    ## 141  141 1.9500000
    ## 142  142 2.8162278
    ## 143  143 0.6000000
    ## 144  144 1.0000000
    ## 145  145 2.4000000
    ## 146  146 1.6500000
    ## 147  147 1.1000000
    ## 148  148 1.8500000
    ## 149  149 3.2449490
    ## 150  150 0.8414214
    ## 152  152 0.9500000
    ## 153  153 1.3964102
    ## 154  154 1.9914214
    ## 155  155 1.5500000
    ## 156  156 1.7000000
    ## 158  158 1.2000000
    ## 159  159 1.4000000
    ## 160  160 1.2000000
    ## 162  162 1.9000000
    ## 163  163 2.3949490
    ## 164  164 0.5000000
    ## 165  165 2.1000000
    ## 166  166 1.8500000
    ## 167  167 1.6500000
    ## 168  168 3.6500000
    ## 169  169 1.6500000
    ## 170  170 2.0500000
    ## 171  171 1.6414214
    ## 172  172 0.9000000
    ## 173  173 0.3000000
    ## 174  174 2.9162278
    ## 175  175 2.5000000
    ## 176  176 1.9732051
    ## 177  177 1.9000000
    ## 178  178 2.6000000
    ## 179  179 1.7500000
    ## 181  181 1.7500000
    ## 182  182 2.6000000
    ## 183  183 1.8000000
    ## 184  184 1.1500000
    ## 185  185 2.2000000
    ## 186  186 2.5500000
    ## 187  187 1.2000000
    ## 188  188 1.8000000
    ## 189  189 2.1000000
    ## 190  190 2.1500000
    ## 191  191 1.6232051
    ## 192  192 2.4500000
    ## 193  193 1.7500000
    ## 194  194 2.7000000
    ## 195  195 1.0000000
    ## 196  196 1.0000000
    ## 197  197 1.5000000
    ## 198  198 2.5736068
    ## 199  199 2.1000000
    ## 200  200 0.4000000
    ## 201  201 3.2914214
    ## 202  202 2.6449490
    ## 203  203 1.2914214
    ## 204  204 2.4000000
    ## 206  206 1.9500000
    ## 207  207 2.8464102
    ## 208  208 2.4500000
    ## 210  210 1.8000000
    ## 211  211 2.2914214
    ## 212  212 2.5732051
    ## 213  213 1.9949490
    ## 214  214 1.8500000
    ## 215  215 2.1162278
    ## 216  216 1.7000000
    ## 217  217 1.5500000
    ## 218  218 1.6000000
    ## 219  219 0.8000000
    ## 220  220 0.5000000
    ## 221  221 2.5000000
    ## 222  222 0.0000000
    ## 223  223 1.8449490
    ## 226  226 1.6500000
    ## 227  227 2.5000000
    ## 228  228 1.8949490
    ## 230  230 1.9500000
    ## 231  231 0.1000000
    ## 232  232 1.4500000
    ## 234  234 2.5232051
    ## 235  235 2.2500000
    ## 236  236 2.0000000
    ## 237  237 2.1500000
    ## 238  238 1.4000000
    ## 239  239 1.9000000
    ## 240  240 0.5000000
    ## 241  241 2.0000000
    ## 243  243 2.4000000
    ## 244  244 1.4000000
    ## 245  245 3.1000000
    ## 246  246 2.3000000
    ## 249  249 3.6449490
    ## 250  250 1.9500000
    ## 252  252 1.8872983
    ## 253  253 1.8000000
    ## 254  254 1.4000000
    ## 255  255 2.2000000
    ## 257  257 2.2500000
    ## 258  258 2.9000000
    ## 259  259 2.7000000
    ## 260  260 1.8500000
    ## 261  261 2.0000000
    ## 262  262 2.2828427
    ## 263  263 2.1500000
    ## 264  264 1.2000000
    ## 265  265 1.6000000
    ## 267  267 3.0828427
    ## 269  269 2.7736068
    ## 270  270 2.1736068
    ## 271  271 1.1000000
    ## 272  272 2.2000000
    ## 273  273 2.0914214
    ## 274  274 2.5000000
    ## 275  275 0.8000000
    ## 276  276 1.8472136
    ## 278  278 1.4500000
    ## 279  279 1.4000000
    ## 280  280 1.7000000
    ## 281  281 1.5236068
    ## 282  282 1.8000000
    ## 283  283 1.8000000
    ## 284  284 1.3232051
    ## 285  285 2.4449490
    ## 287  287 2.4000000
    ## 290  290 0.0500000
    ## 293  293 1.2732051
    ## 294  294 2.2500000
    ## 295  295 0.9500000
    ## 297  297 1.7500000
    ## 298  298 2.3472136
    ## 301  301 2.3232051
    ## 302  302 3.0000000
    ## 303  303 1.3500000
    ## 304  304 0.9000000
    ## 306  306 2.4500000
    ## 307  307 0.3000000
    ## 308  308 1.6500000
    ## 309  309 2.4414214
    ## 310  310 1.8732051
    ## 311  311 1.7000000
    ## 313  313 0.1000000
    ## 314  314 2.4500000
    ## 315  315 1.3500000
    ## 316  316 3.0500000
    ## 318  318 2.4000000
    ## 319  319 3.2500000
    ## 320  320 2.6736068
    ## 321  321 1.6500000
    ## 322  322 1.2000000
    ## 323  323 1.4500000
    ## 324  324 1.3000000
    ## 325  325 2.6500000
    ## 326  326 0.9414214
    ## 328  328 2.4500000
    ## 329  329 2.5372983
    ## 330  330 0.1500000
    ## 332  332 1.5162278
    ## 333  333 1.4414214
    ## 335  335 2.3000000
    ## 336  336 1.4000000
    ## 337  337 2.2000000
    ## 338  338 1.3000000
    ## 339  339 1.3000000
    ## 340  340 0.7500000
    ## 341  341 2.8736068
    ## 342  342 0.2000000
    ## 343  343 2.7000000
    ## 344  344 1.6500000
    ## 345  345 0.2500000
    ## 346  346 1.8500000
    ## 347  347 2.3000000
    ## 348  348 1.8500000
    ## 349  349 2.0000000
    ## 350  350 0.5500000
    ## 351  351 1.6000000
    ## 352  352 2.5914214
    ## 353  353 2.6500000
    ## 354  354 1.8732051
    ## 356  356 0.7000000
    ## 357  357 1.7000000

    percentile_rank <- function(sample, value) {
      100 * sum(sample <= value) / length(sample)
    }

    percentile_rank(PC$PC, 2.8)

    ## [1] 92.97659

## Demographics

    # age 
    mean(dat$SO02_01)

    ## [1] 31.65217

    range(dat$SO02_01)

    ## [1] 18 72

    sd(dat$SO02_01)

    ## [1] 12.35083

    # Gender (1=female, 2=male, 3=diverse)
    dat |> count(SO01)

    ##   SO01   n
    ## 1    1 185
    ## 2    2 112
    ## 3    3   2

    # Time taken
    median(dat$TIME005)/60

    ## Completion time (experiment duration) 
    ## [1] 16

    min(dat$TIME005)/60

    ## [1] 15.51667

    max(dat$TIME005)/60

    ## [1] 65.35

## Plots

    # Histogram of PC Scores with Density
    ggplot(PC, aes(x = PC)) +
      geom_histogram(aes(y = after_stat(density)),
                     color = "black",
                     fill = "gray",
                     binwidth = 0.5,
                     boundary = 0,
                     alpha = 0.7) +
      geom_density(linewidth = 1.2) +
      labs(x = "PC-Ger Score",
           y = "Density") +
      theme(
        axis.title = element_text(face = "bold", size = 14)
      )

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/PC histo-1.png" style="display: block; margin: auto;" />

    # raincloud plot for PC
    my_col <- "#4C72B0"

    ggplot(PC, aes(x = PC, y = "")) +
      stat_slab(
        fill = my_col,
        alpha = 0.7,     
        adjust = .5,
        side = "top",
        color = NA
      ) +
      stat_dots(
        fill = my_col,
        alpha = 0.7,     
        side = "bottom",
        justification = 1,
        binwidth = 0.1,
        dotsize = .7,
        stackratio = 0.8
      ) +
      geom_boxplot(
        fill = my_col,
        alpha = 1,       
        width = 0.12,
        outlier.shape = NA,
        color = "black"
      ) +
      labs(
        x = "PC-Ger Score",
        y = "Density"
      ) +
      theme(
        axis.title = element_text(face = "bold", size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/PC raincloud plot-1.png" style="display: block; margin: auto;" />

    items <- c("SK01","SK02","SK03","SK04SK05","SK06",
               "SK07","SK08","SK09","SK10","SK12SK13")

    var_labels <- c(
      SK01 = "Hand\nlowering",
      SK02 = "Moving\nhands\ntogether",
      SK03 = "Mosquito\nhallucination",
      SK04 = "Sweet\ntaste",
      SK05 = "Sour\ntaste",
      SK06 = "Arm\nrigidity",
      SK07 = "Arm\nimmobilisation",
      SK08 = "Music\nhallucination",
      SK09 = "Negative\nvisual\nhallucination",
      SK10 = "Amnesia",
      SK12 = "Urge to\npress\nspace bar",
      SK13 = "Space bar\namnesia",
      SK04SK05 = "Taste\nhallucination",
      SK12SK13 = "Post-session\nsuggestion"
    )

    label_numbers <- c(
      SK01 = "1",
      SK02 = "2",
      SK03 = "3",
      SK04SK05 = "4",
      SK06 = "5",
      SK07 = "6",
      SK08 = "7",
      SK09 = "8",
      SK10 = "9",
      SK12SK13 = "10"
    )

    var_labels_num <- setNames(
      paste0(label_numbers[names(var_labels)], ". ", var_labels),
      names(var_labels)
    )

    vars_left  <- c("SK01","SK02","SK03","SK04SK05", "SK06")
    vars_right <- c("SK07","SK08","SK09","SK10","SK12SK13")

    singleItemPlot_data <- dat  |> 
          select(CASE, all_of(items)) %>%
          mutate(across(everything(), ~ as.numeric(.))) %>%
          pivot_longer(cols = -CASE,
                   names_to = "Item",
                   values_to = "value")

    make_plot <- function(data, vars) {
      data_filtered <- data %>%
        filter(Item %in% vars) %>%
        mutate(Item = factor(Item, levels = rev(vars)))

      ggplot() +
            # Layer 2: SK12SK13 with different parameters
        geom_dots(
          data = data_filtered %>% filter(Item == "SK12SK13"),
          aes(x = value, y = Item, fill = Item, color = Item),
          smooth = smooth_discrete(width = 3.5, size = .2), 
          binwidth = 0.1,
          dotsize = .65
        ) +
        
        # Layer 1: all items except SK12SK13
        geom_dots(
          data = data_filtered %>% filter(Item != "SK12SK13"),
          aes(x = value, y = Item, fill = Item, color = Item),
          smooth = smooth_discrete(width = .8, size = .1),
          binwidth = 0.1,
          dotsize = .65
        ) +
        

        scale_y_discrete(labels = var_labels_num, expand = expansion(mult = c(0, 0))) +
        scale_x_continuous(breaks = 0:5) +

        scale_fill_manual(values = c(
          SK01 = "#2ECC71FF",
          SK02 = "#F1C40FFF",
          SK03 = "#E74C3CFF",
          SK04SK05 = "#9B59B6FF",
          SK06 = "#1ABC9CFF",
          SK07 = "#db00b6",
          SK08 = "#D35400FF",
          SK09 = "#65f7f4",
          SK10 = "#1f78b4",
          SK12SK13 = "#666666"
        )) +
        scale_color_manual(values = c(
          SK01 = "#2ECC71FF",
          SK02 = "#F1C40FFF",
          SK03 = "#E74C3CFF",
          SK04SK05 = "#9B59B6FF",
          SK06 = "#1ABC9CFF",
          SK07 = "#db00b6",
          SK08 = "#D35400FF",
          SK09 = "#65f7f4",
          SK10 = "#1f78b4",
          SK12SK13 = "#666666"
        )) +

        labs(
          x = "PC-Ger Score",
          y = NULL
        ) +
        theme_minimal() +
        theme(
          axis.title = element_text(face = "bold", size = 24),
          axis.text = element_text(face = "bold", size = 24),
          axis.text.y = element_text(
            face = "bold",
            size = 24,
            hjust = 0,
            margin = margin(r = 10)
          ),
          legend.position = "none"
        )
    }

    p1 <- make_plot(singleItemPlot_data, vars_left)
    p2 <- make_plot(singleItemPlot_data, vars_right)

    p1 + p2

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/single item dot bars-1.png" style="display: block; margin: auto;" />

# Comparison with Lush et al. (2021), First test data

    Lush_dat <- read.csv("PeteOSF/PCS_norms_first_test_data.csv", sep = ",")

    Lush_dat <- Lush_dat %>%  filter(exclude == 0)

    rename_map <- c(
      ParticipantNumber                   = "CASE",
      HandLoweringSubjectiveRating        = "SK01",
      HandsTogetherSubjectiveRating       = "SK02",
      MosquitoSubjectiveRating            = "SK03",
      SweetSubRating                      = "SK04",
      SourSubRating                       = "SK05",
      ArmRigiditySubjectiveRating         = "SK06",
      ArmImmobilisationSubjectiveRating   = "SK07",
      MusicSubjectiveRating               = "SK08",
      VisualHallucinationSubjectiveRating = "SK09",
      AmnesiaSubjectiveRating             = "SK10",
      PostHypnoticSub1                    = "SK12",
      PostHypnoticSub2                    = "SK13",
      TasteSubjectiveRating               = "SK04SK05",
      PostHypnoticSubjectiveRating        = "SK12SK13"
    )

    label_map <- c(
      SK01   = "Hand lowering",
      SK02   = "Moving hands together",
      SK03   = "Mosquito hallucination",
      SK04   = "Sweet taste",
      SK05   = "Sour taste",
      SK06   = "Arm rigidity",
      SK07   = "Arm immobilization",
      SK08   = "Music hallucination",
      SK09   = "Negative visual hallucination",
      SK10   = "Amnesia",
      SK12   = "Urge to press space bar",
      SK13   = "Space bar amnesia",
      SK0405 = "Taste Halluctionation",
      SK1213 = "Post-session suggestion"
    )


    matched <- intersect(names(rename_map), names(Lush_dat))
    names(Lush_dat)[match(matched, names(Lush_dat))] <- rename_map[matched]

    # Items to include
    items <- c("SK01","SK02","SK03","SK04SK05","SK06","SK07","SK08","SK09","SK10","SK12SK13")
    itemsPC <- c("SK01","SK02","SK03","SK04SK05","SK06","SK07","SK08","SK09","SK10","SK12SK13", "PC")

    Lush_dat <- Lush_dat %>% 
                select(CASE, all_of(items)) %>% 
                mutate(PC = rowMeans(across(all_of(items), ~ as.numeric(.)))) %>% 
                mutate(group = "Original") %>% 
                mutate(across(all_of(items), ~ as.numeric(.)))

    Ger_dat <- dat %>% 
               select(CASE, all_of(items)) %>% 
               mutate(PC = rowMeans(across(all_of(items), ~ as.numeric(.)))) %>% 
               mutate(group = "German") %>% 
               mutate(across(all_of(items), ~ as.numeric(.)))


    combined_dat <- bind_rows(Lush_dat, Ger_dat) %>% 
      select(CASE, group, SK01, SK02, SK03, SK04SK05, SK06, SK07, SK08, SK09, SK10, SK12SK13, PC)

    for (nm in intersect(names(label_map), names(combined_dat))) {
      Hmisc::label(combined_dat[[nm]]) <- label_map[[nm]]
    }


    combined_dat_long <- combined_dat %>%
      mutate(across(all_of(itemsPC), ~ as.numeric(haven::zap_labels(.x)))) %>%
      pivot_longer(
        cols = all_of(itemsPC),
        names_to = "Item",
        values_to = "value"
      )

## Item-Level Descriptives

    item_desc <- combined_dat %>%
      group_by(group) %>%
      group_modify(~{
        
        dat_items <- .x[, items, drop = FALSE]
        
        # Cronbach's alpha
        alpha_res <- psych::alpha(dat_items, n.iter = 5000)
        alpha_drop <- alpha_res$alpha.drop
        
        # Bootstrap CI for alpha if item dropped
        drop_boot <- alpha_drop_boot_ci(dat_items, B = 5000, seed = 123)
        
        # Item descriptives
        means <- colMeans(dat_items, na.rm = TRUE)
        sds   <- apply(dat_items, 2, sd, na.rm = TRUE)
        skew  <- apply(dat_items, 2, psych::skew, na.rm = TRUE)
        kurt  <- apply(dat_items, 2, psych::kurtosi, na.rm = TRUE)
        
        # Person composite score within group
        PC <- .x %>%
          mutate(
            PC = rowMeans(across(all_of(items)), na.rm = TRUE)
          ) %>%
          pull(PC)
        
        # Item-total correlation within group
        item_total <- sapply(items, function(x) {
          cor(.x[[x]], PC, use = "pairwise.complete.obs", method = "pearson")
        })
        
        item_total_corrected <- alpha_res$item.stats$r.drop

        data.frame(
          item_code = items,
          Item      = sapply(items, function(x) Hmisc::label(.x[[x]])),
          Mean      = round(means, 2),
          SD        = round(sds, 2),
          Skew      = round(skew, 2),
          Kurt      = round(kurt, 2),
          Alpha     = round(alpha_drop$raw_alpha, 2),
          CI_Low    = round(drop_boot$ci_lower, 2),
          CI_Up     = round(drop_boot$ci_upper, 2),
          ItemTotal = round(item_total, 2), 
          item_total_corrected = round(item_total_corrected, 2)
        )
      }) %>%
      ungroup()




    item_desc %>% select(group, Item, ItemTotal, item_total_corrected)

    ## # A tibble: 20 × 4
    ##    group    Item                            ItemTotal item_total_corrected
    ##    <chr>    <chr>                               <dbl>                <dbl>
    ##  1 German   "Hand lowering"                      0.65                 0.53
    ##  2 German   "Moving hands together"              0.57                 0.42
    ##  3 German   "Mosquito hallucination"             0.57                 0.41
    ##  4 German   ""                                   0.54                 0.37
    ##  5 German   "Arm rigidity"                       0.73                 0.6 
    ##  6 German   "Arm immobilization"                 0.69                 0.55
    ##  7 German   "Music hallucination"                0.38                 0.23
    ##  8 German   "Negative visual hallucination"      0.32                 0.19
    ##  9 German   "Amnesia"                            0.5                  0.35
    ## 10 German   ""                                   0.32                 0.14
    ## 11 Original "Hand lowering"                      0.65                 0.52
    ## 12 Original "Moving hands together"              0.59                 0.45
    ## 13 Original "Mosquito hallucination"             0.61                 0.47
    ## 14 Original ""                                   0.6                  0.47
    ## 15 Original "Arm rigidity"                       0.74                 0.63
    ## 16 Original "Arm immobilization"                 0.74                 0.63
    ## 17 Original "Music hallucination"                0.35                 0.27
    ## 18 Original "Negative visual hallucination"      0.3                  0.16
    ## 19 Original "Amnesia"                            0.54                 0.39
    ## 20 Original ""                                   0.48                 0.31

    ks_results <- combined_dat_long |>
      group_by(Item) |>
      summarise(
        ks_stat   = ks.test(
          value[group == "Original"],
          value[group == "German"]
        )$statistic,
        p_value   = ks.test(
          value[group == "Original"],
          value[group == "German"]
        )$p.value,
        .groups = "drop"
      ) |>
      mutate(significant = p_value < .05)

    ks_results

    ## # A tibble: 11 × 4
    ##    Item     ks_stat  p_value significant
    ##    <chr>      <dbl>    <dbl> <lgl>      
    ##  1 PC        0.0925 0.0845   FALSE      
    ##  2 SK01      0.0575 0.573    FALSE      
    ##  3 SK02      0.0866 0.125    FALSE      
    ##  4 SK03      0.109  0.0244   TRUE       
    ##  5 SK04SK05  0.140  0.00140  TRUE       
    ##  6 SK06      0.0496 0.752    FALSE      
    ##  7 SK07      0.0722 0.291    FALSE      
    ##  8 SK08      0.141  0.00135  TRUE       
    ##  9 SK09      0.0583 0.556    FALSE      
    ## 10 SK10      0.153  0.000363 TRUE       
    ## 11 SK12SK13  0.106  0.0320   TRUE

    label_map <- c(
      SK01      = "Hand lowering",
      SK02      = "Moving hands together",
      SK03      = "Mosquito hallucination",
      SK06      = "Arm rigidity",
      SK07      = "Arm immobilization",
      SK08      = "Music hallucination",
      SK09      = "Negative visual hallucination",
      SK10      = "Amnesia",
      SK04SK05  = "Taste Hallucination",
      SK12SK13  = "Post-session suggestion"
    )

    create_hist_plot <- function(var_name) {
      
      bins <- if (var_name %in% c("SK04SK05", "SK12SK13")) 11 else 6
      
      ggplot(
        combined_dat %>%
          filter(group %in% c("German", "Original")),
        aes(
          x = .data[[var_name]],
          color = group,
          fill = group
        )
      ) +
        geom_histogram(
          aes(y = after_stat(density)),
          alpha = 0.2,
          position = "identity",
          bins = bins
        ) +
        labs(
          title = label_map[[var_name]],
          x = NULL,
          y = "Density",
          color = "Dataset",
          fill = "Dataset"
        ) +
        scale_fill_manual(values = c(
          German = "orange",
          Original = "royalblue"
        )) +
        scale_color_manual(values = c(
          German = "orange",
          Original = "royalblue"
        )) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 24),
          axis.text = element_text(face = "bold", size = 20),
          axis.text.y = element_text(
            face = "bold",
            size = 20,
            hjust = 0,
            margin = margin(r = 10)
          ),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
    }

    plot_order <- c(
      "SK01", "SK07",
      "SK02", "SK08",
      "SK03", "SK09",
      "SK04SK05", "SK10",
      "SK06", "SK12SK13"
    )

    hist_plots <- map(plot_order, create_hist_plot)
    names(hist_plots) <- plot_order

    combined_hist_plot <- wrap_plots(hist_plots, ncol = 2, nrow = 5, guides = "collect") &
      theme(legend.position = "bottom")

    combined_hist_plot

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/item-level density histograms for sample comparison-1.png" style="display: block; margin: auto;" />

## Scale-Level (PC score) Descriptives

    inner_join(combined_dat %>%
      group_by(group) %>%
      summarise(
        mean = mean(PC, na.rm = TRUE),
        sd   = sd(PC, na.rm = TRUE),
        min  = min(PC, na.rm = TRUE),
        max  = max(PC, na.rm = TRUE),
        skew = psych::skew(PC, na.rm = TRUE),
        kurt = psych::kurtosi(PC, na.rm = TRUE)
      ),
    item_desc %>% 
      group_by(group) %>%
      summarise(meanItemTotal = mean(ItemTotal))
    )

    ## # A tibble: 2 × 8
    ##   group     mean    sd   min   max    skew    kurt meanItemTotal
    ##   <chr>    <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>         <dbl>
    ## 1 German    1.85 0.730     0  4.19 -0.195   0.209          0.527
    ## 2 Original  1.73 0.761     0  4.2   0.0106 -0.0901         0.56

    ks.test(
      combined_dat_long |> filter(group == "Original" & Item == "PC") |> pull(value),
      combined_dat_long |> filter(group == "German"   & Item == "PC") |> pull(value)
    )

    ## 
    ##  Asymptotic two-sample Kolmogorov-Smirnov test
    ## 
    ## data:  pull(filter(combined_dat_long, group == "Original" & Item == "PC"), value) and pull(filter(combined_dat_long, group == "German" & Item == "PC"), value)
    ## D = 0.092519, p-value = 0.0845
    ## alternative hypothesis: two-sided

    histo <- ggplot(combined_dat, aes(x = PC, color=group, fill = group)) +
      geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity", bins = 30) +
      geom_density(linewidth = 1.2,
                   alpha = 0.2) +
      labs(x = "PC Score",
           y = "Density",
           color = "Dataset",
           fill = "Dataset") +
      scale_fill_manual(values = c(
          German = "orange",
          Original = "royalblue"
        )) +
        scale_color_manual(values = c(
          German = "orange",
          Original = "royalblue"
        )) +
      theme_minimal() +
        theme(
          axis.title = element_text(face = "bold", size = 24),
          axis.text = element_text(face = "bold", size = 24),
          axis.text.y = element_text(
            face = "bold",
            size = 24,
            hjust = 0,
            margin = margin(r = 10)
          ),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=24)

        )

    histo

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/Histo PC both datasets-1.png" style="display: block; margin: auto;" />

    # Split the two groups
    german <- combined_dat %>%
      filter(group == "German") %>%
      pull(PC)

    original <- combined_dat %>%
      filter(group == "Original") %>%
      pull(PC)

    # Create common probability sequence
    n <- min(length(german), length(original))
    probs <- seq(0, 1, length.out = n)

    # Compute quantiles
    qq_data <- data.frame(
      German = quantile(german, probs = probs, na.rm = TRUE),
      Original = quantile(original, probs = probs, na.rm = TRUE)
    )

    # Plot
    qq1 <- ggplot(qq_data, aes(x = Original, y = German)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        x = "Original PC quantiles",
        y = "German PC quantiles"
      ) +
      coord_flip() +
      theme_minimal() +
        theme(
          axis.title = element_text(face = "bold", size = 24),
          axis.text = element_text(face = "bold", size = 24),
          axis.text.y = element_text(
            face = "bold",
            size = 24,
            hjust = 0,
            margin = margin(r = 10)
          ),
          legend.position = "none"
        )


    # Q-Q plot against normal distribution
    qq2 <- ggplot(data.frame(PC = german), aes(sample = PC)) +
      stat_qq() +
      stat_qq_line(linetype = "dashed", color = "red") +
      labs(
        x = "Normal distribution quantiles",
        y = "German PC quantiles"
      ) +
      coord_flip() +
      theme_minimal() +
        theme(
          axis.title = element_text(face = "bold", size = 24),
          axis.text = element_text(face = "bold", size = 24),
          axis.text.y = element_text(
            face = "bold",
            size = 24,
            hjust = 0,
            margin = margin(r = 10)
          ),
          legend.position = "none"
        )

    qq1

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/Create Q-Q Plots-1.png" style="display: block; margin: auto;" />

    qq2

<img src="Ger_PC_scale_analysis2_files/figure-markdown_strict/Create Q-Q Plots-2.png" style="display: block; margin: auto;" />

## Compare Cronbach’s Alpha

    alpha_lush <- psych::alpha(Lush_dat[items], n.iter=5000)
    alpha_ger  <- psych::alpha(Ger_dat[items], n.iter=5000)

      

    data.frame(
      Dataset = c("Original", "German"),
      Alpha   = c(alpha_lush$total$raw_alpha,
                  alpha_ger$total$raw_alpha)
    )

    ##    Dataset     Alpha
    ## 1 Original 0.7686533
    ## 2   German 0.7254278

    alpha_lush$boot.ci

    ##      2.5%       50%     97.5% 
    ## 0.7336139 0.7688881 0.7969347

    alpha_ger$boot.ci

    ##      2.5%       50%     97.5% 
    ## 0.6681918 0.7254138 0.7687025
