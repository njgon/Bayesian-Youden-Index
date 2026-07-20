library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(boot)  # NEW: required for BCa confidence intervals in the Robustness Check tab

# ── Helper functions ─────────────────────────────────────────────
calculate_PPV <- function(Sen, Esp, Prev) {
  num <- Sen * Prev
  den <- Sen * Prev + (1 - Esp) * (1 - Prev)
  if (den == 0) return(0)
  num / den
}

calculate_NPV <- function(Sen, Esp, Prev) {
  num <- Esp * (1 - Prev)
  den <- (1 - Sen) * Prev + Esp * (1 - Prev)
  if (den == 0) return(0)
  num / den
}

find_intersection_points <- function(Sen, Esp, tolerance = 0.001) {
  Cy    <- Sen + Esp - 1
  prevs <- seq(0.01, 0.99, by = 0.001)
  by_vals <- sapply(prevs, function(p)
    calculate_PPV(Sen, Esp, p) + calculate_NPV(Sen, Esp, p) - 1)
  hits <- which(abs(by_vals - Cy) < tolerance)
  if (length(hits) == 0) return(numeric(0))
  pv  <- prevs[hits]
  grps <- list(); cur <- c(pv[1])
  for (i in 2:length(pv)) {
    if (pv[i] - tail(cur, 1) < 0.02) cur <- c(cur, pv[i])
    else { grps[[length(grps) + 1]] <- mean(cur); cur <- c(pv[i]) }
  }
  grps[[length(grps) + 1]] <- mean(cur)
  unlist(grps)
}

reconstruct_cm <- function(Sen, Esp, Prev, N = 93) {
  np <- round(N * Prev);  nn <- N - np
  TP <- round(Sen * np);  TN <- round(Esp * nn)
  list(TP = TP, FP = nn - TN, FN = np - TP, TN = TN,
       n_positive = np, n_negative = nn, N = N)
}

# ── Bootstrap functions ──────────────────────────────────────────
find_max_by_r <- function(Se, Sp) {
  prevs <- seq(0.001, 0.999, by = 0.001)
  bys   <- sapply(prevs, function(p) calculate_PPV(Se, Sp, p) + calculate_NPV(Se, Sp, p) - 1)
  list(max_by = max(bys), p_max = prevs[which.max(bys)])
}

find_intersections_r <- function(Se, Sp, tol = 0.0015) {
  Cy    <- Se + Sp - 1
  prevs <- seq(0.001, 0.999, by = 0.001)
  bys   <- sapply(prevs, function(p) calculate_PPV(Se, Sp, p) + calculate_NPV(Se, Sp, p) - 1)
  hits  <- which(abs(bys - Cy) < tol)
  if (length(hits) == 0) return(c(NA_real_, NA_real_))
  pv <- prevs[hits]
  grps <- list(); cur <- c(pv[1])
  for (i in seq_along(pv)[-1]) {
    if (pv[i] - tail(cur, 1) < 0.02) cur <- c(cur, pv[i])
    else { grps[[length(grps)+1]] <- mean(cur); cur <- c(pv[i]) }
  }
  grps[[length(grps)+1]] <- mean(cur)
  pts <- unlist(grps)
  c(pts[1], if (length(pts) >= 2) pts[2] else NA_real_)
}

run_bootstrap <- function(VP, FP, FN, VN, B = 2000) {
  n_pos  <- VP + FN
  n_neg  <- VN + FP
  Se_obs <- VP / n_pos
  Sp_obs <- VN / n_neg
  
  res <- replicate(B, {
    Se_b <- pmax(pmin(rbinom(1, n_pos, Se_obs) / n_pos, 0.9999), 0.0001)
    Sp_b <- pmax(pmin(rbinom(1, n_neg, Sp_obs) / n_neg, 0.9999), 0.0001)
    Cy_b <- Se_b + Sp_b - 1
    mb   <- find_max_by_r(Se_b, Sp_b)
    ints <- find_intersections_r(Se_b, Sp_b)
    c(Cy_b, mb$max_by, mb$p_max, ints[1], ints[2])
  })
  t(res)
}

# ══════════════════════════════════════════════════════════════════
# NEW: helper functions for the "Robustness Check (S2)" tab
#      (empirical bootstrap distributions, skewness, BCa intervals,
#       and a non-parametric case-resampling bootstrap)
# ══════════════════════════════════════════════════════════════════

# NEW: sample-moment skewness (no extra package dependency)
skewness_fn <- function(x) {
  x <- x[is.finite(x)]
  m <- mean(x); s <- sd(x); n <- length(x)
  if (s == 0 || n < 3) return(NA_real_)
  (mean((x - m)^3)) / (s^3)
}

# NEW: parametric bootstrap extended with By* evaluated at a fixed
#      prevalence P0 (in addition to Cy*, max By*, and P at max By*)
run_bootstrap_ext <- function(VP, FP, FN, VN, P0, B = 2000) {
  n_pos  <- VP + FN
  n_neg  <- VN + FP
  Se_obs <- VP / n_pos
  Sp_obs <- VN / n_neg
  
  res <- replicate(B, {
    Se_b <- pmax(pmin(rbinom(1, n_pos, Se_obs) / n_pos, 0.9999), 0.0001)
    Sp_b <- pmax(pmin(rbinom(1, n_neg, Sp_obs) / n_neg, 0.9999), 0.0001)
    Cy_b     <- Se_b + Sp_b - 1
    By_P0_b  <- calculate_PPV(Se_b, Sp_b, P0) + calculate_NPV(Se_b, Sp_b, P0) - 1
    mb_b     <- find_max_by_r(Se_b, Sp_b)
    c(Cy_b, By_P0_b, mb_b$max_by, mb_b$p_max)
  })
  out <- t(res)
  colnames(out) <- c("Cy", "By_P0", "By_max", "P_max")
  out
}

# NEW: statistic function for non-parametric case-resampling bootstrap.
#      df has one row per reconstructed subject, with a "cell" column
#      in {TP, FP, FN, TN}. Resampling rows with replacement lets BOTH
#      margins (n_pos, n_neg) vary freely, unlike the independent-binomial
#      (fixed-margin) parametric model above.
make_case_resample_stat <- function(P0) {
  function(df, indices) {
    d   <- df[indices, , drop = FALSE]
    tab <- table(factor(d$cell, levels = c("TP", "FP", "FN", "TN")))
    TPb <- as.numeric(tab["TP"]); FPb <- as.numeric(tab["FP"])
    FNb <- as.numeric(tab["FN"]); TNb <- as.numeric(tab["TN"])
    n_pos_b <- TPb + FNb
    n_neg_b <- TNb + FPb
    if (n_pos_b == 0 || n_neg_b == 0) return(c(NA, NA, NA, NA))
    Se_b <- pmax(pmin(TPb / n_pos_b, 0.9999), 0.0001)
    Sp_b <- pmax(pmin(TNb / n_neg_b, 0.9999), 0.0001)
    Cy_b    <- Se_b + Sp_b - 1
    By_P0_b <- calculate_PPV(Se_b, Sp_b, P0) + calculate_NPV(Se_b, Sp_b, P0) - 1
    mb_b    <- find_max_by_r(Se_b, Sp_b)
    c(Cy_b, By_P0_b, mb_b$max_by, mb_b$p_max)
  }
}

# ── UI ───────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
    body { font-family: 'Inter','Segoe UI',sans-serif; font-size:14px;
           color:#2c3e50; background-color:#f8f9fa; }
    .well { background-color:#ffffff; border:1px solid #e1e8ed;
            border-radius:12px; box-shadow:0 2px 8px rgba(0,0,0,0.05); padding:20px; }
    h2,h3,h4,h5 { font-weight:600; color:#1a1a2e; }
    .shiny-input-container { margin-bottom:20px; }
    .control-label { font-weight:600; font-size:14px; color:#2c3e50; margin-bottom:8px; }
    .btn { border-radius:8px; font-weight:500; font-size:13px;
           padding:8px 16px; transition:all 0.2s; }
    .btn-success { background-color:#27ae60; border-color:#27ae60; }
    .btn-success:hover { background-color:#229954; transform:translateY(-1px); }
    .btn-primary { background-color:#2980b9; border-color:#2980b9; }
    .nav-tabs>li>a { font-weight:600; font-size:14px; color:#7f8c8d;
                     border:none; padding:10px 18px; }
    .nav-tabs>li.active>a { color:#3498db; background-color:transparent;
                             border-bottom:3px solid #3498db; }
    .tab-content { background-color:#ffffff; padding:25px; border-radius:12px;
                   margin-top:20px; box-shadow:0 2px 12px rgba(0,0,0,0.08); }
    .cm-wrap { margin:20px auto; max-width:100%; overflow-x:auto; }
    .cm-wrap table { width:100%; border-collapse:collapse; font-size:14px;
                     border-radius:10px; overflow:hidden;
                     box-shadow:0 2px 8px rgba(0,0,0,0.1); }
    .cm-wrap th { background:linear-gradient(135deg,#1a1a2e 0%,#16213e 100%);
                  color:white; font-weight:600; padding:12px; text-align:center; }
    .cm-wrap td { padding:10px 12px; text-align:center; border:1px solid #e1e8ed; }
    .cm-wrap .tp { background-color:#d5f4e6; color:#196f3d; font-weight:700; }
    .cm-wrap .fp { background-color:#fadbd8; color:#c0392b; font-weight:700; }
    .cm-wrap .fn { background-color:#fadbd8; color:#c0392b; font-weight:700; }
    .cm-wrap .tn { background-color:#d5f4e6; color:#196f3d; font-weight:700; }
    .cm-wrap .rh { background:linear-gradient(135deg,#2c3e50 0%,#34495e 100%);
                   color:white; font-weight:600; }
    .cm-wrap .tc { background-color:#ecf0f1; font-weight:600; }
    .metric-card { background:#ffffff; border-radius:10px; padding:15px;
                   margin:10px 0; border-left:4px solid;
                   box-shadow:0 1px 3px rgba(0,0,0,0.1); }
    .info-box { background:linear-gradient(135deg,#f8f9fa 0%,#ffffff 100%);
                border-radius:10px; padding:20px; margin:15px 0;
                border-left:4px solid #3498db; }
    .boot-table { width:100%; border-collapse:collapse; font-size:14px; margin-top:15px; }
    .boot-table th { background:#1a1a2e; color:white; padding:10px 14px;
                     text-align:left; font-weight:600; }
    .boot-table td { padding:9px 14px; border-bottom:1px solid #ecf0f1; }
    .boot-table tr:nth-child(even) td { background:#f8f9fa; }
    .boot-table .obs { font-weight:700; color:#2c3e50; }
    .boot-table .ic  { font-weight:600; color:#2980b9; }
    hr { border:0; height:1px;
         background:linear-gradient(90deg,transparent,#e1e8ed,transparent); }
  "))),
  
  div(style = "max-width:1400px; margin:0 auto; padding:20px;",
      
      div(style = "text-align:center; margin-bottom:30px;",
          h2(style = "font-size:28px; font-weight:700; color:#1a1a2e;",
             "Bayesian Youden Index"),
          p(style = "font-size:15px; color:#7f8c8d;",
            "How sensitivity, specificity and prevalence shape classifier performance")
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class = "well",
              h4(style = "margin-top:0; margin-bottom:20px; font-size:16px;",
                 "Classifier Parameters"),
              tags$label("Sensitivity (Se)", style="font-weight:600; font-size:13px;"),
              sliderInput("sensitivity", label=NULL, min=0.05, max=1.00, value=0.91, step=0.01),
              tags$label("Specificity (Sp)", style="font-weight:600; font-size:13px;"),
              sliderInput("specificity", label=NULL, min=0.05, max=1.00, value=0.98, step=0.01),
              hr(),
              h4(style = "margin-bottom:15px; font-size:16px;", "Calculated Metrics"),
              uiOutput("metrics_info")
          )
        ),
        
        mainPanel(
          width = 9,
          tabsetPanel(
            type = "tabs",
            
            # ── Tab 1: Plot ──
            tabPanel(
              HTML("C<sub>Y</sub> vs B<sub>Y</sub>"), br(),
              div(style="text-align:right; margin-bottom:15px;",
                  downloadButton("download_plot","Download Plot (PNG)",
                                 class="btn-success", icon=icon("download"))),
              plotOutput("youden_plot", height="600px"), br(),
              uiOutput("interpretation_youden")
            ),
            
            # ── Tab 2: Bootstrap ─────────────────────────────────────
            tabPanel(
              HTML("Bootstrap CI"), br(),
              div(class="info-box",
                  h4(style="margin-top:0; color:#2980b9;",
                     "Parametric Bootstrap — Uncertainty Quantification"),
                  p(style="color:#7f8c8d; margin-bottom:15px;",
                    HTML("Confidence intervals for C<sub>Y</sub>, B<sub>Y</sub> max,
                       P at max B<sub>Y</sub>, and intersection points P<sub>1</sub>
                       and P<sub>2</sub>. Each bootstrap replicate draws
                       VP* ~ Binomial(n<sub>+</sub>, Se) and
                       TN* ~ Binomial(n<sub>-</sub>, Sp) from the observed
                       confusion matrix.")),
                  fluidRow(
                    column(4,
                           numericInput("boot_vp", "True Positives (VP)", value=797, min=1),
                           numericInput("boot_fp", "False Positives (FP)", value=271, min=0)
                    ),
                    column(4,
                           numericInput("boot_fn", "False Negatives (FN)", value=82,  min=0),
                           numericInput("boot_vn", "True Negatives (VN)", value=204, min=1)
                    ),
                    column(4,
                           numericInput("boot_B", "Bootstrap replicates (B)",
                                        value=2000, min=500, max=10000, step=500),
                           br(),
                           actionButton("run_boot", "Run Bootstrap",
                                        class="btn-primary", icon=icon("play"),
                                        style="width:100%; margin-top:8px;")
                    )
                  )
              ),
              br(),
              uiOutput("boot_results")
            ),
            
            # ════════════════════════════════════════════════════════
            # NEW TAB: Robustness Check (S2)
            #   - empirical bootstrap distributions of Cy*, By*(P0),
            #     By*(max), with skewness
            #   - percentile vs BCa intervals
            #   - non-parametric case-resampling bootstrap alongside
            #     the parametric (independent-binomial) one
            # ════════════════════════════════════════════════════════
            tabPanel(
              HTML("Robustness Check (S2)"), br(),
              div(class="info-box",
                  h4(style="margin-top:0; color:#2980b9;",
                     "Supplementary Material S2 — Bootstrap Robustness"),
                  p(style="color:#7f8c8d; margin-bottom:15px;",
                    HTML("Uses the same confusion matrix counts entered in the
                       <b>Bootstrap CI</b> tab. Compares the percentile-method
                       intervals already reported there against BCa intervals,
                       and compares the parametric (independent-binomial,
                       fixed-margin) bootstrap against a non-parametric
                       case-resampling bootstrap that lets both margins vary
                       (equivalent to a multinomial resampling of the four
                       confusion-matrix cells).")),
                  fluidRow(
                    column(4,
                           numericInput("robust_p0", "Observed prevalence P0 (for By* at P0)",
                                        value = 0.484, min = 0.01, max = 0.99, step = 0.001)
                    ),
                    column(4,
                           p(style="font-size:12px; color:#95a5a6; margin-top:6px;",
                             "VP, FP, FN, VN and B are taken from the Bootstrap CI tab.")
                    ),
                    column(4,
                           actionButton("run_robust", "Run Robustness Check",
                                        class="btn-primary", icon=icon("play"),
                                        style="width:100%; margin-top:6px;")
                    )
                  )
              ),
              br(),
              plotOutput("robust_hist", height = "320px"),
              br(),
              uiOutput("robust_summary")
            ),
            
            # ── Tab 3: Confusion Matrix ──
            tabPanel(
              "Confusion Matrix", br(),
              h4(style="text-align:center; margin-bottom:20px;",
                 "Reconstructed Confusion Matrix"),
              p(style="text-align:center; color:#7f8c8d; margin-bottom:30px;",
                "Based on Se, Sp and prevalence at HOMA-IR (P = 0.484), N = 93"),
              uiOutput("cm_output"), br(),
              uiOutput("cm_metrics")
            ),
            
            # ── Tab 4: Value Table ──
            tabPanel(
              "Value Table", br(),
              h4(style="margin-bottom:20px;", "Metric values at different prevalences"),
              DTOutput("value_table")
            ),
            
            # ── Tab 5: Information ──
            tabPanel(
              "Information", br(),
              div(class="info-box",
                  h3(style="margin-top:0; color:#3498db;", "About the Metrics"),
                  h4(HTML("<b>C<sub>Y</sub></b> — Classical Youden Index")),
                  p(HTML("Prevalence-independent: C<sub>Y</sub> = Se + Sp - 1.")),
                  h4(HTML("<b>B<sub>Y</sub></b> — Bayesian Youden Index")),
                  p(HTML("Prevalence-dependent: B<sub>Y</sub> = PPV + NPV - 1.")),
                  h4(HTML("Intersection Points P<sub>1</sub> and P<sub>2</sub>")),
                  p(HTML("Prevalences where C<sub>Y</sub> = B<sub>Y</sub>.")),
                  hr(),
                  h4("Bayesian Se and Sp"),
                  p(HTML("<b>Se<sub>B</sub> = PPV</b>: posterior probability of IR given positive result.")),
                  p(HTML("<b>Sp<sub>B</sub> = NPV</b>: posterior probability of non-IR given negative result."))
              )
            )
          )
        )
      ),
      
      hr(),
      div(style="text-align:center; margin-top:30px;",
          p(style="color:#95a5a6; font-size:12px;",
            "Developed with R Shiny | Bayesian Youden Index"),
          p(style="color:#e67e22; font-size:13px; font-weight:500;",
            "HOMA-IR Prevalence: P = 0.484 | N = 93"))
  )
)

# ── Server ───────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rd <- reactive({
    req(input$sensitivity, input$specificity)
    Sen <- input$sensitivity; Esp <- input$specificity
    prevs <- seq(0.01, 0.99, by=0.01)
    data.frame(Prev=prevs) %>%
      mutate(PPV = mapply(calculate_PPV, Sen, Esp, Prev),
             NPV = mapply(calculate_NPV, Sen, Esp, Prev),
             By  = PPV + NPV - 1,
             Cy  = Sen + Esp - 1)
  })
  
  ints <- reactive({
    req(input$sensitivity, input$specificity)
    find_intersection_points(input$sensitivity, input$specificity)
  })
  
  # ── Main plot ────────────────────────────────────────────────
  build_plot <- reactive({
    req(rd())
    data <- rd(); Sen <- input$sensitivity; Esp <- input$specificity
    Cy   <- Sen + Esp - 1
    homa_prev  <- 0.484
    max_by     <- max(data$By, na.rm=TRUE)
    idx_max    <- which.max(data$By)
    prev_max   <- data$Prev[idx_max]
    homa_idx   <- which.min(abs(data$Prev - homa_prev))
    by_at_homa <- data$By[homa_idx]
    ip         <- ints()
    
    p <- ggplot(data, aes(x=Prev)) +
      geom_hline(aes(yintercept=Cy, color="Cy"), linewidth=1.2, alpha=0.9) +
      geom_line(aes(y=By, color="By"), linewidth=1.2, alpha=0.9) +
      geom_vline(xintercept=homa_prev, linetype="longdash",
                 color="#7f8c8d", linewidth=0.8, alpha=0.7) +
      geom_vline(xintercept=prev_max, linetype="dashed",
                 color="#e74c3c", linewidth=0.8, alpha=0.6) +
      geom_point(data=data[idx_max,], aes(y=By),
                 color="#e74c3c", size=3.5, shape=17) +
      geom_point(data=data.frame(x=homa_prev,y=Cy), aes(x=x,y=y),
                 color="#2980b9", size=3.5, shape=16, inherit.aes=FALSE) +
      geom_point(data=data.frame(x=homa_prev,y=by_at_homa), aes(x=x,y=y),
                 color="#e74c3c", size=3.5, shape=16, inherit.aes=FALSE) +
      scale_color_manual(
        values=c("Cy"="#2980b9","By"="#e74c3c"),
        labels=c(expression(C[Y]~"(Classical Youden)"),
                 expression(B[Y]~"(Bayesian Youden)")),
        name=NULL) +
      scale_x_continuous(name="Prevalence (P)", breaks=seq(0,1,.1),
                         limits=c(0,1), expand=c(0,0)) +
      scale_y_continuous(name="Metric Value", limits=c(0,1), breaks=seq(0,1,.1)) +
      labs(title=expression(C[Y]~"vs"~B[Y]~"- Bayesian Youden Index"),
           subtitle=paste0("Se = ",sprintf("%.3f",Sen),"   |   Sp = ",
                           sprintf("%.3f",Esp),"   |   CY = ",sprintf("%.3f",Cy))) +
      theme_minimal(base_size=12) +
      theme(plot.title=element_text(face="bold",hjust=0.5,size=18,color="#1a1a2e"),
            plot.subtitle=element_text(size=12,hjust=0.5,color="#7f8c8d"),
            plot.background=element_rect(fill="white",color=NA),
            panel.background=element_rect(fill="#fafbfc",color=NA),
            panel.grid.major=element_line(color="#ecf0f1",linewidth=0.5),
            panel.border=element_rect(color="#dcdde1",fill=NA,linewidth=0.8),
            legend.position="top",
            legend.background=element_rect(fill="white",color="#ecf0f1",linewidth=0.5),
            legend.text=element_text(size=12),
            legend.key.width=unit(2,"cm"),
            axis.title=element_text(face="bold",size=13,color="#2c3e50"),
            axis.text=element_text(size=11,color="#7f8c8d"),
            plot.margin=margin(15,20,15,15))
    
    if (length(ip) > 0)
      for (pv in ip)
        p <- p + geom_vline(xintercept=pv, linetype="dotted",
                            color="#95a5a6", linewidth=0.8, alpha=0.7)
    
    # Labels
    lbl <- data.frame(
      x   = c(0.02, min(prev_max+0.08,0.82),
              min(homa_prev+0.08,0.72), min(homa_prev+0.08,0.72)),
      y   = c(Cy+0.03, min(max_by+0.05,0.97),
              0.08, min(by_at_homa+0.05,0.97)),
      lab = c(paste0("CY = ",sprintf("%.3f",Cy)),
              paste0("Max BY = ",sprintf("%.3f",max_by),"\nP = ",sprintf("%.3f",prev_max)),
              paste0("P0 = ",sprintf("%.3f",homa_prev)),
              paste0("BY(P0) = ",sprintf("%.3f",by_at_homa))),
      stringsAsFactors=FALSE)
    
    for (k in seq_len(nrow(lbl)))
      p <- p + geom_label(data=lbl[k,], mapping=aes(x=x,y=y,label=lab),
                          hjust=0, color="#2c3e50", fill="white",
                          size=3.8, fontface="bold",
                          label.size=0.3, label.padding=unit(0.25,"lines"),
                          inherit.aes=FALSE)
    
    if (length(ip) > 0) {
      int_df <- data.frame(
        x=ifelse(seq_along(ip)==1, ip-0.09, ip+0.02),
        y=rep(0.70, length(ip)),
        lab=paste0("P",seq_along(ip)," = ",sprintf("%.3f",ip)),
        stringsAsFactors=FALSE)
      p <- p + geom_label(data=int_df, mapping=aes(x=x,y=y,label=lab),
                          hjust=0.5, color="#2c3e50", fill="#f0f0f0",
                          size=3.8, fontface="bold",
                          label.size=0.3, label.padding=unit(0.25,"lines"),
                          inherit.aes=FALSE)
    }
    return(p)
  })
  
  output$youden_plot <- renderPlot({ build_plot() }, res=120)
  
  # ── Bootstrap tab ────────────────────────────────────────────
  boot_data <- eventReactive(input$run_boot, {
    req(input$boot_vp, input$boot_fp, input$boot_fn, input$boot_vn, input$boot_B)
    withProgress(message="Running bootstrap...", value=0, {
      set.seed(42)
      res <- run_bootstrap(input$boot_vp, input$boot_fp,
                           input$boot_fn, input$boot_vn,
                           B = input$boot_B)
      incProgress(1)
      res
    })
  })
  
  output$boot_results <- renderUI({
    req(boot_data())
    res <- boot_data()
    VP <- input$boot_vp; FP <- input$boot_fp
    FN <- input$boot_fn; VN <- input$boot_vn
    n_pos <- VP + FN;    n_neg <- VN + FP
    Se_obs <- VP / n_pos;  Sp_obs <- VN / n_neg
    Cy_obs  <- Se_obs + Sp_obs - 1
    mb_obs  <- find_max_by_r(Se_obs, Sp_obs)
    int_obs <- find_intersections_r(Se_obs, Sp_obs)
    
    colnames(res) <- c("Cy","max_BY","P_max","P1","P2")
    obs_vals <- c(Cy=Cy_obs, max_BY=mb_obs$max_by,
                  P_max=mb_obs$p_max, P1=int_obs[1], P2=int_obs[2])
    labels <- c(Cy="C_Y", max_BY="Max B_Y",
                P_max="P at Max B_Y",
                P1="P1  (C_Y = B_Y)",
                P2="P2  (C_Y = B_Y)")
    
    rows <- paste(sapply(names(labels), function(k) {
      v  <- res[!is.na(res[,k]), k]
      lo <- quantile(v, 0.025)
      hi <- quantile(v, 0.975)
      se <- sd(v)
      n_valid <- length(v)
      paste0("<tr>",
             "<td>", labels[k], "</td>",
             "<td class='obs'>", sprintf("%.4f", obs_vals[k]), "</td>",
             "<td class='ic'>",  sprintf("%.4f", lo), "</td>",
             "<td class='ic'>",  sprintf("%.4f", hi), "</td>",
             "<td>", sprintf("%.4f", se), "</td>",
             "<td>", n_valid, "</td>",
             "</tr>")
    }), collapse="")
    
    HTML(paste0(
      "<div style='background:#f8f9fa; padding:20px; border-radius:10px;'>",
      "<h5 style='margin-top:0; color:#1a1a2e;'>Results — B = ", input$boot_B,
      " replicates</h5>",
      "<p style='color:#7f8c8d; font-size:13px;'>",
      "n<sub>+</sub> = ", n_pos, " positives &nbsp;|&nbsp; ",
      "n<sub>-</sub> = ", n_neg, " negatives &nbsp;|&nbsp; ",
      "Se = ", sprintf("%.4f", Se_obs), " &nbsp;|&nbsp; ",
      "Sp = ", sprintf("%.4f", Sp_obs), "</p>",
      "<table class='boot-table'>",
      "<thead><tr>",
      "<th>Statistic</th><th>Observed</th>",
      "<th>IC 95% Lower</th><th>IC 95% Upper</th>",
      "<th>Bootstrap SE</th><th>Valid reps</th>",
      "</tr></thead><tbody>", rows, "</tbody></table>",
      "<p style='margin-top:12px; font-size:12px; color:#95a5a6;'>",
      "Percentile method. P1 and P2 valid replicates exclude cases ",
      "where no intersection was found.</p>",
      "</div>"
    ))
  })
  
  # ══════════════════════════════════════════════════════════════
  # NEW: server logic for the "Robustness Check (S2)" tab
  # ══════════════════════════════════════════════════════════════
  robust_data <- eventReactive(input$run_robust, {
    req(input$boot_vp, input$boot_fp, input$boot_fn, input$boot_vn,
        input$boot_B, input$robust_p0)
    
    VP <- input$boot_vp; FP <- input$boot_fp
    FN <- input$boot_fn; VN <- input$boot_vn
    P0 <- input$robust_p0
    B  <- input$boot_B
    
    withProgress(message = "Running robustness check...", value = 0, {
      
      # Parametric (independent-binomial, fixed-margin) bootstrap —
      # the same sampling model as the "Bootstrap CI" tab, extended
      # with By* evaluated at P0
      incProgress(0.15, detail = "Parametric bootstrap")
      set.seed(42)
      param_res <- run_bootstrap_ext(VP, FP, FN, VN, P0, B = B)
      
      # Non-parametric case-resampling bootstrap via the boot package
      incProgress(0.55, detail = "Non-parametric case-resampling bootstrap")
      df_cases <- data.frame(cell = c(rep("TP", VP), rep("FP", FP),
                                      rep("FN", FN), rep("TN", VN)))
      set.seed(42)
      stat_fn  <- make_case_resample_stat(P0)
      boot_obj <- tryCatch(
        boot(data = df_cases, statistic = stat_fn, R = B),
        error = function(e) NULL
      )
      
      incProgress(1, detail = "Done")
      list(param = param_res, boot_obj = boot_obj, P0 = P0,
           VP = VP, FP = FP, FN = FN, VN = VN)
    })
  })
  
  # NEW: histograms of the parametric bootstrap distributions
  #      (Cy*, By* at P0, By* at its maximum), with skewness annotated
  output$robust_hist <- renderPlot({
    req(robust_data())
    rr <- robust_data()
    param <- as.data.frame(rr$param)
    
    long_df <- data.frame(
      value = c(param$Cy, param$By_P0, param$By_max),
      stat  = factor(
        rep(c("Cy*", paste0("By*(P0=", sprintf("%.3f", rr$P0), ")"), "By*(max)"),
            each = nrow(param)),
        levels = c("Cy*", paste0("By*(P0=", sprintf("%.3f", rr$P0), ")"), "By*(max)")
      )
    )
    
    skew_labels <- long_df %>%
      group_by(stat) %>%
      summarise(sk = skewness_fn(value), .groups = "drop") %>%
      mutate(lab = paste0("skewness = ", sprintf("%.3f", sk)))
    
    ggplot(long_df, aes(x = value)) +
      geom_histogram(bins = 40, fill = "#2980b9", alpha = 0.75, color = "white") +
      facet_wrap(~ stat, scales = "free", nrow = 1) +
      geom_text(data = skew_labels,
                aes(x = Inf, y = Inf, label = lab),
                hjust = 1.05, vjust = 1.5, inherit.aes = FALSE,
                size = 4, fontface = "bold", color = "#2c3e50") +
      labs(title = "Empirical bootstrap distributions (parametric, independent-binomial)",
           x = "Value", y = "Frequency") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14, color = "#1a1a2e"),
            strip.text = element_text(face = "bold", size = 12),
            panel.grid.minor = element_blank())
  }, res = 120)
  
  # NEW: comparison table — percentile vs BCa, parametric vs
  #      non-parametric case-resampling
  output$robust_summary <- renderUI({
    req(robust_data())
    rr <- robust_data()
    VP <- rr$VP; FP <- rr$FP; FN <- rr$FN; VN <- rr$VN; P0 <- rr$P0
    param <- as.data.frame(rr$param)
    
    n_pos <- VP + FN; n_neg <- VN + FP
    Se_obs <- VP / n_pos; Sp_obs <- VN / n_neg
    Cy_obs    <- Se_obs + Sp_obs - 1
    By_P0_obs <- calculate_PPV(Se_obs, Sp_obs, P0) + calculate_NPV(Se_obs, Sp_obs, P0) - 1
    mb_obs    <- find_max_by_r(Se_obs, Sp_obs)
    obs_vals  <- c(Cy = Cy_obs, By_P0 = By_P0_obs, By_max = mb_obs$max_by)
    labels    <- c(Cy = "C_Y*", By_P0 = paste0("B_Y* at P0=", sprintf("%.3f", P0)),
                   By_max = "B_Y* (maximum)")
    
    boot_obj <- rr$boot_obj
    if (is.null(boot_obj)) {
      return(div(class = "info-box",
                 p(style = "color:#c0392b; font-weight:600;",
                   "The non-parametric case-resampling bootstrap could not be computed for this configuration (e.g. too many degenerate resamples). Only the parametric results are shown.")))
    }
    
    rows <- paste(sapply(seq_along(labels), function(k) {
      col_name <- names(labels)[k]
      x_param  <- param[[col_name]]
      lo_p <- quantile(x_param, 0.025, na.rm = TRUE)
      hi_p <- quantile(x_param, 0.975, na.rm = TRUE)
      sk_p <- skewness_fn(x_param)
      
      x_nonparam <- boot_obj$t[, k]
      lo_np <- quantile(x_nonparam, 0.025, na.rm = TRUE)
      hi_np <- quantile(x_nonparam, 0.975, na.rm = TRUE)
      
      bca <- tryCatch({
        ci <- boot.ci(boot_obj, type = "bca", index = k)
        ci$bca[4:5]
      }, error = function(e) c(NA_real_, NA_real_))
      
      flag <- if (!is.na(sk_p) && abs(sk_p) > 0.5)
        "<span style='color:#e67e22; font-weight:600;'>BCa recommended</span>"
      else
        "<span style='color:#27ae60;'>approx. symmetric</span>"
      
      paste0("<tr>",
             "<td>", labels[k], "</td>",
             "<td class='obs'>", sprintf("%.4f", obs_vals[k]), "</td>",
             "<td class='ic'>[", sprintf("%.4f", lo_p), ", ", sprintf("%.4f", hi_p), "]</td>",
             "<td>", sprintf("%.4f", sk_p), "</td>",
             "<td>", flag, "</td>",
             "<td class='ic'>[", sprintf("%.4f", lo_np), ", ", sprintf("%.4f", hi_np), "]</td>",
             "<td class='ic'>",
             if (any(is.na(bca))) "n/a" else paste0("[", sprintf("%.4f", bca[1]), ", ", sprintf("%.4f", bca[2]), "]"),
             "</td>",
             "</tr>")
    }), collapse = "")
    
    HTML(paste0(
      "<div style='background:#f8f9fa; padding:20px; border-radius:10px;'>",
      "<h5 style='margin-top:0; color:#1a1a2e;'>Robustness comparison — B = ", input$boot_B, " replicates</h5>",
      "<table class='boot-table'>",
      "<thead><tr>",
      "<th>Statistic</th><th>Observed</th>",
      "<th>Parametric 95% percentile CI</th><th>Skewness (parametric)</th><th>Recommendation</th>",
      "<th>Non-parametric 95% percentile CI</th><th>Non-parametric 95% BCa CI</th>",
      "</tr></thead><tbody>", rows, "</tbody></table>",
      "<p style='margin-top:12px; font-size:12px; color:#95a5a6;'>",
      "Parametric = independent-binomial bootstrap with fixed margins (n+, n-), as in the Bootstrap CI tab. ",
      "Non-parametric = case-resampling bootstrap over N = ", VP+FP+FN+VN,
      " reconstructed subject-level records, letting both margins vary freely (equivalent to a multinomial resample of the four confusion-matrix cells). ",
      "BCa intervals use jackknife-based acceleration computed automatically by the <code>boot</code> package.</p>",
      "</div>"
    ))
  })
  
  # ── Confusion matrix ─────────────────────────────────────────
  output$cm_output <- renderUI({
    req(input$sensitivity, input$specificity)
    cm <- reconstruct_cm(input$sensitivity, input$specificity, 0.484, N=93)
    HTML(paste0(
      "<div class='cm-wrap'><table><thead><tr>",
      "<th></th><th>Observed: IR</th><th>Observed: NIR</th><th>Total</th>",
      "</tr></thead><tbody>",
      "<tr><td class='rh'>Algorithm: IR</td>",
      "<td class='tp'>",cm$TP,"<br><small>(TP)</small></td>",
      "<td class='fp'>",cm$FP,"<br><small>(FP)</small></td>",
      "<td class='tc'>",cm$TP+cm$FP,"</td></tr>",
      "<tr><td class='rh'>Algorithm: NIR</td>",
      "<td class='fn'>",cm$FN,"<br><small>(FN)</small></td>",
      "<td class='tn'>",cm$TN,"<br><small>(TN)</small></td>",
      "<td class='tc'>",cm$FN+cm$TN,"</td></tr>",
      "<tr><td class='rh'>Total</td>",
      "<td class='tc'>",cm$n_positive,"</td>",
      "<td class='tc'>",cm$n_negative,"</td>",
      "<td class='tc'><b>",cm$N,"</b></td></tr>",
      "</tbody></table></div>"
    ))
  })
  
  output$cm_metrics <- renderUI({
    req(input$sensitivity, input$specificity)
    Sen <- input$sensitivity; Esp <- input$specificity; P <- 0.484
    cm  <- reconstruct_cm(Sen, Esp, P, N=93)
    PPV <- calculate_PPV(Sen, Esp, P); NPV <- calculate_NPV(Sen, Esp, P)
    Cy  <- Sen + Esp - 1;              By  <- PPV + NPV - 1
    
    card <- function(title, val, sub, col)
      div(class="metric-card", style=paste0("border-left-color:",col,";"),
          p(style="margin:0;font-size:12px;color:#7f8c8d;", HTML(title)),
          p(style="margin:5px 0 0;font-size:22px;font-weight:700;color:#2c3e50;",
            sprintf("%.4f", val)),
          p(style="margin:5px 0 0;font-size:11px;color:#bdc3c7;", sub))
    
    div(style="background:#f8f9fa;padding:20px;border-radius:12px;margin-top:20px;",
        h5(style="margin-top:0;margin-bottom:15px;font-size:16px;",
           "Derived Metrics at P = 0.484"),
        div(style="display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:15px;",
            card("Se (Classical)",        Sen, "TP/(TP+FN)",              "#27ae60"),
            card("Sp (Classical)",        Esp, "TN/(TN+FP)",              "#2980b9"),
            card("Se<sub>B</sub> = PPV",  PPV, paste0("TP/(TP+FP)=",cm$TP,"/",(cm$TP+cm$FP)), "#8e44ad"),
            card("Sp<sub>B</sub> = NPV",  NPV, paste0("TN/(TN+FN)=",cm$TN,"/",(cm$TN+cm$FN)), "#e67e22"),
            card("C<sub>Y</sub>",         Cy,  "Se + Sp - 1",             "#3498db"),
            card("B<sub>Y</sub>",         By,  "PPV + NPV - 1",           "#e74c3c")
        ))
  })
  
  # ── Value table ──────────────────────────────────────────────
  output$value_table <- renderDT({
    req(rd())
    target <- round(seq(0.05, 0.95, by=0.05), 2)
    df <- rd() %>%
      mutate(Prev_r = round(Prev,2)) %>%
      filter(Prev_r %in% target) %>%
      select(Prev, PPV, NPV, By, Cy) %>%
      mutate(across(where(is.numeric), ~round(.x,4))) %>%
      rename("Prevalence"="Prev","PPV = Se_B"="PPV","NPV = Sp_B"="NPV",
             "B_Y (Bayesian)"="By","C_Y (Classical)"="Cy")
    datatable(df, extensions="Buttons",
              options=list(pageLength=10, dom="Bfrtip", scrollX=TRUE,
                           buttons=c("copy","csv","excel"),
                           columnDefs=list(list(className="dt-center",targets="_all"))),
              class="display compact stripe hover", rownames=FALSE)
  })
  
  # ── Interpretation ───────────────────────────────────────────
  output$interpretation_youden <- renderUI({
    Sen <- input$sensitivity; Esp <- input$specificity
    data <- rd()
    max_by   <- max(data$By, na.rm=TRUE)
    prev_max <- data$Prev[which.max(data$By)]
    cy_val   <- Sen + Esp - 1
    ip       <- ints()
    asym <- if (Esp > Sen+0.05) "Higher Sp => BY optimal at low prevalences"
    else if (Sen > Esp+0.05) "Higher Se => BY optimal at high prevalences"
    else "Well-balanced => BY optimal near P = 0.50"
    int_text <- if (length(ip)>0)
      paste0("<p>Intersection points (CY = BY): ",
             paste(paste0("<b>P",seq_along(ip)," = ",sprintf("%.3f",ip),"</b>"),collapse=" &nbsp; "),
             "</p>")
    else "<p>No intersection points found.</p>"
    div(class="info-box", style="margin-top:20px;",
        h5(style="margin-top:0;","Interpretation"),
        p(HTML(paste0("<b>CY</b> = ",sprintf("%.3f",cy_val),
                      " &nbsp;|&nbsp; <b>Max BY</b> = ",sprintf("%.3f",max_by),
                      " at P = ",sprintf("%.3f",prev_max)))),
        HTML(int_text),
        p(HTML(paste0("<b>Asymmetry:</b> ", asym))))
  })
  
  # ── Sidebar metrics ──────────────────────────────────────────
  output$metrics_info <- renderUI({
    Sen <- input$sensitivity; Esp <- input$specificity; P <- 0.484
    Cy  <- Sen + Esp - 1
    PPV <- calculate_PPV(Sen, Esp, P); NPV <- calculate_NPV(Sen, Esp, P)
    By  <- PPV + NPV - 1; ip <- ints()
    int_text <- if (length(ip)>0)
      paste(sapply(seq_along(ip), function(i)
        paste0("<span style='font-size:13px;font-weight:500;'>P<sub>",i,"</sub> = ",
               sprintf("%.3f",ip[i]),"</span>")), collapse="<br>")
    else "<span style='font-size:12px;color:#95a5a6;'>No intersections</span>"
    row_h <- function(lbl,val,col)
      div(style="display:flex;justify-content:space-between;padding:7px 0;border-bottom:1px solid #ecf0f1;",
          span(style="font-size:13px;", HTML(lbl)),
          span(style=paste0("font-size:14px;font-weight:700;color:",col,";"),
               sprintf("%.4f",val)))
    div(row_h("Se (classical)",    Sen, "#27ae60"),
        row_h("Sp (classical)",    Esp, "#2980b9"),
        row_h("Se<sub>B</sub>=PPV",PPV, "#8e44ad"),
        row_h("Sp<sub>B</sub>=NPV",NPV, "#e67e22"),
        div(style="border-top:2px solid #ecf0f1;margin:5px 0;"),
        row_h("C<sub>Y</sub>",     Cy,  "#3498db"),
        row_h("B<sub>Y</sub>(P0)", By,  "#e74c3c"),
        div(style="border-top:2px solid #ecf0f1;margin:8px 0;"),
        div(p(style="font-size:12px;font-weight:600;margin-bottom:6px;",
              HTML("C<sub>Y</sub>=B<sub>Y</sub> at:")),
            HTML(int_text)))
  })
  
  # ── Download ─────────────────────────────────────────────────
  output$download_plot <- downloadHandler(
    filename = function() paste0("BY_CY_",format(Sys.time(),"%Y%m%d_%H%M%S"),".png"),
    content  = function(file)
      ggsave(filename=file, plot=build_plot(), device="png",
             width=12, height=8, dpi=300, bg="white")
  )
}

shinyApp(ui = ui, server = server)
