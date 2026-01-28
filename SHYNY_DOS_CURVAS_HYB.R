# ══════════════════════════════════════════════════════════════════
# SHINY APP: ACCURACY, PPV+NPV
# ══════════════════════════════════════════════════════════════════

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)

# ══════════════════════════════════════════════════════════════════
# AUXILIARY FUNCTIONS
# ══════════════════════════════════════════════════════════════════

calculate_PPV <- function(Sen, Esp, Prev) {
  numerator <- Sen * Prev
  denominator <- Sen * Prev + (1 - Esp) * (1 - Prev)
  if (denominator == 0) return(0)
  return(numerator / denominator)
}

calculate_NPV <- function(Sen, Esp, Prev) {
  numerator <- Esp * (1 - Prev)
  denominator <- (1 - Sen) * Prev + Esp * (1 - Prev)
  if (denominator == 0) return(0)
  return(numerator / denominator)
}

calculate_accuracy <- function(Sen, Esp, Prev) {
  return(Sen * Prev + Esp * (1 - Prev))
}

# ══════════════════════════════════════════════════════════════════
# UI (USER INTERFACE)
# ══════════════════════════════════════════════════════════════════

ui <- fluidPage(
  
  # Title
  titlePanel(
    tags$div(
      h2("Comparison of Binary Classification Metrics",
         style = "text-align: center; color: #2E86AB; font-weight: bold; font-size: 24px;"),
      p("Explore how sensitivity, specificity, and prevalence affect different metrics",
        style = "text-align: center; color: #333333; font-size: 16px; margin-top: 10px;")
    )
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      h4 {
        color: #495057;
        font-weight: bold;
        font-size: 18px !important;
      }
      .btn {
        margin-bottom: 5px;
        font-size: 14px !important;
      }
      .nav-tabs > li > a {
        font-weight: bold;
        font-size: 16px !important;
      }
      body {
        font-size: 16px !important;
      }
      .slider-label {
        font-size: 16px !important;
        color: #333333 !important;
        font-weight: bold !important;
      }
    "))
  ),
  
  # Layout
  sidebarLayout(
    
    # ════════════════════════════════════════════════════════════════
    # SIDEBAR PANEL (Controls)
    # ════════════════════════════════════════════════════════════════
    
    sidebarPanel(
      width = 3,
      
      h4("⚙️ Classifier Parameters", style = "font-size: 18px; color: #333333;"),
      
      # Sensitivity Slider
      div(style = "margin-bottom: 25px;",
          tags$label("Sensitivity (True Positive Rate)", 
                     class = "slider-label",
                     style = "display: block; margin-bottom: 8px; color: #333333; font-size: 16px;"),
          sliderInput(
            inputId = "sensitivity",
            label = NULL,
            min = 0.05,
            max = 1.00,
            value = 0.91,
            step = 0.01
          )
      ),
      
      # Specificity Slider
      div(style = "margin-bottom: 25px;",
          tags$label("Specificity (True Negative Rate)", 
                     class = "slider-label",
                     style = "display: block; margin-bottom: 8px; color: #333333; font-size: 16px;"),
          sliderInput(
            inputId = "specificity",
            label = NULL,
            min = 0.05,
            max = 1.00,
            value = 0.98,
            step = 0.01
          )
      ),
      
      hr(style = "border-top: 2px solid #ddd;"),
      
      # Calculated Information
      h4("📊 Calculated Metrics", style = "font-size: 18px; color: #333333; margin-top: 20px;"),
      
      uiOutput("metrics_info"),
      
      hr(style = "border-top: 2px solid #ddd;"),
      
      # Predefined Example Buttons
      h4("🎯 Quick Examples", style = "font-size: 18px; color: #333333; margin-top: 20px;"),
      
      actionButton("example1", "Balanced (0.85, 0.85)",
                   class = "btn-primary",
                   style = "width: 100%; margin-bottom: 10px; font-size: 15px; padding: 10px;"),
      
      actionButton("example2", "High Specificity (0.70, 0.95)",
                   class = "btn-info",
                   style = "width: 100%; margin-bottom: 10px; font-size: 15px; padding: 10px;"),
      
      actionButton("example3", "High Sensitivity (0.95, 0.70)",
                   class = "btn-success",
                   style = "width: 100%; margin-bottom: 10px; font-size: 15px; padding: 10px;"),
      
      actionButton("example4", "HOMA Case (0.91, 0.98)",
                   class = "btn-warning",
                   style = "width: 100%; font-size: 15px; padding: 10px;")
    ),
    
    # ════════════════════════════════════════════════════════════════
    # MAIN PANEL (Graphs and Table)
    # ════════════════════════════════════════════════════════════════
    
    mainPanel(
      width = 9,
      
      # Tabs
      tabsetPanel(
        type = "tabs",
        id = "mainTabs",
        
        # Tab 1: Accuracy vs PPV+NPV
        tabPanel(
          "📊 Accuracy vs PPV+NPV",
          value = "tab1",
          br(),
          plotOutput("accuracy_plot", height = "600px"),
          br(),
          uiOutput("interpretation_accuracy")
        ),
        
        # Tab 2: Youden vs PPV+NPV-1
        tabPanel(
          "📈 Youden vs PPV+NPV-1",
          value = "tab2",
          br(),
          plotOutput("youden_plot", height = "600px"),
          br(),
          uiOutput("interpretation_youden")
        ),
        
        # Tab 3: Value Table
        tabPanel(
          "📋 Value Table",
          value = "tab3",
          br(),
          h4("Metric values at different prevalences", style = "font-size: 20px; color: #333333;"),
          div(style = 'overflow-x: auto; font-size: 14px;', 
              dataTableOutput("value_table"))
        ),
        
        # Tab 4: Information
        tabPanel(
          "ℹ️ Information",
          value = "tab4",
          br(),
          h3("About the Metrics", style = "color: #2E86AB; font-size: 22px;"),
          
          h4("🔵 Accuracy", style = "font-size: 18px; color: #333333;"),
          p("Proportion of correct predictions (TP + TN) over the total.
            Varies linearly with prevalence: Accuracy = Sen×Prev + Esp×(1-Prev).",
            style = "font-size: 16px;"),
          
          h4("🔴 PPV + NPV", style = "font-size: 18px; color: #333333;"),
          p("Sum of Positive Predictive Value and Negative Predictive Value.
            Reflects the combined clinical utility of the classifier. Has a
            bell shape, with maximum at intermediate prevalences. Range: [0, 2].",
            style = "font-size: 16px;"),
          
          h4("🔵 Youden Index (Sen + Esp - 1)", style = "font-size: 18px; color: #333333;"),
          p("Prevalence-independent metric that characterizes the intrinsic
            capacity of the classifier. Range: [-1, 1].",
            style = "font-size: 16px;"),
          
          h4("🔴 PPV+NPV-1", style = "font-size: 18px; color: #333333;"),
          p("Normalized index based on predictive values. Depends on prevalence
            and reflects contextual clinical utility. Range: [-1, 1].",
            style = "font-size: 16px;"),
          
          hr(),
          
          h4("💡 Key Observations:", style = "font-size: 18px; color: #333333;"),
          tags$ul(
            tags$li("Accuracy varies LINEARLY with prevalence", style = "font-size: 16px;"),
            tags$li("PPV+NPV has a BELL shape (non-linear)", style = "font-size: 16px;"),
            tags$li("Youden Index is CONSTANT (horizontal line)", style = "font-size: 16px;"),
            tags$li("PPV+NPV-1 also has a BELL shape", style = "font-size: 16px;"),
            tags$li("HOMA Prevalence (P=0.484) is marked with orange vertical line", style = "font-size: 16px;")
          ),
          
          hr(),
          
          h4("📚 Formulas:", style = "font-size: 18px; color: #333333;"),
          tags$ul(
            tags$li(HTML("<b>Accuracy:</b> Sen×Prev + Esp×(1-Prev)"), style = "font-size: 16px;"),
            tags$li(HTML("<b>PPV:</b> (Sen×Prev) / [Sen×Prev + (1-Esp)×(1-Prev)]"), style = "font-size: 16px;"),
            tags$li(HTML("<b>NPV:</b> [Esp×(1-Prev)] / [(1-Sen)×Prev + Esp×(1-Prev)]"), style = "font-size: 16px;"),
            tags$li(HTML("<b>Youden:</b> Sen + Esp - 1"), style = "font-size: 16px;"),
            tags$li(HTML("<b>PPV+NPV-1:</b> PPV + NPV - 1"), style = "font-size: 16px;")
          ),
          
          hr(),
          
          h4("📖 References:", style = "font-size: 18px; color: #333333;"),
          tags$ul(
            tags$li("Altman, D. G., & Bland, J. M. (1994). Diagnostic tests. BMJ.", style = "font-size: 16px;"),
            tags$li("Malagón et al. (2024). Insulin resistance indices validation.", style = "font-size: 16px;"),
            tags$li("HOMA-IR study: Prevalence = 0.48387", style = "font-size: 16px; color: #FF6B35; font-weight: bold;")
          )
        )
      )
    )
  ),
  
  # Footer
  hr(),
  div(style = "text-align: center;",
      p("Developed with R Shiny | Binary classification metrics with imbalance", 
        style = "color: #666666; font-size: 14px;"),
      p("HOMA Prevalence: P = 0.48387", 
        style = "color: #FF6B35; font-size: 14px; font-weight: bold;")
  )
)

# ══════════════════════════════════════════════════════════════════
# SERVER (App Logic)
# ══════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ════════════════════════════════════════════════════════════════
  # REACTIVE DATA
  # ════════════════════════════════════════════════════════════════
  
  reactive_data <- reactive({
    req(input$sensitivity, input$specificity)
    
    Sen <- input$sensitivity
    Esp <- input$specificity
    
    # Create prevalence sequence
    prevs <- seq(0.01, 0.99, by = 0.01)
    
    # Calculate all metrics
    data <- data.frame(Prev = prevs) %>%
      mutate(
        Accuracy = mapply(calculate_accuracy, Sen, Esp, Prev),
        PPV = mapply(calculate_PPV, Sen, Esp, Prev),
        NPV = mapply(calculate_NPV, Sen, Esp, Prev),
        PPV_plus_NPV = PPV + NPV,
        PPV_plus_NPV_minus_1 = PPV_plus_NPV - 1,
        Youden = Sen + Esp - 1
      )
    
    return(data)
  })
  
  # ════════════════════════════════════════════════════════════════
  # METRICS INFORMATION
  # ════════════════════════════════════════════════════════════════
  
  output$metrics_info <- renderUI({
    data <- reactive_data()
    Sen <- input$sensitivity
    Esp <- input$specificity
    Youden <- Sen + Esp - 1
    
    # Find maximum PPV+NPV
    max_ppvnpv <- max(data$PPV_plus_NPV, na.rm = TRUE)
    idx_max_ppvnpv <- which.max(data$PPV_plus_NPV)
    prev_max_ppvnpv <- data$Prev[idx_max_ppvnpv]
    
    # Accuracy range
    min_acc <- min(data$Accuracy, na.rm = TRUE)
    max_acc <- max(data$Accuracy, na.rm = TRUE)
    
    # HOMA prevalence value
    homa_prev <- 0.48387
    accuracy_at_homa <- data$Accuracy[which.min(abs(data$Prev - homa_prev))]
    ppvnpv_at_homa <- data$PPV_plus_NPV[which.min(abs(data$Prev - homa_prev))]
    
    HTML(paste0(
      "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px; border-left: 5px solid #2E86AB;'>",
      "<h5 style='margin-top: 0; color: #333333; font-size: 17px; font-weight: bold;'>📈 Current Metrics</h5>",
      "<table style='width: 100%; font-size: 16px;'>",
      "<tr><td><b style='color: #333333;'>Sensitivity:</b></td><td style='text-align: right; color: #333333;'>", round(Sen, 3), "</td></tr>",
      "<tr><td><b style='color: #333333;'>Specificity:</b></td><td style='text-align: right; color: #333333;'>", round(Esp, 3), "</td></tr>",
      "<tr><td><b style='color: #333333;'>Youden Index:</b></td><td style='text-align: right; color: #333333;'>", round(Youden, 3), "</td></tr>",
      "<tr><td colspan='2'><hr style='margin: 8px 0; border-color: #ddd;'></td></tr>",
      "<tr><td><b style='color: #333333;'>Accuracy Range:</b></td><td style='text-align: right; color: #333333;'>[", round(min_acc, 3), ", ", round(max_acc, 3), "]</td></tr>",
      "<tr><td><b style='color: #333333;'>Max PPV+NPV:</b></td><td style='text-align: right; color: #333333;'>", round(max_ppvnpv, 3), "</td></tr>",
      "<tr><td><b style='color: #333333;'>At Prev:</b></td><td style='text-align: right; color: #333333;'>", round(prev_max_ppvnpv*100, 1), "%</td></tr>",
      "<tr><td colspan='2'><hr style='margin: 8px 0; border-color: #ddd;'></td></tr>",
      "<tr><td><b style='color: #FF6B35;'>At HOMA (P=0.484):</b></td><td></td></tr>",
      "<tr><td style='padding-left: 20px; color: #333333;'>Accuracy:</td><td style='text-align: right; color: #333333;'>", round(accuracy_at_homa, 3), "</td></tr>",
      "<tr><td style='padding-left: 20px; color: #333333;'>PPV+NPV:</td><td style='text-align: right; color: #333333;'>", round(ppvnpv_at_homa, 3), "</td></tr>",
      "</table>",
      "</div>"
    ))
  })
  
  # ════════════════════════════════════════════════════════════════
  # PLOT 1: ACCURACY vs PPV+NPV
  # ════════════════════════════════════════════════════════════════
  
  output$accuracy_plot <- renderPlot({
    req(reactive_data())
    
    data <- reactive_data()
    Sen <- input$sensitivity
    Esp <- input$specificity
    homa_prev <- 0.48387
    
    # Find maximum PPV+NPV
    max_ppvnpv <- max(data$PPV_plus_NPV, na.rm = TRUE)
    idx_max_ppvnpv <- which.max(data$PPV_plus_NPV)
    prev_max_ppvnpv <- data$Prev[idx_max_ppvnpv]
    
    # Find values at HOMA prevalence
    homa_idx <- which.min(abs(data$Prev - homa_prev))
    accuracy_at_homa <- data$Accuracy[homa_idx]
    ppvnpv_at_homa <- data$PPV_plus_NPV[homa_idx]
    
    # Create plot
    p <- ggplot(data, aes(x = Prev)) +
      
      # Accuracy line
      geom_line(aes(y = Accuracy, color = "Accuracy"), 
                size = 2, alpha = 0.9) +
      
      # PPV+NPV line
      geom_line(aes(y = PPV_plus_NPV, color = "PPV + NPV"), 
                size = 2, alpha = 0.9) +
      
      # Vertical line at maximum PPV+NPV
      geom_vline(xintercept = prev_max_ppvnpv, 
                 linetype = "dashed", color = "#A23B72", 
                 size = 1, alpha = 0.7) +
      
      # Vertical line for HOMA prevalence
      geom_vline(xintercept = homa_prev, 
                 linetype = "longdash", color = "#FF6B35", 
                 size = 1.5, alpha = 0.9) +
      
      # Points at maximum
      geom_point(data = data[idx_max_ppvnpv, ], 
                 aes(y = PPV_plus_NPV),
                 color = "#A23B72", size = 5, shape = 17) +
      
      # Points at HOMA prevalence
      geom_point(aes(x = homa_prev, y = accuracy_at_homa),
                 color = "#2E86AB", size = 5, shape = 16) +
      
      geom_point(aes(x = homa_prev, y = ppvnpv_at_homa),
                 color = "#A23B72", size = 5, shape = 16) +
      
      # Color scale
      scale_color_manual(
        values = c("Accuracy" = "#2E86AB", 
                   "PPV + NPV" = "#A23B72"),
        name = "Metric"
      ) +
      
      # X-axis
      scale_x_continuous(
        name = "Prevalence",
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(0, 1, by = 0.1),
        limits = c(0, 1),
        expand = c(0, 0)
      ) +
      
      # Y-axis
      scale_y_continuous(
        name = "Metric Value",
        limits = c(0.5, 2),
        breaks = seq(0.5, 2, by = 0.25)
      ) +
      
      # Labels
      labs(
        title = "Accuracy vs PPV+NPV",
        subtitle = paste0("Sensitivity = ", round(Sen, 3), 
                          " | Specificity = ", round(Esp, 3),
                          " | HOMA Prevalence = ", round(homa_prev, 3))
      ) +
      
      # Theme
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 24, color = "#333333"),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "#333333"),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 16, color = "#333333"),
        legend.text = element_text(size = 15, color = "#333333"),
        legend.key.size = unit(1.5, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(color = "gray70", fill = NA, size = 1),
        axis.title = element_text(face = "bold", size = 18, color = "#333333"),
        axis.text = element_text(size = 14, color = "#333333"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15))
      )
    
    # Add annotations with larger text
    p <- p +
      annotate("text", 
               x = prev_max_ppvnpv - 0.05, 
               y = max_ppvnpv + 0.07,
               label = paste0("Max PPV+NPV\n", round(max_ppvnpv, 3)),
               size = 5, hjust = 1, color = "#A23B72", fontface = "bold") +
      
      annotate("text", 
               x = homa_prev, 
               y = 0.55,
               label = paste0("HOMA\nP=", homa_prev),
               size = 6, hjust = 0.5, vjust = 0,
               color = "#FF6B35", fontface = "bold") +
      
      annotate("text", 
               x = homa_prev + 0.02, 
               y = accuracy_at_homa,
               label = paste0("Acc=", round(accuracy_at_homa, 3)),
               size = 5, hjust = 0, color = "#2E86AB", fontface = "bold") +
      
      annotate("text", 
               x = homa_prev + 0.02, 
               y = ppvnpv_at_homa,
               label = paste0("PPV+NPV=", round(ppvnpv_at_homa, 3)),
               size = 5, hjust = 0, color = "#A23B72", fontface = "bold")
    
    return(p)
  })
  
  # ════════════════════════════════════════════════════════════════
  # PLOT 2: YOUDEN vs PPV+NPV-1
  # ════════════════════════════════════════════════════════════════
  
  output$youden_plot <- renderPlot({
    req(reactive_data())
    
    data <- reactive_data()
    Sen <- input$sensitivity
    Esp <- input$specificity
    Youden <- Sen + Esp - 1
    homa_prev <- 0.48387
    
    # Find maximum PPV+NPV-1
    max_ppvnpv1 <- max(data$PPV_plus_NPV_minus_1, na.rm = TRUE)
    idx_max <- which.max(data$PPV_plus_NPV_minus_1)
    prev_max <- data$Prev[idx_max]
    
    # Find value at HOMA prevalence
    homa_idx <- which.min(abs(data$Prev - homa_prev))
    ppvnpv1_at_homa <- data$PPV_plus_NPV_minus_1[homa_idx]
    
    # Create plot - SIN ÁREA DEBAJO DE LA CURVA
    p <- ggplot(data, aes(x = Prev)) +
      
      # Youden horizontal line
      geom_hline(aes(yintercept = Youden, color = "Youden Index"),
                 size = 2, alpha = 0.9) +
      
      # PPV+NPV-1 curve - SIN ÁREA
      geom_line(aes(y = PPV_plus_NPV_minus_1, color = "PPV+NPV-1"),
                size = 2, alpha = 0.9) +
      
      # Vertical line at maximum
      geom_vline(xintercept = prev_max,
                 linetype = "dashed", color = "#A23B72", size = 1, alpha = 0.7) +
      
      # Vertical line for HOMA prevalence
      geom_vline(xintercept = homa_prev,
                 linetype = "longdash", color = "#FF6B35", 
                 size = 1.5, alpha = 0.9) +
      
      # Points
      geom_point(data = data[idx_max, ],
                 aes(y = PPV_plus_NPV_minus_1),
                 color = "#A23B72", size = 5, shape = 17) +
      
      # Points at HOMA prevalence
      geom_point(aes(x = homa_prev, y = Youden),
                 color = "#2E86AB", size = 5, shape = 16) +
      
      geom_point(aes(x = homa_prev, y = ppvnpv1_at_homa),
                 color = "#A23B72", size = 5, shape = 16) +
      
      # Color scale
      scale_color_manual(
        values = c("Youden Index" = "#2E86AB", 
                   "PPV+NPV-1" = "#A23B72"),
        name = "Metric"
      ) +
      
      # X-axis
      scale_x_continuous(
        name = "Prevalence",
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(0, 1, by = 0.1),
        limits = c(0, 1),
        expand = c(0, 0)
      ) +
      
      # Y-axis
      scale_y_continuous(
        name = "Metric Value",
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1)
      ) +
      
      # Labels
      labs(
        title = "Youden Index vs PPV+NPV-1",
        subtitle = paste0("Sensitivity = ", round(Sen, 3), 
                          " | Specificity = ", round(Esp, 3),
                          " | Youden = ", round(Youden, 3),
                          " | HOMA Prevalence = ", round(homa_prev, 3))
      ) +
      
      # Theme
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 24, color = "#333333"),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "#333333"),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 16, color = "#333333"),
        legend.text = element_text(size = 15, color = "#333333"),
        legend.key.size = unit(1.5, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(color = "gray70", fill = NA, size = 1),
        axis.title = element_text(face = "bold", size = 18, color = "#333333"),
        axis.text = element_text(size = 14, color = "#333333"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15))
      )
    
    # Add annotations with larger text
    p <- p +
      annotate("text", 
               x = prev_max + 0.01, 
               y = max_ppvnpv1 + 0.03,
               label = paste0("Maximum\nPrev = ", round(prev_max * 100, 1), "%\n",
                              "Value = ", round(max_ppvnpv1, 3)),
               size = 5, hjust = 0, color = "#A23B72", fontface = "bold") +
      
      annotate("text", 
               x = 0.95, 
               y = Youden + 0.03,
               label = paste0("Youden = ", round(Youden, 3)),
               size = 5, hjust = 1, color = "#2E86AB", fontface = "bold") +
      
      annotate("text", 
               x = homa_prev+0.10, 
               y = 0.1,
               label = paste0("HOMA\nP=", homa_prev),
               size = 6, hjust = 0.5, vjust = 0,
               color = "#FF6B35", fontface = "bold") +
      
      annotate("text", 
               x = homa_prev + 0.02, 
               y = Youden - 0.02,
               label = paste0("Youden=", round(Youden, 3)),
               size = 5, hjust = 0, color = "#2E86AB", fontface = "bold") +
      
      annotate("text", 
               x = homa_prev + 0.02, 
               y = ppvnpv1_at_homa + 0.02,
               label = paste0("PPV+NPV-1=", round(ppvnpv1_at_homa, 3)),
               size = 5, hjust = 0, color = "#A23B72", fontface = "bold")
    
    return(p)
  })
  
  # ════════════════════════════════════════════════════════════════
  # INTERPRETATIONS
  # ════════════════════════════════════════════════════════════════
  
  output$interpretation_accuracy <- renderUI({
    data <- reactive_data()
    Sen <- input$sensitivity
    Esp <- input$specificity
    homa_prev <- 0.48387
    
    # Find maximum
    max_ppvnpv <- max(data$PPV_plus_NPV, na.rm = TRUE)
    idx_max_ppvnpv <- which.max(data$PPV_plus_NPV)
    prev_max_ppvnpv <- data$Prev[idx_max_ppvnpv]
    
    # Values at HOMA
    homa_idx <- which.min(abs(data$Prev - homa_prev))
    acc_at_homa <- data$Accuracy[homa_idx]
    ppvnpv_at_homa <- data$PPV_plus_NPV[homa_idx]
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 5px solid #2E86AB;'>",
      "<h5 style='margin-top: 0; color: #333333; font-size: 20px; font-weight: bold;'><span style='font-size: 1.5em;'>💡</span> Interpretation</h5>",
      
      "<p style='font-size: 17px; color: #333333;'><b style='color: #2E86AB;'>Accuracy</b> shows a <b>linear relationship</b> with prevalence, ",
      "ranging from ", round(min(data$Accuracy), 3), " to ", round(max(data$Accuracy), 3), ".</p>",
      
      "<p style='font-size: 17px; color: #333333;'><b style='color: #A23B72;'>PPV+NPV</b> exhibits a <b>bell-shaped curve</b>, ",
      "reaching its peak of ", round(max_ppvnpv, 3), " at prevalence = ", 
      round(prev_max_ppvnpv * 100, 1), "%.</p>",
      
      "<p style='font-size: 18px; color: #333333; font-weight: bold;'><span style='color: #FF6B35;'>At HOMA Prevalence (", homa_prev, "):</span></p>",
      "<ul style='font-size: 16px; color: #333333;'>",
      "<li><span style='color: #2E86AB;'>Accuracy</span> = ", round(acc_at_homa, 3), "</li>",
      "<li><span style='color: #A23B72;'>PPV+NPV</span> = ", round(ppvnpv_at_homa, 3), "</li>",
      "</ul>",
      
      "<p style='font-size: 17px; color: #333333; margin-top: 15px;'><b>Clinical Insight:</b> While Accuracy measures overall correctness, ",
      "PPV+NPV better captures clinical utility by combining both positive ",
      "and negative predictive values, which are crucial for medical decision-making.</p>",
      "</div>"
    ))
  })
  
  output$interpretation_youden <- renderUI({
    Sen <- input$sensitivity
    Esp <- input$specificity
    data <- reactive_data()
    homa_prev <- 0.48387
    
    # Find maximum
    max_ppvnpv1 <- max(data$PPV_plus_NPV_minus_1, na.rm = TRUE)
    idx_max <- which.max(data$PPV_plus_NPV_minus_1)
    prev_max <- data$Prev[idx_max]
    
    # Values at HOMA
    homa_idx <- which.min(abs(data$Prev - homa_prev))
    ppvnpv1_at_homa <- data$PPV_plus_NPV_minus_1[homa_idx]
    youden_val <- Sen + Esp - 1
    
    # Determine asymmetry
    if (Esp > Sen + 0.05) {
      asymmetry <- "The classifier has <b>higher specificity than sensitivity</b>, 
                   performing best at <b>low prevalences</b> where false positives are minimized."
      color <- "#17a2b8"
    } else if (Sen > Esp + 0.05) {
      asymmetry <- "The classifier has <b>higher sensitivity than specificity</b>, 
                   performing best at <b>high prevalences</b> where false negatives are minimized."
      color <- "#28a745"
    } else {
      asymmetry <- "The classifier is <b>well-balanced</b> (sensitivity ≈ specificity), 
                   with optimal performance near 50% prevalence."
      color <- "#ffc107"
    }
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 5px solid ", color, ";'>",
      "<h5 style='margin-top: 0; color: #333333; font-size: 20px; font-weight: bold;'><span style='font-size: 1.5em;'>💡</span> Interpretation</h5>",
      
      "<p style='font-size: 17px; color: #333333;'><b style='color: #2E86AB;'>Youden Index</b> = ", round(youden_val, 3), 
      " remains <b>constant</b> across all prevalences, representing the classifier's ",
      "intrinsic ability to distinguish between classes.</p>",
      
      "<p style='font-size: 17px; color: #333333;'><b style='color: #A23B72;'>PPV+NPV-1</b> varies with prevalence, reaching a maximum of ",
      round(max_ppvnpv1, 3), " at prevalence = ", round(prev_max * 100, 1), "%.</p>",
      
      "<p style='font-size: 18px; color: #333333; font-weight: bold;'><span style='color: #FF6B35;'>At HOMA Prevalence (", homa_prev, "):</span></p>",
      "<ul style='font-size: 16px; color: #333333;'>",
      "<li><span style='color: #2E86AB;'>Youden Index</span> = ", round(youden_val, 3), "</li>",
      "<li><span style='color: #A23B72;'>PPV+NPV-1</span> = ", round(ppvnpv1_at_homa, 3), "</li>",
      "</ul>",
      
      "<p style='font-size: 17px; color: #333333; margin-top: 15px;'><b>Asymmetry Analysis:</b> ", asymmetry, "</p>",
      
      "<p style='font-size: 17px; color: #333333;'><b>Key Difference:</b> Youden is a prevalence-independent measure of classifier ",
      "capability, while PPV+NPV-1 is a prevalence-dependent measure of clinical utility ",
      "in specific populations.</p>",
      "</div>"
    ))
  })
  
  # ════════════════════════════════════════════════════════════════
  # VALUE TABLE
  # ════════════════════════════════════════════════════════════════
  
  output$value_table <- renderDataTable({
    req(reactive_data())
    
    data <- reactive_data() %>%
      filter(Prev %in% seq(0.05, 0.95, by = 0.05)) %>%
      select(Prev, Accuracy, PPV, NPV, PPV_plus_NPV, PPV_plus_NPV_minus_1, Youden) %>%
      mutate(
        Prev = paste0(round(Prev * 100, 0), "%"),
        across(where(is.numeric), ~round(.x, 4))
      ) %>%
      rename(
        "Prevalence" = Prev,
        "PPV+NPV" = PPV_plus_NPV,
        "PPV+NPV-1" = PPV_plus_NPV_minus_1
      )
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        scrollX = TRUE,
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '16px', 'font-weight': 'bold'});",
          "}"
        )
      ),
      class = 'display compact stripe hover',
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: #333333; font-size: 18px; font-weight: bold;',
        "Metric values at different prevalence levels"
      )
    ) %>%
      formatStyle(
        'Prevalence',
        backgroundColor = styleEqual(
          "48%", 
          '#FFF3CD'
        ),
        fontWeight = styleEqual(
          "48%",
          'bold'
        )
      ) %>%
      formatStyle(
        columns = names(data),
        fontSize = '15px'
      )
  })
  
  # ════════════════════════════════════════════════════════════════
  # EXAMPLE BUTTONS
  # ════════════════════════════════════════════════════════════════
  
  observeEvent(input$example1, {
    updateSliderInput(session, "sensitivity", value = 0.85)
    updateSliderInput(session, "specificity", value = 0.85)
    updateTabsetPanel(session, "mainTabs", selected = "tab1")
  })
  
  observeEvent(input$example2, {
    updateSliderInput(session, "sensitivity", value = 0.70)
    updateSliderInput(session, "specificity", value = 0.95)
    updateTabsetPanel(session, "mainTabs", selected = "tab2")
  })
  
  observeEvent(input$example3, {
    updateSliderInput(session, "sensitivity", value = 0.95)
    updateSliderInput(session, "specificity", value = 0.70)
    updateTabsetPanel(session, "mainTabs", selected = "tab1")
  })
  
  observeEvent(input$example4, {
    updateSliderInput(session, "sensitivity", value = 0.91)
    updateSliderInput(session, "specificity", value = 0.98)
    updateTabsetPanel(session, "mainTabs", selected = "tab1")
  })
}

# ══════════════════════════════════════════════════════════════════
# RUN THE APP
# ══════════════════════════════════════════════════════════════════

shinyApp(ui = ui, server = server)
