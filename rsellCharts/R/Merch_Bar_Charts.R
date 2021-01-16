#' Creates Merchant Bar Chart
#'
#' This function Creates Merchant Bar Chart.
#'
#' Requires: ggplot2, dplyr, scales
#'
#' @param chart_data  The Chart Data.
#' @param x_axis      The X-Axis.
#' @param chart_type  The Chart Type
#' @param chart_title The Chart Title.
#' @return Chart for presentation.
#' @export
#' @examples
#'

Merch_Bar_Charts <- function(chart_data, x_axis, chart_type, chart_title) {

    chart_data <- chart_data %>%
                  mutate(sold_site = as.factor(sold_site),
                         tool_tip = case_when(chart_type == "Count"  ~ scales::comma(!!as.symbol(x_axis)),
                                              chart_type == "Dollar" ~ scales::dollar(!!as.symbol(x_axis))))

    ##### Set CSS #####

    tooltip_css   <- "background-color: #565455; color: white; padding: 10px; border-radius: 5px;"
    hover_css     <- "fill: #f4bec3;"
    hover_inv_css <- "opacity: 0.35;"

    ##### Set Y Axis #####

    man_y <- pretty(c(0, max(chart_data[[x_axis]])))

    ##### Create Chart #####

    m_chart <- ggplot(data = chart_data,
                      aes(x = sold_site,
                          y = .data[[x_axis]],
                          fill = "#f4bec3",
                          tooltip = tool_tip,
                          data_id = .data[[x_axis]])) +

                labs(title = chart_title, x = "", y = "") +

                theme_light(base_family = "sans") +

                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank()) +

                theme(axis.text.y  = element_text(size = 12),
                      axis.text.x  = element_text(size = 12),
                      plot.title   = element_text(face = "bold", size = 16)) +

                geom_hline(yintercept = man_y[man_y != 0], linetype = "dashed", size = .5, color = "#cccbce") +

                geom_bar_interactive(stat = "identity", width = .5) +

                geom_hline(yintercept = 0, linetype = "solid",  size = .5, color = "#cccbce") +

                scale_fill_manual(values = c("#f4bec3"), guide = FALSE)

    ###

    if(chart_type == "Dollar") {

        m_chart <- m_chart + scale_y_continuous(breaks = c(man_y),
                                                labels = scales::dollar_format(),
                                                limits = c(min(man_y), max(man_y)))

    } else {

        m_chart <- m_chart + scale_y_continuous(breaks = c(man_y),
                                                labels = scales::comma_format(),
                                                limits = c(min(man_y), max(man_y)))

    }

    ###

    g_chart <- girafe(ggobj = m_chart,
                      fonts = list(sans = "Arial"),
                      width_svg = 8, height_svg = 5)

    g_chart <- girafe_options(x = g_chart,
                              opts_tooltip(css = tooltip_css, offx = -80, offy = -80),
                              opts_hover(css = hover_css),
                              opts_hover_inv(css = hover_inv_css),
                              opts_toolbar(saveaspng = FALSE),
                              opts_selection(type = "none"))

    ###

    return(g_chart)

}
