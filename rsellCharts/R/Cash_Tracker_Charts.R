#' Creates Cash Tracker Charts
#'
#' This function Creates Cash Tracker Charts.
#'
#' Requires: ggplot2, dplyr
#'
#' @param cf_table The Cash Flow Table.
#' @param chart_var The variable name of the measurement to chart.
#' @param chart_title The Chart Title.
#' @return Chart for presentation.
#' @export
#' @examples
#'

Cash_Tracker_Charts <- function(cf_table, chart_var, chart_title) {

    cf_table <- cf_table %>%
                mutate(pos_neg  = !!as.symbol(chart_var) >= 0,
                       tool_tip = scales::dollar(!!as.symbol(chart_var)))

    ##### Set CSS #####

    tooltip_css   <- "background-color: #565455; color: white; padding: 10px; border-radius: 5px;"
    hover_css     <- "fill: #f4bec3;"
    hover_inv_css <- "opacity: 0.35;"

    ##### Set Y Axis #####

    cont_neg <- min(cf_table[[chart_var]]) < 0

    if(cont_neg) { man_y <- pretty(c(min(cf_table[[chart_var]]), max(cf_table[[chart_var]]))) } else
                 { man_y <- pretty(c(0,                          max(cf_table[[chart_var]]))) }

    ##### Create Chart #####

    m_chart <- ggplot(data = cf_table,
                      aes(x = o_date,
                          y = .data[[chart_var]],
                          fill = pos_neg,
                          tooltip = tool_tip,
                          data_id = .data[[chart_var]])) +

                labs(title = chart_title, x = "", y = "") +

                theme_light(base_family = "sans") +

                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank()) +

                theme(axis.text.y  = element_text(size = 8),
                      axis.text.x  = element_text(size = 8),
                      plot.title   = element_text(face = "bold", size = 10)) +

                geom_hline(yintercept = man_y[man_y != 0], linetype = "dashed", size = .5, color = "#cccbce") +

                geom_bar_interactive(stat = "identity", width = 3.0) +

                geom_hline(yintercept = 0, linetype = "solid",  size = .5, color = "#cccbce") +

                coord_cartesian(xlim = c(min(cf_table$o_date) - weeks(1), Sys.Date() + weeks(1))) +

                scale_y_continuous(breaks = c(man_y),
                                   labels = scales::dollar_format()) +

                scale_x_date(breaks = scales::date_breaks("months"),
                             labels = scales::date_format("%b-%Y"))

    ##

    if(cont_neg) { m_chart <- m_chart + scale_fill_manual(values = c("#a2a0a6", "#f4bec3"), guide = FALSE) } else
                 { m_chart <- m_chart + scale_fill_manual(values = c("#f4bec3"),            guide = FALSE) }

    ##

    g_chart <- girafe(ggobj = m_chart,
                      fonts = list(sans = "Arial"),
                      width_svg = 14, height_svg = 4)

    g_chart <- girafe_options(x = g_chart,
                              opts_tooltip(css = tooltip_css, offx = -80, offy = -80),
                              opts_hover(css = hover_css),
                              opts_hover_inv(css = hover_inv_css),
                              opts_toolbar(saveaspng = FALSE),
                              opts_selection(type = "none"))

    ###

    return(g_chart)

}
