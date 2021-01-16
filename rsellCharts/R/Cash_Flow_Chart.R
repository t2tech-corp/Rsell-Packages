#' Creates Cash Flow Chart
#'
#' This function Creates Cash Flow Chart.
#'
#' Requires: ggplot2, dplyr
#'
#' @param cf_table The Cash Flow Table.
#' @param chart_title The Chart Title.
#' @return Chart for presentation.
#' @export
#' @examples
#'

Cash_Flow_Chart <- function(cf_table, chart_title) {

    ##### Set Cash Outlays to Negative

    chart_data <- cf_table %>%
                  mutate(tool_tip_in = scales::dollar(week_in),
                         tool_tip_ot = scales::dollar(week_out)) %>%
                  mutate(week_out = week_out * -1)

    ##### Set CSS #####

    tooltip_css   <- "background-color: #565455; color: white; padding: 10px; border-radius: 5px;"
    hover_css     <- "fill: #f4bec3;"
    hover_inv_css <- "opacity: 0.35;"

    ##### Set Y Axis #####

    man_y <- pretty(c(min(chart_data$week_out), max(chart_data$week_in)))

    ##### Create Chart #####

    m_chart <- ggplot() +

                labs(title = chart_title, x = "", y = "") +

                theme_light(base_family = "sans") +

                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank()) +

                theme(axis.text.y  = element_text(size = 8),
                      axis.text.x  = element_text(size = 8),
                      plot.title   = element_text(face = "bold", size = 10)) +

                geom_hline(yintercept = man_y[man_y != 0], linetype = "dashed", size = .5, color = "#cccbce") +

                geom_bar_interactive(data = chart_data,
                                     aes(x = o_date,
                                         y = week_out,
                                         fill = "#a2a0a6",
                                         tooltip = tool_tip_ot,
                                         data_id = week_out),
                                     stat = "identity",
                                     width = 3.0) +

                geom_bar_interactive(data = chart_data,
                                     aes(x = o_date,
                                         y = week_in,
                                         fill = "#f4bec3",
                                         tooltip = tool_tip_in,
                                         data_id = week_in),
                                     stat = "identity",
                                     width = 3.0) +

                geom_hline(yintercept = 0, linetype = "solid", size = .5, color = "#cccbce") +

                coord_cartesian(xlim = c(min(chart_data$o_date) - weeks(1), Sys.Date() + weeks(1))) +

                scale_fill_manual(values = c("#a2a0a6", "#f4bec3"), guide = FALSE) +

                scale_y_continuous(breaks = c(man_y),
                                   labels=scales::dollar_format()) +

                scale_x_date(breaks = scales::date_breaks("months"),
                             labels = scales::date_format("%b-%Y"))

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
