#' Creates Poshmark Bar Chart
#'
#' This function Creates Poshmark Bar Chart.
#'
#' Requires: ggplot2, scales
#'
#' @param chart_data  The Chart Data.
#' @param factor_var  The Factor Variable.
#' @param x_axis      The X-Axis.
#' @param chart_title The Chart Title.
#' @param chart_type  The Chart Type
#' @return Chart for presentation.
#' @export
#' @examples
#'

PM_Bar_Charts <- function(chart_data, factor_var, x_axis, chart_title, chart_type) {

    ##### Set Y Axis #####

    man_y <- pretty(c(0, chart_data[[x_axis]]))

    ##### Create Chart #####

    w_chart <- ggplot2::ggplot(data = chart_data,
                               aes(x = reorder(factor(!!as.symbol(factor_var)), !!as.symbol(x_axis)),
                                   y = !!as.symbol(x_axis),
                                   fill = "#fbe7e9")) +

                        geom_hline(yintercept = man_y, linetype = "dashed", size = .5, color = "#cccbce")   +

                        labs(title = chart_title, x = "", y = "") +

                        geom_bar(stat  = "identity",
                                 width = .5) +

                        theme_bw() +

                        coord_flip(expand = TRUE) +

                        scale_fill_manual(values = c("#fbe7e9"), guide = FALSE) +

                        theme(axis.text.x = element_text(size = 10),
                              axis.text.y = element_text(size = 10),
                              plot.title  = element_text(face = "bold")) +

                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              panel.background = element_blank(), axis.line = element_line(colour = "#404041"))

    ###

    if(chart_type == "dollar") {

        w_chart <- w_chart + ggplot2::scale_y_continuous(breaks = man_y, limits = c(min(man_y), max(man_y)), labels = scales::dollar(man_y, big.mark = ","))

    } else {

        w_chart <- w_chart + ggplot2::scale_y_continuous(breaks = man_y, limits = c(min(man_y), max(man_y)))

    }

    ###

    return(w_chart)

}
