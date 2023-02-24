library(purrr)
library(ggplot2)

my_plot <- function(a, A, upper_bound_function) {
    start_number <- upper_bound_function(a, A)
    original_start_number <- round(start_number, 4)
    if (start_number < 0) {
        start_number <- 0
    } else {
        start_number <- ceiling(start_number)
    }
    start_number <- start_number + 1
    values <- 30
    x_values <- seq(start_number, start_number + values, by = 1)
    y_values <- x_values
    for (i in seq(1, values+1, by = 1)) {
        y_values[i] <- abs(a**y_values[i])
    }
    data <- data.frame(n = x_values, y = y_values)

    graph <- ggplot(data, aes(x = n, y = y)) +
        geom_point(size = 2) +
        geom_line() +
        geom_hline(
            aes(yintercept = A, colour = "A", ),
            linetype = "dashed"
        ) +
        geom_vline(
            aes(xintercept = original_start_number, colour = "log_a A"),
            linetype = "dashed"
        ) +
        scale_color_manual(
            name = "Legenda", 
            values = c("A" = "blue", "log_a A" = "red")) +
        scale_x_continuous(
            breaks = c(original_start_number,x_values), 
            labels = c(original_start_number, x_values)) +
        theme(axis.text.x = element_text(angle = 90))
    return(graph)
}

plot <- my_plot(1.2, 2, function(a, A) log(A, base = abs(a)))
plot
