library(tidyverse) 
library(patchwork)

x_labs <- c("Control", "Endophyte", "Mycorrhiza", "Interaction")
x_factor <- factor(x_labs, levels = x_labs)

# -------------------------------------------------------------
# Case of NO interaction:
#  - Endophyte increase by +3
#  - Mycorrhiza increase by +2
#  - Interaction is just additive: 5+3+2 = 10

data_no_int <- data.frame(x = x_factor,
                         y = c(5, 8, 7, 10))

p_no_interaction <- ggplot(data = data_no_int, aes(x = x, y = y)) +
  geom_col() + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 1)) +
  labs(x = "",
       title = "No interaction") +
  geom_hline(yintercept = 10, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_no_interaction


# -------------------------------------------------------------
data_synergetic <- data.frame(x = x_factor,
                         y = c(5, 8, 7, 12))

p_synergetic <- ggplot(data = data_synergetic, aes(x = x, y = y)) +
  geom_col() + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 1)) +
  labs(x = "",
       y = "",
       title = "Synergetic") +
  geom_hline(yintercept = 10, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


p_synergetic


# -----------------------

data_antagonistic <- data.frame(x = x_factor,
                         y = c(5, 8, 7, 9))

p_antagonistic <- ggplot(data = data_antagonistic, aes(x = x, y = y)) +
  geom_col() + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 1)) +
  labs(x = "",
       y = "",
       title = "Antagonistic") +
  geom_hline(yintercept = 10, linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p_antagonistic

# --------------------------

p_no_interaction + p_synergetic + p_antagonistic


