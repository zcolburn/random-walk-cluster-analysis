# Load the pipe operator.
library(magrittr)

# Load ggplot2.
library(ggplot2)

# Create fake expression data.frame.
num_cells <- 1000
num_genes <- 10
expression_levels <- matrix(rnorm(
  num_genes*num_cells, sd = 10
), ncol = num_cells)
colnames(expression_levels) <- paste0("Cell", 1:num_cells)
rownames(expression_levels) <- paste0("Gene", 1:num_genes)
expression_levels <- as.data.frame(expression_levels)

# Create a distance matrix.
cor_distance <- cor(expression_levels)

# Create a distance matrix
dist_matrix <- as.matrix(dist(
  cor_distance, method = "euclidean", diag = TRUE, upper = TRUE
))


# Find k nearest neighbors for each point and store in nn.
k <- 5
n <- ncol(dist_matrix)
nn <- matrix(0, n, k) # n x k
for (i in 1:n){
  nn[i,] = FastKNN::k.nearest.neighbors(i, dist_matrix, k = k)
}

# Perform random walks.
# In parallel with reduced matrix.
set.seed(8)
num_steps <- 10000
num_cells <- 20
record_at_steps <- as.integer(num_steps/100)
traffic_recorder <- as.vector(matrix(0, 1, n))
passengers_recorder <- matrix(0, num_cells, num_steps)
cells_of_interest <- sample(1:n, num_cells)
data_in <- lapply(
  split(1:num_cells, 1:parallel::detectCores()),
  function(cell_ids){
    tibble::lst(
      nn = nn,
      cell_ids = cell_ids,
      internal_cell_id = 1:length(cell_ids),
      num_steps = num_steps,
      record_at_steps = record_at_steps,
      k = k
    )
  }
)

passengers_recorder <- parallel::parLapply(
  parallel::makeCluster(parallel::detectCores()),
  data_in,
  function(data_in){
    steps_to_record <- seq(1, data_in$num_steps, data_in$record_at_steps)
    if(tail(steps_to_record, 1) != data_in$num_steps){
      steps_to_record <- c(steps_to_record, data_in$num_steps)
    }
    steps_to_record[2:(length(steps_to_record)-1)] <- (
      steps_to_record[2:(length(steps_to_record)-1)] -1
    )
    
    num_records <- length(steps_to_record)
    
    options("scipen"=100, "digits"=12)
    
    passengers_recorder <- matrix(0, length(data_in$cell_ids), num_records)
    colnames(passengers_recorder) <- steps_to_record
    rownames(passengers_recorder) <- data_in$cell_ids
    
    for (cell in rownames(passengers_recorder)){
      current_cell <- cell
      stops_on_the_way <- c()
      for (step in 1:data_in$num_steps){
        next_cell <- data_in$nn[as.integer(current_cell), ceiling(runif(1)*data_in$k)]
        stops_on_the_way[as.character(next_cell)] <- TRUE
        if(step %in% steps_to_record){
          passengers_recorder[cell, sprintf("%d",step)] <- length(stops_on_the_way)
        }
        current_cell <- next_cell
      }
    }
    
    library(magrittr)
    result <- passengers_recorder %>% 
      as.data.frame() %>%
      tibble::rownames_to_column(., var = "cell") %>%
      tibble::as_data_frame()
    
    return(result)
  }
) %>%
  dplyr::bind_rows()


# Rearrange into an analyzable format.
tall_passengers <- passengers_recorder %>%
  tibble::as_tibble() %>%
  tidyr::gather(., key = "step", value = "count", -cell) %>%
  dplyr::mutate(
    step = factor(
      step,
      levels = as.character(1:num_steps),
      labels = as.character(1:num_steps)
    )
  )


# Graph unsummarized results.
# 
# Subset the data for plotting.
x_axis_mod <- 1000
max_val <- max(as.numeric(as.character(tall_passengers$step)))
valid_times <- c(
  1,
  seq(
    0, max_val, x_axis_mod
  )
)
if(tail(valid_times, 1) != max_val){
  valid_times <- c(valid_times, max_val)
}

tall_passengers %>%
  dplyr::filter(
    .,
    as.character(step) %in% valid_times
  ) %>%
  dplyr::mutate(
    prop = count/n
  ) %>%
  ggplot(
    .,
    aes(step, prop, group = cell)
  )+
  geom_line()+#alpha=.2
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()
  
