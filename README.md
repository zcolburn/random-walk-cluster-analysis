# Random walk cluster analysis
Using a distance matrix as input, perform random walks between neighbors to analyze the clustering of cells.


Randomly walk between k nearest neighbors and keep track of the total number of unique cells that have been touched at each time step.


An example output is depicted below. Each line represents a single cell. The y-axis represents the proportion of the total number of unique cells that were touched during the course of the simulation. The x-axis indicates the step number in the simulation. It is important to subset the data before graphing because the total number of data points grows very quickly. In this simulation, the results of 20 simulations of 10,000 steps each are depicted. Since this would result in 200,000 data points, it is easy to imagine a larger data set taking a very long to generate a figure.


![Output graph](output.png "Example output graph")
