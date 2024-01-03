# Clear the workspace
rm(list = ls(all = TRUE))

# Define Driver_zone and Zone_transition for a Markov chain
Driver_zone <- c("North", "South", "West")
Zone_transition <- matrix(c(0.3, 0.3, 0.4, 0.4, 0.4, 0.2, 0.5, 0.3, 0.2),
                          byrow = TRUE, nrow = 3,
                          dimnames = list(Driver_zone, Driver_zone))

# Create a Markov chain object using the 'markovchain' package
library(markovchain)
MC_zone <- new("markovchain", states = Driver_zone,
               transitionMatrix = Zone_transition,
               name = "Movement of Drivers")

# Plot the transition diagram
library(diagram)
plotmat(t(Zone_transition), pos = c(1, 2),
        lwd = 1, box.lwd = 1, cex.txt = 0.8,
        box.size = 0.1, box.type = "circle",
        box.prop = 0.5, box.col = "green",
        arr.length = 0.25, arr.width = 0.2, arr.lcol = "purple",
        self.cex = 0.6, self.shifty = 0.08,
        self.shiftx = 0.05,
        main = "Transition Diagram")

# Calculate probabilities after 2 trips and specific transitions
MC_zone2 <- MC_zone^2 
transitionProbability(MC_zone^2, "North", "West")

# Check properties of the Markov chain
is.irreducible(MC_zone)
period(MC_zone)

# Obtain steady state and long-run distribution
s_state <- steadyStates(MC_zone)
long_run_dist <- 50 * s_state

# Export data from markov chain to data frame
MC_zoneDF <- as(MC_zone, "data.frame")

# Define sequences and estimate transition probabilities
sequence <- c("U", "C", "C", "D", "D", "U", "C", "D", "C", "U", "U", "D", "U", "D", "C", "C",
              "U", "D", "C", "C")
sequenceMatr <- createSequenceMatrix(sequence, sanitize = FALSE)
tran_prob <- createSequenceMatrix(sequence, toRowProbs = TRUE, sanitize = FALSE)

# Create another Markov chain object for match results
MC_team <- new("markovchain", states = c("U", "C", "D"),
               transitionMatrix = tran_prob,
               name = "Match Result")

# Calculate steady state for the match results
st_state <- steadyStates(MC_team)

# Find maximum likelihood estimates for the Markov chain
mcFitMLE <- markovchainFit(data = sequence, method = "mle", byrow = TRUE,
                           confidencelevel = 0.9)

# Define a state space and transition matrix for a Markov chain
s_space <- c("XX", "X*X", "XY", "X*Y")
t_matrix <- matrix(c(0.5, 0, 0.5, 0, 0.25, 0.25, 0.25, 0.25,
                     0.5, 0, 0.5, 0, 0, 0.5, 0.5, 0),
                   byrow = TRUE, nrow = 4,
                   dimnames = list(s_space, s_space))

# Create another Markov chain object
m_chain <- new("markovchain", states = s_space,
               transitionMatrix = t_matrix,
               name = "Markov Chain")

# Plot the transition diagram for the new chain
plotmat(t(t_matrix), pos = c(2, 2), box.size = 0.075,
        main = "Transition Diagram")

# Check properties of the new chain
is.irreducible(m_chain)
period(m_chain)
steadyStates(m_chain)

# Calculate the ratio in the 25th generation from the given initial values
# Initially, X*X = 5, XX = 7, X*Y = 2, XY = 11

# Place your calculations for the 25th generation ratio here

