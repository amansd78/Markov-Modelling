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




s_space <- c('car', 'empty', 'm.cycle')
s_space

# Define the generator matrix
g_matrix <- matrix(c(-1/8, 1/8, 0, .75*3/20, -3/20, .25*3/20, 0, 1/3, -1/3),
                   byrow = TRUE, nrow = 3, dimnames = list(s_space, s_space))
g_matrix

# Create a CTMC object
MC_service <- new('ctmc', states = s_space, generator = g_matrix, name = 'Service Time')
MC_service
plot(MC_service)

# Convert generator matrix to transition matrix
t_matrix <- generatorToTransitionMatrix(g_matrix, byrow = TRUE)
t_matrix

# Calculate expected hitting time from state i to state j
ExpectedTime(MC_service, 2, 3)

# Check if the CTMC object is irreducible
is.CTMCirreducible(MC_service)

# Check if the CTMC object is time reversible
is.TimeReversible(MC_service)

# Find the probability at time T = 2 from initial stage = 1
probabilityatT(MC_service, 2, 1, useRCpp = TRUE)

# Find the probability at time T = 8 from initial stage = 3
probabilityatT(MC_service, 8, 3, useRCpp = TRUE)

# Generate 20 random CTMC trajectories
rctmc(20, MC_service, T = 0, include.T0 = TRUE, out.type = "df")



# Define the state space for a more complex CTMC
s_space_complex <- c('A', 'B', 'C', 'D', 'E')
s_space_complex

# Define a larger generator matrix
g_matrix_complex <- matrix(c(-0.2, 0.1, 0.05, 0.03, 0.02,
                             0.15, -0.25, 0.1, 0.03, 0.02,
                             0.05, 0.1, -0.3, 0.05, 0.1,
                             0.08, 0.12, 0.05, -0.25, 0,
                             0.1, 0.15, 0.2, 0.1, -0.55),
                           byrow = TRUE, nrow = 5, dimnames = list(s_space_complex, s_space_complex))
g_matrix_complex

# Create a CTMC object for the more complex CTMC
MC_complex <- new('ctmc', states = s_space_complex, generator = g_matrix_complex, name = 'Complex CTMC')
MC_complex
plot(MC_complex)

# Convert generator matrix to transition matrix for the complex CTMC
t_matrix_complex <- generatorToTransitionMatrix(g_matrix_complex, byrow = TRUE)
t_matrix_complex

# Calculate expected hitting time for specific state transitions in the complex CTMC
ExpectedTime(MC_complex, 'A', 'E')
ExpectedTime(MC_complex, 'B', 'D')

# Check if the complex CTMC object is irreducible and time reversible
is.CTMCirreducible(MC_complex)
is.TimeReversible(MC_complex)

# Find the probability at time T = 5 from initial stage = 'C' in the complex CTMC
probabilityatT(MC_complex, 5, 'C', useRCpp = TRUE)

# Generate 30 random CTMC trajectories for the complex CTMC
rctmc(30, MC_complex, T = 0, include.T0 = TRUE, out.type = "df")


