mutate <- function(solution, probability, variance, range) {
    mutated_solution <- solution
    
    for (index in 1:length(mutated_solution)) {
        val <- mutated_solution[index]
        addition <- 0
        random <- runif(1, 0, 1)
        if (probability > random) {
            addition <- runif(1, 0, 1)
            mutation <- val + addition
            while (mutation < -range || mutation > range) {
                addition <- runif(1, 0, 1)
                mutation <- val + addition
            }
            mutated_solution[index] = mutation
        }
    }
    
    mutated_solution
}

range <- 5
values <- seq(from=-range, to=range, by=.01)
solution <- sample(values, size=10, replace=TRUE)
probability <- 1
variance <- 0.005

mutated_solution = mutate(solution, probability, variance, range)
print(solution)
print(mutated_solution)
plot(solution)
plot(mutated_solution)
