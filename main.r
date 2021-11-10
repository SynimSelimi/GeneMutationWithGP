mutate <- function(solution, probability, variance, range) {
    mutated_solution <- solution
    
    for (index in 1:length(mutated_solution)) {
        val <- mutated_solution[index]
        random <- runif(1, 0, 1)

        if (probability > random) {
            addition <- rnorm(1, mean=0, sd=sqrt(variance))
            mutation <- val + addition
            while (mutation < -range || mutation > range) {
                addition <- rnorm(1, mean=0, sd=sqrt(variance))
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
probability <- 0.25
variance <- 0.005

mutated_solution = mutate(solution, probability, variance, range)
sprintf("%.3f", solution)
sprintf("%.3f", mutated_solution)
plot(solution)
plot(mutated_solution)
