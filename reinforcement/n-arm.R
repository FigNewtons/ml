# Comparsion of the e-greedy and softmax approach
# to the n-bandit problem

library(ggplot2)

# epsilon-greedy selection 
egreedy <- function(n = 10, times = 200, epsilon = 0.01){
    
    total_reward <- 0
    counts <- rep(0, n)
    averages <- rep(0, n)
    
    for(i in 1:times){
        
        choice <- ifelse(runif(1) < epsilon, 
                         sample.int(n, size=1), 
                         which.max(averages))
        
        reward <- rnorm(1, mean = choice)
        
        # Update values to reflect choice    
        total_reward <- total_reward + reward
        counts[choice] <- counts[choice] + 1 
        
        # Update rule: Q_{k + 1} = Q_{k} + (r_{k + 1} - Q_{k}) / (k + 1)
        averages[choice] <- 
            averages[choice] + (reward - averages[choice]) / counts[choice]
    }
    
    total_reward
}

# softmax selection using Gibbs distribution
softmax <- function(n = 10, times = 200, temp = 0.50){
    
    total_reward <- 0
    counts <- rep(0, n)
    averages <- rep(0, n)
    gibbs <- rep(0, n)
    
    for(i in 1:times){
        
        for(j in 1:n){
            gibbs[j] <- exp(averages[j] / temp) / sum(exp(averages / temp))
        }
        
        choice <- sample.int(n, size = 1, prob = gibbs)
        reward <- rnorm(1, mean = choice)
        
        # Update everything else
        total_reward <- total_reward + reward
        counts[choice] <- counts[choice] + 1 
        
        # Update rule: Q_{k + 1} = Q_{k} + (r_{k + 1} - Q_{k}) / (k + 1)
        averages[choice] <- 
            averages[choice] + (reward - averages[choice]) / counts[choice]  
    }
    
    total_reward
}


run <- function(trials = 100, n = 10, times = 1000, e = 0.01, t = 0.50){

    greedy_results <-
        replicate(trials, egreedy(n=n, times=times, epsilon=0)) / (times * n)
    e_results <- 
        replicate(trials, egreedy(n=n, times=times, epsilon=e)) / (times * n)
    softmax_results <- 
        replicate(trials, softmax(n=n, times=times, temp=t)) / (times * n)
    
    results <- 
        data.frame(cbind(1:trials, greedy_results, e_results, softmax_results))
    names(results) <- c("n", "Greed", "E", "Softmax")
    
    ggplot(results) +
        geom_point(aes(x = n, y = Greed, color = "blue")) + 
        geom_smooth(method = "lm", aes(x = n, y = Greed, color = "blue")) +
        
        geom_point(aes(x = n, y = E, color = "green")) +
        geom_smooth(method = "lm", aes(x = n, y = E, color = "green")) +
        
        geom_point(aes(x = n, y = Softmax, color = "red")) +
        geom_smooth(method = "lm", aes(x = n, y = Softmax, color = "red")) +
        
        labs(x = "Trial", y = "Average %") +
        scale_color_discrete(name="Method", 
                             labels=c("Pure Greed", "Greed 0.01", "Softmax")) 
        
}