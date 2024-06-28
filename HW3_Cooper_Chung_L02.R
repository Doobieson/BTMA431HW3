# Cooper Chung (Redacted Student ID)
# Fall 2022 BTMA 431 HW3

### Question 1 ###
# Initialize variables to utilize in this question.
muffin.cost <- 0.20
customer.cost <- 2.50


### Question 1a. ###
# To solve Question 1a, we would simply multiply the revenue per unit by the number of muffins sold, and subtract the cost to produce each muffin
# multiplied by the amount stocked. Although demand is 120, we have only stocked 100.
# You can only sell however much you have stocked, so since the demand is 120, we will only be able to sell 100, and turn away the other 20 customers.
# Therefore, the profit if only 100 muffins were stocked and 100 were sold would be $230.
profit <- (customer.cost * 100) - (muffin.cost * 100)
print(profit)


### Question 1b. ###
# Using a similar line of logic as Question 1a, we can apply it to the customers as well. Although we've stocked 100, we can only sell however much is demanded.
# In this case, the demand was only 80, so we only sell 80, and incur a net loss on the 20 remaining muffins. However, since producing the muffins is so cheap,
# we still turn a net profit, but not as much as if we were to sell more/stock less. 
# Therefore, the profit if 100 muffins were stocked and 80 were sold is $180.
profit <- (customer.cost * 80) - (muffin.cost * 100)
print(profit)


### Question 1c. ###
# The decision variable here would be how many muffins to stock, and for multiple reasons. The first reason being that it is a variable that we can control.
# For any given day, we are not able to control the demand, but we are able to decide how many muffins to stock. Second, anticipating the demand,
# figuring out the optimal amount to stock, and deciding how many to produce is our "lever", as we are leveraging our power to control how many
# muffins to produce, in order to maximize the expected profit on any given day.


### Question 1d. ###
simulation.attempts <- 100000 # Change to higher number for more accurate results
muffin.demand <- rnorm(simulation.attempts, mean = 120, sd = 25) # Simulate demand for muffins
stocking.level <- 100:200 # Set range for feasible stocking levels
profit <- list()          # Empty lists to store numbers and keep track
amount.sold <- list()

for(stock in stocking.level){               # For each stocking level and for each level of demand
  amount.sold <- pmin(stock, muffin.demand) # Find which one is lower, this is the amount that is sold
  net.profit <- (amount.sold * customer.cost) - (stock * muffin.cost) # Use the amount sold to calculate the net profit
  profit <- append(profit, net.profit)      # Append the net profit to the list of profits
}

profit <- split(unlist(profit), rep(1:101, each=simulation.attempts)) # Split the list of profits 101 times, each time by the amount of simulations (levels of demand) there are

expected.profit <- lapply(profit, mean) # Apply the mean function to each item in the list of profits, output this in a new list

max.expected.profit <- which.max(expected.profit) # Find which entry has the highest expected profit

max.profit.stocking.level <- stocking.level[max.expected.profit] # Find the corresponding entry in the stocking levels that would result in this highest expected profit

max.profit.stocking.level # The stocking level that maximizes the expected profit is 155 muffins


### Question 1e ###
optimal.stocking.level <- 155 # Set optimal stocking level as a global variable

fulfilled.demand <- mean(muffin.demand <= optimal.stocking.level) # From the simulated demand, sum the number of times the demand was less than or equal to the optimal stocking level.
# This gets us the amount of times that the demand was 100% fulfilled. We then divide it by the number of simulations to see how often the demand is fulfilled.
fulfilled.demand # The chance that the demand tomorrow will be fulfilled is 0.92


### Question 1f ###
muffin.demand.understock <- muffin.demand - optimal.stocking.level # Subtract the optimal stocking level from the simulated demand, and store it in a new list

mean.understock <- mean(muffin.demand.understock[muffin.demand.understock > 0]) # In this list, only using the numbers that are greater than 0, take the mean

mean.understock # On average, there is an excess demand of 11.30 muffins when stocking optimally


### Question 1g ###
muffin.demand.overstock <- muffin.demand - optimal.stocking.level # Same as above

mean.overstock <- mean(muffin.demand.overstock[muffin.demand.overstock < 0]) # In this list, only using the numbers that are less than 0, take the mean

mean.overstock # On average, there is a excess supply of 39 muffins when stocking optimally


### Question 1h ###
# We create a function in order to use lapply() in the future
# In this case, the demand was simulated with standard deviation values beginning with 0, and ending at 30 by increments of 5
sd.values <- seq(from = 0, to = 30, by = 5)
profit.sd <- list()
amount.sold.sd <- list()
optimal.stocking.level.sd <- 155 # Designate the optimal stocking level

net.profit.finder <- function(sdlist){
  muffin.demand.sd <- rnorm(simulation.attempts, mean = 120, sd = sdlist) # Demand is simulated with specified standard deviation
  
  amount.sold.sd <- pmin(optimal.stocking.level.sd, muffin.demand.sd)     # Pairwise minimum between the optimal stocking level and the demand
  net.profit.sd <- (amount.sold.sd * customer.cost) - (optimal.stocking.level.sd * muffin.cost)
  profit.sd <- append(profit.sd, mean(net.profit.sd))
}

net.profit.list <- lapply(sd.values, net.profit.finder) # lapply the function created above, on the sd.values list that contains all our required values of standard deviation
net.profit.unlisted <- unlist(net.profit.list)          # Turn the above list into a numeric vector to use in plot

library(ggplot2) # Load library to create plot

qplot(x = sd.values,
      y = net.profit.unlisted,
      xlab = "Demand Standard Deviation",
      ylab = "Expected Profit at Optimal Stocking Level 155",
      geom = c("point", "smooth"))
# We can see that as the standard deviation increases, it has a negative effect on the expected profit.
# Therefore, higher demand variability has a negative effect on expected profit


### Question 1i ###
# Since the optimal stocking level is 155, they should only stock 55, since they are essentially getting 100 for free. The logic follows this: We have already calculated
# that 155 is their optimal stocking level. However, since they are getting 100 for free, they can essentially achieve the same optimal stocking level by stocking only
# 55, while AT THE SAME TIME achieving the SAME SERVICE LEVEL. Therefore, since the optimal stocking level already achieves the highest expected profit, they can cut their stocking
# costs by another $20 with the free 100 muffins.


### Question 2 ###
# Use the stem code provided in the assignment to import the required data
library(dplyr)
url <- "https://s3.amazonaws.com/tripdata/202101-citibike-tripdata.csv.zip"
temp <- tempfile()
download.file(url, temp)
citibike <- read.csv(unz(temp, "202101-citibike-tripdata.csv"), stringsAsFactors = FALSE)
unlink(temp)

citibike.trips <- citibike %>%
  filter(usertype == "Subscriber" & tripduration <= 86400 | usertype == "Customer" & tripduration <= 86400) # Slight modification to the stem code to only include rows that are not
# only subscriber or customer, but also to ignore trip times that are longer than 24 hours.
# There are 86400 seconds in 24 hours, and trips longer than 24 hours are considered lost/stolen bikes.

### Question 2a ###                                                                                         
# Set global variables to detail the proposed overage policy of charging per second
subscriber.threshold <- 45 * 60                 # Amount of time in seconds before subscribers incur overage charges 
subscriber.charge.per.second <- 2.50/(15 * 60)  # Amount of money charged per second for subscribers
customer.threshold <- 30 * 60                   # Amount of time in seconds before customers incur overage charges
customer.charge.per.second <- 3/(30 * 60)       # Amount of money charged per second for customers
overage.counter <- list()                       # Empty list to keep track of overage charges

for(trip in 1:nrow(citibike.trips)){                                                                          # For each recorded trip in citibike.trips
  if(citibike.trips$usertype[trip] == "Customer" & citibike.trips$tripduration[trip] > customer.threshold){   # If the user is a customer, AND the duration of the trip is longer than the customer threshold
    overage.time <- citibike.trips$tripduration[trip] - customer.threshold                                    # Calculate the amount of time they are over by
    overage.charge <- overage.time * customer.charge.per.second                                               # Calculate the amount of money to charge based on how much time they are over by
    overage.counter <- append(overage.counter, overage.charge)                                                # Append this amount of money to the empty list created earlier to keep track
  }
  else if(citibike.trips$usertype[trip] == "Subscriber" & citibike.trips$tripduration[trip] > subscriber.threshold){  # The same as above, except for subscribers instead
    overage.time <- citibike.trips$tripduration[trip] - subscriber.threshold
    overage.charge <- overage.time * subscriber.charge.per.second
    overage.counter <- append(overage.counter, overage.charge)
  }
}

average.overage.charge <- mean(unlist(overage.counter)) # Calculate the average overage charge based on all the overages calculated above
# The average overage charge for a citibike user is 3.96, or $4.00


### Question 2b ###
sd.overage.charge <- sd(unlist(overage.counter)) # Calculate the standard deviation of overage charges based on the overages calculated above
# The Standard Deviation of overage charges is 14.02, or $14.00


### Question 2c ###
# Modify the code in question 2a to separate them into subscribers and customers
subscriber.overages <- list()
for(trip in 1:nrow(citibike.trips)){
  if(citibike.trips$usertype[trip] == "Subscriber" & citibike.trips$tripduration[trip] > subscriber.threshold){
    overage.time <- citibike.trips$tripduration[trip] - subscriber.threshold
    overage.charge <- overage.time * subscriber.charge.per.second
    subscriber.overages <- append(subscriber.overages, overage.charge)
  }  
}

subscriber.revenue <- sum(unlist(subscriber.overages)) # Sum the total overage charges from subscribers to find total revenue from subscribers
# The total revenue from subscribers is $75,631.61, or $76,000

customer.overages <- list()
for(trip in 1:nrow(citibike.trips)){
  if(citibike.trips$usertype[trip] == "Customer" & citibike.trips$tripduration[trip] > customer.threshold){
    overage.time <- citibike.trips$tripduration[trip] - customer.threshold
    overage.charge <- overage.time * customer.charge.per.second
    customer.overages <- append(customer.overages, overage.charge)
  }
}

customer.revenue <- sum(unlist(customer.overages)) # Sum the total overage charges from customers to find total revenue from customers
# The total revenue from customers is $75,147.83, or $75,000


### Question 2d ###
# Reuse code from question 2a with minor modifications
subscriber.charge <- 2.50     # Subscriber charge given in assignment
subscriber.increment <- 900   # 900 seconds is 15 minutes, which is the incremental charge time for subscribers given in the assignment
customer.charge <- 3          # Customer charge given in assignment
customer.increment <- 1800    # 1800 seconds is 30 minutes, which is the incremental charge time for customers given in the assignment
incremental.overage.counter <- list()

for(trip in 1:nrow(citibike.trips)){
  if(citibike.trips$usertype[trip] == "Customer" & citibike.trips$tripduration[trip] > customer.threshold){
    overage.time <- citibike.trips$tripduration[trip] - customer.threshold
    overage.multiplier <- ceiling(overage.time / customer.increment) # Different from code in 2a, this time we find the amount of times a customer would have to pay an overage fee, use ceiling to round up to the nearest whole integer
    overage.charge <- overage.multiplier * customer.charge  # Using this amount of times ("multiplier"), we calculate the overage charge
    incremental.overage.counter <- append(incremental.overage.counter, overage.charge)
  }
  else if(citibike.trips$usertype[trip] == "Subscriber" & citibike.trips$tripduration[trip] > subscriber.threshold){
    overage.time <- citibike.trips$tripduration[trip] - subscriber.threshold
    overage.multiplier <- ceiling(overage.time / subscriber.increment) # Same as above but for subscribers
    overage.charge <- overage.multiplier * subscriber.charge
    incremental.overage.counter <- append(incremental.overage.counter, overage.charge)
  }
}

average.incremental.overage.charge <- mean(unlist(incremental.overage.counter)) 
# The average incremental overage charge is $5.80


### Question 2e ###
sd.incremental.overage.charge <- sd(unlist(incremental.overage.counter))
# The standard deviation of incremental overage charges is 13.93, or $13.90


### Question 2f ###
# To answer this question, we just choose the option that gathers the most revenue for citibike
total.continuous.revenue <- sum(unlist(overage.counter))
total.incremental.revenue <- sum(unlist(incremental.overage.counter))
# In this case, their incremental (current) model for overage charges still gathers them the most revenue. I would recommend that they stick with their current pricing policy.


### Question 2g ###
# Yes, this is the case. Their incremental policy which has the higher expected revenue also has the lower standard deviation compared to their continuous policy
# See question 2e


### Question 2h ###
# Repeat question 2c, but use the code from 2d modified to separate the revenues into subscribers and customers
incremental.subscriber.overages <- list()
for(trip in 1:nrow(citibike.trips)){
  if(citibike.trips$usertype[trip] == "Subscriber" & citibike.trips$tripduration[trip] > subscriber.threshold){
    overage.time <- citibike.trips$tripduration[trip] - subscriber.threshold
    overage.multiplier <- ceiling(overage.time / subscriber.increment)
    overage.charge <- overage.multiplier * subscriber.charge
    incremental.subscriber.overages <- append(incremental.subscriber.overages, overage.charge)
  }
}

expected.subscriber.overage <- mean(unlist(incremental.subscriber.overages)) # Different from 2c, instead of finding the total revenues from subscribers, take the mean to find expected value
# The expected subscriber overage is $7.00

incremental.customer.overages <- list()
for(trip in 1:nrow(citibike.trips)){
  if(citibike.trips$usertype[trip] == "Customer" & citibike.trips$tripduration[trip] > customer.threshold){
    overage.time <- citibike.trips$tripduration[trip] - customer.threshold
    overage.multiplier <- ceiling(overage.time / customer.increment)
    overage.charge <- overage.multiplier * customer.charge
    incremental.customer.overages <- append(incremental.customer.overages, overage.charge)
  }
}  

expected.customer.overage <- mean(unlist(incremental.customer.overages)) # Same as above but for customers
# The expected customer overage is $5.10


### Question 3a ###
# Given a fixed price, the total demand is representative of a binomial distribution. A binomial distribution has only possible outcomes of success or failure.
# In this case, at each fixed price, each person that walks by the stand can only have 2 outcomes: 1. They buy the apple juice (success) or 2. They don't buy the apple juice (failure)


### Question 3b ###
price = 2
customers = 50
purchase.probability <- 1 - (price / 10)

expected.demand <- customers * purchase.probability
# At a price level of $2, we can expect 40 customers to purchase the apple juice


### Question 3c ###
sd.expected.demand <- sqrt(customers * purchase.probability * (1 - purchase.probability))
# At a price level of $2, the standard deviation of the demand is 2.8284


### Question 3d ###
# Repeat question 3b,c but with different values
price = 4
purchase.probability <- 1 - (price / 10)

expected.demand <- customers * purchase.probability
# At a price level of $4, we can expect 30 customers to purchase the apple juice


### Question 3e ###
sd.expected.demand <- sqrt(customers * purchase.probability * (1 - purchase.probability))
# At a price level of $4, the standard deviation of the demand is 3.4641


### Question 4a ###
# With the hint given in the assignment, the marginal cost of stocking one extra muffin would be $0.20
# However, by definition, the marginal cost of production is the amount it costs to produce one additional unit.
# In this case, it is 20 cents, or $0.20.


### Question 4b ###
# By definition, the marginal revenue is the revenue gained from selling one additional item. The formula for this is: Change in Revenue / Change in Quantity.
# Our Revenue is our Demand, and our Quantity is our Stocking level.
# Since the revenue per muffin is $2.50, the expected marginal revenue would be $2.50 multiplied by the amount that the demand is greater than the
# optimal stocking quantity. Therefore, the answer is 2.50 * (D > S*).


### Question 4c ###
# Did not attempt


### Question 5a ###
# This problem is a binomial distribution because each ticket is either success or failure of being drawn
# First, we set our overall probability. This is essentially how many tickets we have out of the entire pool of tickets created.
overall.probability <- 10000/20000000000

# Second, we use our overall odds, and try it across a whole week. In this case, we are looking for odds that in a whole week,
# our probability of getting drawn once, happens 0 times. In this case, the probability of that happening is 0.904, or 0.90.
no.draws <- pbinom(q = 0, size = 201600, prob = overall.probability)

# Repeat the above, but this time for one draw in a week. In this case, the probability of that happening is 0.095, or 0.10.
# Also must be careful using lower.tail in this case. Lower.tail is a strict inequality, so when turning it to false,
# we must make sure our q value is also changed. We are looking for AT LEAST 1 draw, so q will be 0 when lower.tail is false
one.draw <- pbinom(q = 0, size = 201600, prob = overall.probability, lower.tail = FALSE)


### Question 5b ###
# In this question, we keep increasing our ticket count by 1 until our odds of winning once within a week is 80%
ticket.count = 0 # Establish initial ticket count
overall.probability.v2 <- ticket.count/20000000000 # Get overall probability with above ticket count
week.probability <- pbinom(q = 0, size = 201600, prob = overall.probability.v2, lower.tail = FALSE) # Calculate odds of winning once within a week
stopper <- 0 # Create variable for while loop to loop on

while(stopper < 1){                                    # While the looped-on variable is not 1
  if(week.probability < 0.80){                         # If the odds of winning once within the next week is less than 80%
    ticket.count <- ticket.count + 1                   # Add 1 to the ticket count
    overall.probability.v2 <- ticket.count/20000000000 # Re-do the odds of winning within a week calculation. Repeat until the odds are at least 80%
    week.probability <- pbinom(q = 0, size = 201600, prob = overall.probability.v2, lower.tail = FALSE)
  }
  else if(week.probability >= 0.80){ # If the odds reach 80%, stop the loop by changing the looped-on variable
    stopper <- 1
  }
}

ticket.count # You would need 159,666 tickets to increase your chances of winning within the next week to 80%. Rounded up to 160,000


### Question 5c ###
# First, we set prize money
# Second, we observe 365 days, by 10,512,000 tickets (the amount of tickets drawn per year), by our overall probability of success
# We average out how many times we win in a year, and multiply that by the prize money
# In this case if we had 10,000 tickets, we would expect to earn $1,300 dollars over the next year
prize <- 250
success.draws <- rbinom(365, 10512000, overall.probability)
average.draws <- mean(success.draws)
earnings <- average.draws * prize
earnings


### Question 5d ###
# Create plot first
# Create a list from 100 to 10,000,000 in increments of 100 to figure out expected earnings from
# Create an empty list to store these expected earnings in
ticket.amounts <- seq(from = 100, to = 10000000, by = 100)
earning.amounts <- list()

# Function that repeats question 5c, but on any list of values you pass it. It appends the expected earnings onto the empty list created earlier
overall.profit.finder <- function(ticket.list){
  overall.prob.seq <- ticket.list/20000000000
  successful.draw.seq <- rbinom(365, 10512000, overall.prob.seq)
  average.draw.seq <- mean(successful.draw.seq)
  earning.seq <- average.draw.seq * prize
  earning.amounts <- append(earning.amounts, earning.seq)
}

# Pass the ticket.amounts list into the function to find out expected earnings at required ticket levels
earning.plot <- lapply(ticket.amounts, overall.profit.finder)

# Load ggplot2 to create a plot with the amount of raffle tickets on the x axis, and the expected yearly earnings on the y axis
library("ggplot2")
qplot(x = ticket.amounts,
      y = unlist(earning.plot),
      xlab = "Amount of Raffle Tickets",
      ylab = "Average Yearly Reward Amount")

### Friend with 100,000 tickets:
# Repeat question 5c but with 100,000 tickets
# Friend with 100,000 tickets can expect to earn roughly $13,000 yearly
hundredk.prob <- 100000/20000000000
hundredk.draws <- rbinom(365, 10512000, hundredk.prob)
hundredk.avg <- mean(hundredk.draws)
hundredk.earnings <- hundredk.avg * prize
hundredk.earnings

### Friend with 1,000,000 tickets
# Repeat question 5c but with 1,000,000 tickets
# Friend with 1,000,000 tickets can expect to earn roughly $131,000 yearly
mill.prob <- 1000000/20000000000
mill.draws <- rbinom(365, 10512000, mill.prob)
mill.avg <- mean(mill.draws)
mill.earnings <- mill.avg * prize
mill.earnings
