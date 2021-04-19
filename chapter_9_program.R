# Game-day Simulator for Baseball (R)
library(lattice)  # graphics package for probability matrix visual #loading and attaching an add-on package 
#defining the function simulator
simulator <- function(home_mean,away_mean,niterations) { 
     # input runs scored means, output probability of winning for home team
     set.seed(1234)  # set to obtain reproducible results 
     away_game_score <- numeric(niterations) #away runs-scored distribution of the away team for each game (iteration)
     home.game.score <- numeric(niterations) #home runs-scored distribution of the home team for each game (iteration)
     home_win <- numeric(niterations) #probability of winning for home team
     i <- 1 #i is initially initialized to 1
     #creating a while loop that executes the same code again and again until i takes a value greater than the number of games (iterations)
     while (i < niterations + 1) { 
         #using a negative binomial distribution with k set to 4
         away_game_score[i] <- rnbinom(1,mu=away_mean, size = 4) #drawing from the away runs-scored distribution of the away team 
         home.game.score[i] <- rnbinom(1,mu=home_mean, size = 4) #drawing from the home runs-scored distribution of the home team 
         #if the away team score is tied with the home team score the observation is discared, if the away team score is higher than the home team score, the away team wins 
         #if the home team score is higher than the away team score, the home team wins 
         if(away_game_score[i] > home.game.score[i]) home_win[i] <- 1 
         if(away_game_score[i] > home.game.score[i] || 
         away_game_score[i] < home.game.score[i]) i <- i + 1 
         }
     n_home_win <- sum(home_win) #adding the number of times the home team wins 
     n_home_win/niterations  # return probability of home team winning by diving the number of home team wins by the number of games (iterations)
     } 

#the number of games is 100,000
niterations <- 100000  # use smaller number for testing
# probability matrix for results... home team is rows, away team is columns
probmat <- matrix(data = NA, nrow = 9, ncol = 9,  #creating a matrix called probmat, number of rows = 9, number of columns = 9 
  dimnames = list(c(as.character(1:9)), c(as.character(1:9)))) #setting the rows and column names as numbers 1 through 9 
for (index_home in 1:9) #creating a for-loop that iterates through the numbers 1 through 9 for the home team 
for (index_away in 1:9) #creating a for-loop that iterates through the numbers 1 through 9 for the away team 
#checking if each element of the index_home vector is unequal to the corresponding element of the index_away vector 
if (index_home != index_away) {
     probmat[index_home,index_away] <- 
        simulator(index_home, index_away, niterations) #probabaility matrix for the results come from a probability simulator for home and away teams 
     }
#starting the graphics device driver for producing a PDF graphic using the file "fig_sports_analytics_prob_matrix.pdf"
#the width of the graphic is 8.5 and the height of the graphic is 8.5
pdf(file = "fig_sports_analytics_prob_matrix.pdf", width = 8.5, height = 8.5)
x <- rep(1:nrow(probmat),times=ncol(probmat)) #replicating the elements of the vector, 1 through the number of rows and the number of columns in the probability matrix 
y <- NULL #y is the null object 
#creating a for-loop so that each run of the loop will assign i to a different value in the column and rows 
for (i in 1:ncol(probmat)) y <- c(y,rep(i,times=nrow(probmat)))
#sprintf with 3 decimal places, returning a numeric value for the probability matrix, assigned as probtext
probtext <- sprintf("%0.3f", as.numeric(probmat))  # fixed format 0.XXX
text_data_frame <- data.frame(x, y, probtext) #creating a data frame with home team runs expected (rows), away team runs expected (columns), and probability of home team winning 
text_data_frame$probtext <- as.character(text_data_frame$probtext)
text_data_frame$probtext <- ifelse((text_data_frame$probtext == "NA"),
    NA,text_data_frame$probtext)  # define diagonal cells as missing
text_data_frame <- na.omit(text_data_frame)  # diagonal cells
#drawing a level plot for the probabilites 
#number of levels the range of probabilities will be divided into = 25
#number of tick marks = 9
print(levelplot(probmat, cuts = 25, tick.number = 9,
    col.regions=colorRampPalette(c("violet", "white", "light blue")), #creating a vector of colors 
    xlab = "Visiting Team Runs Expected", #the x-axis label 
    ylab = "Home Team Runs Expected", #the y-axis label
        panel = function(...) {
        panel.levelplot(...)  
        #creating panel text, labels = home team runs expected and visting teams runs expected 
        panel.text(text_data_frame$x, text_data_frame$y, 
        labels = text_data_frame$probtext) 
        }))
dev.off()  #closing the file "fig_sports_analytics_prob_matrix.pdf"       
# Suggestion for the student: Develop simulators for football or basketball.    

