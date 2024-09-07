#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title Select a door
#' @description `select_door` Randomly chooses a door between 1-3.
#' @details Randomly selecting a door is similar to Lets make a deal when the contestant chooses a door of their liking.
#' @param ... no arguments are used by the function.
#' @return The function returns the picked door.
#' @examples select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Open a GOAT door
#' @description `open_goat_door` randomly chooses a door that contains a goat, but is also not your selected door.
#' @details This is similar to Lets Make a Deal when after the contestant chooses their selected door the host then opens a non winning door.
#' @param `game` - A vector of three containing two insistence of "goat" and one instance of "car".
#'        `a.pick` - A number representing the contestants pick.
#' @return A number representing the door the host picked.
#' @examples open_goat_door(new.game, my.initial.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Change door or Stay
#' @description `change_door` will give you the choice to either change your pick or stay with you initial pick.
#' @details In the classic game show, Lets Make a Deal, after the host opens a non winning door, they then give you the option to change or stay with your original door.
#' @param `stay` - A Boolean representing whether the contestant switches their initial pick. The default value is "T"
#'        `opened.door` - A number representing the hosts door. 
#'        `a.pick` - A number representing the contestants initial pick.
#' @return A number representing the contestants final pick.
#' @examples change_door(stay=T, host.door, my.initial.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Determine a winner
#' @description `determine_winner` will show if the contestant will "WIN" or "LOSE"
#' @details In any game show the contestant will either "WIN" or "LOSE". If the final pick contains a "goat" the contestant will Lose, if it is a "car" the contestant is a winner.
#' @param `final.pick` - A number representing the contestants final pick.
#'        `game` - A vector of strings representing the order of goats and cars.
#' @return Returns a string of either "Win" or "lose".
#' @examples determine_winner(my.final.pick, new.game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Play a new game
#' @description Running all functions to simulate a new game.
#' @details This code is combining all functions in order to simulate a new game.
#' @param ... no arguments are used by the function.
#' @return A data frame detailing the results.
#' @examples play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Simulate the game 100
#' @description A loop of the game will run 100 times.
#' @details This code is telling the game to run n times in a row with different results and to stop after a number of times.
#' @param 'n' - A number representing the number of times to run the game. The default value is 100.
#' @return This returns the amount of loses and wins out of n times in a table in percent form.
#' @examples play_n_games(100)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
