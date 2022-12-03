import Input

shape Rock = 1
shape Paper = 2
shape Scissors = 3

outcome Loss = 0
outcome Draw = 3
outcome Win = 6

my_move Rock Loss = Scissors
my_move Rock Draw = Rock
my_move Rock Win = Paper
my_move Paper Loss = Rock
my_move Paper Draw = Paper
my_move Paper Win = Scissors
my_move Scissors Loss = Paper
my_move Scissors Draw = Scissors
my_move Scissors Win = Rock

score (opponent, result) =
  shape (my_move opponent result) + outcome result

main = do
  plan <- input "input.txt"
  let Right turns = plan
  print $ sum (map score turns)
