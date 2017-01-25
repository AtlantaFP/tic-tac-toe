# tic-tac-toe
A AtlantaFP playground for you to contribute your polyglot FP solutions to tic-tac-toe. Taken from [NashFP](https://github.com/NashFP)

```
SHALL WE PLAY A GAME?
```

Let's play [tic tac toe](https://en.wikipedia.org/wiki/Tic-tac-toe)!

## Rules of play
A board had 9 squares (3 x 3). There are two players "X" and "O". "X" always goes first. Each turn, a player puts their mark in an open square. If a player can get three of their mark ("X" or "O") in a row (vertically, horizontally, or diagonally) they win. 

We can represent the board with number 1 through 9.

```
  1 | 2 | 3 
 ---|---|---
  4 | 5 | 6 
 ---|---|---
  7 | 8 | 9
```

A game (in-progress or completed) can be represented as list of numbers for example:
```
# Empty board
[]

    |   |   
 ---|---|---
    |   |  
 ---|---|---
    |   | 

# One move
[3]

    |   | X 
 ---|---|---
    |   |  
 ---|---|---
    |   |   
  
 # Heated battle
[1,5,2,3,7,4,6]

  X | X | O  
 ---|---|---
  O | O | X 
 ---|---|---
  X |   |   
  
```

## Exercises

### Render board
Given a list of numbers draw a board (line above) 

### Managing moves 
Enforce rules: 
* Only accept moves between 1 through 9
* No duplicate moves 
* No moves after a win
* No more than 9 moves

### Computer vs human
Dr. Falken is going to go nuclear on you!

### FP GUI for tic-tac-toe (Elm, Fable, ...)
Pretty!

## Write your own
Contribute your solution by adding a folder name {your twitter handle}+{your language} such as "bryan_hunter+erlang".

Enjoy!
