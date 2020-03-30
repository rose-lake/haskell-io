# exercises

## adder :: simple IO 'game'
- The game asks the user how many numbers they wish to add
- It then prompts the user for each number to add
- And finally displays the sum

### scr/Adder_Integer
- function `adder_integer`
    - Refactored. Stores user input in an Integer. More robust.

### src/Adder_Int.hs
- function `adder_int`
    - First implementation. Some minimal validation. Edge cases will break. Stores user input in an Int.

### scr/Examples.hs ::
- function `getThreeReturnTwoChars`
    - book example (Hutton Ch.10). Takes in three characters, returns a tuple of the first and third characters.
- function `putStr'`
    - my own implementation of `putStr` using `sequence_`
