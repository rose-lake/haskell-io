# exercises

## adder :: simple IO 'game'
- The game asks the user how many numbers they wish to add
- It then prompts the user for each number to add
- And finally displays the sum

- `adder_integer`
    - More robust validation and data types. Stores user input in an Integer.
    - src/Adder_Integer.hs

- `adder_int`
    - First implementation. Some minimal validation. Edge cases will break. Stores user input in an Int.
    - src/Adder_Int.hs

## other examples

- `getThreeReturnTwoChars`
    - book example (Hutton Ch.10). Takes in three characters, returns a tuple of the first and third characters.
    - src/Examples.hs

- `putStr'`
    - my own implementation of `putStr` using `sequence_`
    - src/Examples.hs
