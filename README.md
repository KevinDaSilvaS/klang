# klang
#### Klang is a strong typed, imperative, immutable programming language that transpiles to js. 
#### Klang stands for not only the pokemon but the group of two words ```K```(Initial of the creator of the language Kevin da Silva) and ```lang``` for the word language.
#### Inspired by languages like Haskell, Javascript, python and tiger. Klang is written in haskell and can be run on the browser by compiling into js.

## Features

- Immutability: all variables in klang should start with the '_'(_validVariable) symbol and cant change their state
- Only change of state occurs in runtime and only the loop iterator changes its state 
- Klang supports two kinds of types: Integers and Strings
- Klang is a strong typed language that uses from type inference to infer type into the expression
- Like in haskell ifs can only compare with same type like: Integer == Integer or String == String. Supported comparative operators(==, !=, >, <, >=, <=)
- Arithmetic expressions(+, -, *, /) are supported for integers
- Strings are double quoted: "valid string"
- Output is done by: show _myVariable
- Input is done like _readLine for string values and _readNum for integers
- Loops are made using the routine keyword
- Klang has no {} or () to create block you have to use ':' (to open) and ;(to close)
- 
### Valid Klang code
```
let _v_ := 132 
let _hi := "Hi " 

let _name     := _readLine
let _inputInt := _readNum + 3 + 5

show _inputInt * 2

routine 10 :
    show _hi
    show _name
    show _GET_CURR_INDEX

    let _VAR := 3
    show _VAR

    if _GET_CURR_INDEX > 5 :
        show "index is greater than 5"
    ;
;
```

### Generates the following output:
```
const _v_ = 132  
const _hi = "Hi "  
const _name =  prompt("Please enter a valid string")  

const _inputInt =  parseInt(prompt("Please enter a valid integer")) + 3 + 5  
console.log( _inputInt * 2 ) 
for (let _GET_CURR_INDEX = 0; _GET_CURR_INDEX < ( 10 ); _GET_CURR_INDEX++) {
    console.log( _hi ) 
    console.log( _name ) 
    console.log( _GET_CURR_INDEX ) 
    const _VAR = 3  
    console.log( _VAR ) 
    if (( _GET_CURR_INDEX ) > ( 5 )) { 
        console.log( "index is greater than 5" ) 
     
    }
 
}

```
### Getting Started

- First clone the project.
- build the project (klang compiler is done in haskell so i recommend using stack by just typing ```stack build```).
- create a file .klang 
- write the following on the file ```show "hello world"```
- copy the path
- on terminal run ``` stack run $YOUR_COPIED_FILEPATH```
- If successful a file of same name with the extension .js will appear on your klang project folder
- If an error occurs the compiler will give you an output with the possible reason of the error in red

## DOCS KLANG

### Variables
In klang all variables are constants and must start with the '_'(underscore) symbol, that means you cant change the value of a variable in your program.
Variables must be followed of: <let> <variable_name> <assignment_symbol> <value>
Valid variable Assignment: 
```
let _myVar := 123
let _VARIABLE     := "_VARIABLE IS A CONSTANT"
let _another_one  := _VARIABLE
let _Calculations := _myVar + 2 * 3
let _myVar := _VARIABLE //outputs an error constants can be declared only once in your program
```

### Ifs
In klang if statements can contain only one logical expression per if. Ifs in klang support the current comparative operators> >, <, >=, <=, !=, and ==.
If statements must be like: <if> <expression_or_value> <comparative_operator> <expression_or_value> <open_block> <your_code>  <close_block>
Valid if statements: 
```
let _myVar := 123
let _foo   := 45

if _myVar > _foo : 
    ...code here
;

if 200 < _foo : 
    ...code here
;

if "hi" == "HI" : 
    ...code here
;
```

### Loops
In klang loops receive an integer as iterator and the current iteration can be accessed by the ```_GET_CURR_INDEX``` reserved variable.
Loops in klang are like the following: <routine> <integer> <open_block> <your_code>  <close_block>
Valid loops: 
```
let _myIterator := 5

routine _myIterator : 
    ...code here
;

routine 10 : 
    ...code here
;

routine _myIterator * 2 : 
    show _GET_CURR_INDEX // _GET_CURR_INDEX RECEIVES THE CURRENT INDEX VALUE FROM ZERO TO MAX-1 LIKE THE i in a for loop
    ...code here
;

show _GET_CURR_INDEX // OUTSIDE ROUTINE _GET_CURR_INDEX VALUE IS 1, _GET_CURR_INDEX CHANGES ITSELF ONLY INSIDE THE LOOP OUTSIDE IS RESERVED AS THE VALUE 1

```
### Output
Output messages in klang can be done just by using the show keyword
```
    show 3 + 4 * 6
    show "Hello World"
```

### Input
Klang has an input detection for integers and strings. _readNum and _readLine especifically. Like the _GET_CURR_INDEX the _readNum is also an immutable variable that used out of the context o input in runtime has default value of 1 and _readLine has a default value of "" out of the input context.  
```
    show _readNum + 4 * 6
    let r := _readNum
    routine _readNum : 
        show _readLine
    ;
```
