#Features

 - author quizzes using markdown syntax
 - support for latex formula syntax
 - per answer feedback
 - share body between different questions
 - support for different question types
    - numerical (with specified tolerance)
    - mutliple choice questions

#Syntax
    ```
    ##Answer {.multichoice}
    correct
     :  lion
     :  elefant
    
    50
     :  bird
     
    wrong
     :  bee
    ```

 - numerical questions

    ```
    ##Answers {.numerical}
    true
     :  0.3 +- 0.01
     :  3e-1 +- 1e-2
     
    false
     :  -100
    ```

 - block-quotes after definition list items are feedback for questions 

    ```
    true
     :  lion
     
        > is a mamal.	
  	```
