Implementation:
-> Firstly I have implemented type checking (which is essential for the execution) by introducing a new datatype AST in the ast.sml file and accordingly changing the grammar (and semantic) rules. 

-> I have created a single datatype AST so that it is easier to handle the stacks of the vmc machine.

-> In the stack.sml file, I have implemented the general purpose functional stack structure (Funstack) .

-> In the memory.sml file, I have first calculated the no of variable declarations and accordingly defined the memory array.
    -> For this, I have first imported the ast generated and then by pattern matching, I have calculated the size of the list of list of variable declarations.
    -> For assigning and updating the memory, I have used a HashTable. 
    -> Also I have defined a function lofltol which converts a list of list to a list.
    -> Another function iter iterates over every element in a given list, and for every declaration it updates the memory array makin use of the hashtable.
    -> The function defhtable pattern matches the given ast, breaks the list of list of variable declarations to a list of variable declarations and then uses the iter function to update the memory array.

-> In the postfix.sml file, I have generated the postfix stack for a given ast.
    -> This is accomplished by first importing the ast, and then calling the convpostfix function to convert it.
    -> This starts by the postfixgen function pattern matching the given ast, extracting the list of list denoting command sequence.
    -> This is followed by the Comm, Comm* (* denoting plus, mult, minus, percent, divi) functions operating over each command, and pushing to the postfixstack whenever we reach an identifer (the postfixstack is passed as a parameter to enable this).
    -> This results in the postfix stack.

-> In the vmc.sml file, I have first imported the ast, defined the vmc structure to implement the vmc machine.


Test Program :
program good2::
    var A, B, C, D: int;
    var E, F, G: int;
{
    A := ((A * B) + ((C / D) % A));
    B := (A - B);
}

Corresponding AST :

program
    ("good2",
     ([(["A","B","C","D"],"int"),(["E","F","G"],"int")],
      [assign
         ("A",
          plus
            (mult (identifi "A",identifi "B"),
             percent (divi (identifi "C",identifi "D"),identifi "A")))])) 


Corresponding Postfix stack:

[valu "assign",valu "B",valu "minus",valu "B",valu "A",valu "assign",
   valu "A",valu "plus",valu "percent",valu "A",valu "div",valu "D",valu "C",
   valu "mult",valu "B",valu "A"] 

   Note: My postfix stack is actually generated in a reverse fashion (with respect to the usual postfix stack), but that doesn't affect the evaluation.

Running Instructions :
1) Type make in the terminal
2) Type sml in the terminl to enter the sml interactive mode.
3) Type use "ast.sml" ;
4) Type use "stack.sml" ;
5) Type use "memory.sml" ;
6) Type use "postfix.sml" ;
7) Type use "vmc.sml" ;


Acknowledgements:
    https://smlfamily.github.io/Basis/list.html#SIG:LIST.foldr:VAL  -> for referring to list datatype in sml

    User’s Guide to ML-Lex and ML-Yacc , Roger Price -> for referring to yacc, lex, .cm specifications

    https://www.cs.cornell.edu/courses/cs312/2005sp/lectures/rec07.html     -> for reference to some stack functions

    stackoverflow -> for understanding and rectifying errors