:- table expr_minus/3, term/3, multiply/3, division/3. 
%:- use_rendering(svgtree).
% program will parse the block ending with a [.].
% test the following predicate by running the following command 

% program([begin, const, x, =, 8, ;, var, y, ;, var, z, ;, z, :=, 0, ;, if, x, =, y, +, 2, then, begin, var, u, ;, z , := , 5,;, u, :=, 3, end, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile, end, .], []).

program(main(Block)) --> [main], block(Block).

% block is any block that starts with a [begin] and ends with an [end].
% a block has declarations and commands.
% test the following predicate using the following commands 

% block([begin, const, x, =, 8, ;, var, y, ;, var, z, ;, z, :=, 0, ;, if, x, =, y, +, 2, then, begin, var, u, ;, z , := , 5,;, u, :=, 3, end, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile, end], []).

block(block(Statements)) --> ['{'], statements(Statements), ['}'].

statements(statements(Declarations, Statements)) --> declarations(Declarations), statements(Statements).
statements(statements(Commands, Statements)) --> commands(Commands), statements(Statements).
statements(statements(Declarations)) --> declarations(Declarations).
statements(statements(Commands)) --> commands(Commands).

% a declarations is anything that either declares a [const] or a [var].
% running the command below would give the working of the predicate declarations/2.

% declarations([const, z, =, 2, ;, var, z, ;, var, y], []).

declarations(declarations(datatype(Datatype), identifier(Identifier), number(Number))) --> datatype(Datatype), variable(Identifier), [=], number(Number), [;].
declarations(declarations(datatype(Datatype), identifier(Identifier), value(Value))) --> datatype(Datatype), variable(Identifier), [=], value(Value), [;].
declarations(declarations(datatype(Datatype), identifier(Identifier))) --> datatype(Datatype), variable(Identifier), [;].

% commands will be assignment, an if condition or a while condition.
% a command can have a block within it, and will incorporate that to parse a statement
% Run the command below to test the command/2 predicate.

% commands([z, :=, 0, ;, if, x, =, y, +, 2, then, z , := , 5, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile], []).
% commands([z, :=, 0, ;, if, x, =, y, +, 2, then, begin, var, u, ;, z , := , 5,;, u, :=, 3, end, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile], []).

commands(=(Variable, Expressions)) --> variable(Variable), [=], expr(Expressions), [;].
commands(legacy_for_loop(declarations(Declaration), boolean(Boolean), expressions(Epression), commands(Commands))) --> [for], ['('], expr(Declaration), [;], boolean(Boolean), [;], expr(Epression), [')'], commands(Commands).
commands(if_else(boolean(Boolean), commands1(Commands1), commands2(Commands2))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands1),  [else],  commands(Commands2).
commands(if(boolean(Boolean), commands(Commands))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands).
commands(while(boolean(Boolean), commands(Commands))) --> [while], ['('], boolean(Boolean), [')'], commands(Commands).
commands(print_string(N)) --> [print], [<<], [N], [;], {string(N)}.
commands(print_number(N)) --> [print], [<<], [N], [;], {number(N)}.
commands(print_id(N)) --> [print], [<<], variable(N), [;].
commands(block(Block)) --> block(Block).


% boolean is a condition which checks whether a given statement is of type boolean
% to satisfy this, it should either be true, false, expression = expression, or not boolean.

% boolean([x, =, y, +, 2], []).
% boolean([not, x, =, z], []).

boolean(true(T)) --> [true], {T = true}.
boolean(false(F)) --> [false], {F = false}.
boolean(>(Expressions, Expressions2)) --> expr(Expressions), [>], expr(Expressions2).
boolean(<(Expressions, Expressions2)) --> expr(Expressions), [<], expr(Expressions2).
boolean(==(Expressions, Expressions2)) --> expr(Expressions), [==], expr(Expressions2).
boolean(not(Boolean)) --> [!], boolean(Boolean).

% expressions are arithmetic expressions 
% expressions can be [2, +, 3, +, 5], and this way it handles the precedence by introducing different levels 
% for different operators, we follow the PEMDAS rule and therefore express precedence following the said rule.
% expressions will also adhere to the associativity rule and take care of left -> right rule in arithmetic.
% test the following predicate by passing the following command(s)

% expressions([2, +, 3, *, 5], []).
% expressions([2, +, 3, *, 5], []).
% expressions([2, *, 3, *, 5, + , 3, *, 4, *, 5], []).

expr(expr_assgn(Datatype, Variable, Expressions)) --> datatype(Datatype), variable(Variable), [:=], expr(Expressions).
expr(Brackets) --> expr_minus(Brackets).
expr_minus(-(T1, T2)) --> expr_minus(T1), [-], term(T2).
expr_minus(T1) --> term(T1).
term(+(T1, T2)) --> term(T1), [+], multiply(T2).
term(N) --> multiply(N).
multiply(*(T1, T2)) --> multiply(T1), [*], division(T2).
multiply(N) --> division(N).
division('/'(T1, T2)) --> division(T1), [/], number(T2).
division(Assign) --> brackets(Assign).
brackets('(expr)'(N)) --> ['('], expr(N), [')'].
brackets(N) --> number(N).
brackets(Variable) --> variable(Variable).


% the variable predicate tracks all the different types of variable names allowed in the grammar.
% can test this using the following command.

% variable([x], []).
variable(variable(N)) --> [N], {atom(N)}.

% a number is the highest priority in the arithmetic operations, and any expressions within brackets
% will be considered as an individual number and hence you can see that it is taken into account by
% passing the expressions within quotes. Test the following predicate by using the following commands

% number(['(', 3, +, 2, +, 1, ')'], []).
% number([3], []).

number(N) --> [N], { number(N) }.
value(string(String)) --> [String], {string(String)}.

datatype(float) --> [float].
datatype(int) --> [int].
datatype(string) --> [string].
datatype(var) --> [var].
datatype(const) --> [const].



% ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



% program_eval/4 takes the Initial Tree, with command line arguments.
% After evaluating the block, it produces the Final_Environment

% program(P,[begin ,var ,x,;, var, y,;, var ,z,; ,z,:=,'(', z, :=, x, +, 2,')',+ , y , end , .],[]), program_eval(P,2,3,Z).
% program(P,[begin, var, z, ; , var, x, ;, z, :=, x, end, .],[]), program_eval(P,2,3,Z).
% program(P,[begin ,var, x,; ,var ,y,;, var ,z,; ,z,:=,x,+,y ,end,.],[]), program_eval(P,2,3,Z).
% program(P,[begin ,var ,x,;, var, y,; ,var ,z,; ,z,:=,'(',z,:=,x,+,2,')',+,y ,end,.],[]), program_eval(P,2,3,Z).
% program(P,[begin ,var, x,; ,var, y,; ,var ,z,; ,if ,x,=,y ,then, z,:=,1, else ,z,:=,0 ,endif ,end,.],[]), program_eval(P,2,3,Z).
% program(P,[begin, var, x,;, var, y,; ,var, z,;, if, x, =, 0, then, z,:=,x, else, z,:=,y, endif ,end,.],[]), program_eval(P,2,3,Z).
% program(P,[begin ,var, x,;, var, y,;, var, z,;, if, not,x,=,y, then, z,:=,x, else, z,:=,y, endif ,end,.],[]), program_eval(P,2,3,Z).
% program(P,[begin ,var ,x,; ,var ,z,; ,z,:=,0,;, while ,not ,x,=,0, do ,z ,:= ,z,+,1,; ,x,:=,x,-,1 ,endwhile, end,.],[]), program_eval(P,2,3,Z).

program_eval(main(Block), ArgX, ArgY, FE) :- eval_block(Block, [(x, ArgX), (y, ArgY)], FE).

% eval_block/3 will evaluate whatever it gets within a block, Declarations and Commands, and produce the final Environment
% eval_block(block(declarations(variable(var(x)), variable(var(z))), commands(var(z):=0, while(not(var(x)=0), do(commands(var(z):=var(z)+1, var(x):=var(x)-1))))), [(x,2),(y,3)], FV).

eval_block(block(Statements), Environment, Final_Env) :- eval_statements(Statements, Environment, Final_Env).

/* 

statements(statements(Declarations, Statements)) 
statements(statements(Commands, Statements)) 
statements(statements(Declarations)) 
statements(statements(Commands))

*/

eval_statements(statements(Declarations, Statements), Environment, NewEnvironment) :- eval_declarations(Declarations, Environment, MediatorEnvironment), eval_statements(Statements, MediatorEnvironment, NewEnvironment).
eval_statements(statements(Commands, Statements), Environment, NewEnvironment) :- eval_commands(Commands, Environment, MediatorEnvironment), eval_statements(Statements, MediatorEnvironment, NewEnvironment).
eval_statements(statements(Declarations), Environment, NewEnvironment) :- eval_declarations(Declarations, Environment, NewEnvironment).
eval_statements(statements(Commands), Environment, NewEnvironment) :- eval_commands(Commands, Environment, NewEnvironment).

% eval_declarations/3 will handle the different types of declarations, delarations update the environment
% eval_declarations(declarations(variable(var(x)), variable(var(z))), [(x,2),(y,3)], FV).

/* 

declarations(declarations(datatype(Datatype), identifier(Identifier), number(Number)))
declarations(declarations(datatype(Datatype), identifier(Identifier), value(Value)))
declarations(declarations(datatype(Datatype), identifier(Identifier)))

*/


eval_declarations(declarations(datatype(_Datatype), identifier(variable(Identifier)), number(Value)), Environment, New_Environment) :- update(Identifier, Value, Environment, New_Environment).
eval_declarations(declarations(datatype(_Datatype), identifier(variable(Identifier)), value(string(Value))), Environment, New_Environment) :- update(Identifier, Value, Environment, New_Environment).
eval_declarations(declarations(datatype(_Datatype), identifier(variable(Identifier))), Environment, New_Environment) :- (declaration_initial_lookup(Identifier, Environment, Result) -> update(Identifier, Result, Environment, New_Environment) ; update(Identifier, _, Environment, New_Environment)).

% eval_commands/3 will take input commands and will evaluate them conforming to the current environment
% eval_commands will have a side affect on the environment to produce a new environment.

% eval_commands(commands(var(z):=0, while(not(var(x)=0), do(commands(var(z):=var(z)+1, var(x):=var(x)-1)))), [(x,2),(y,3)], FV).

/* 

commands(=(Variable, Expressions)) --> variable(Variable), [=], expr(Expressions), [;].
commands(legacy_for_loop(declarations(Declaration), boolean(Boolean), expressions(Epression), commands(Commands))) --> [for], ['('], expr(Declaration), [;], boolean(Boolean), [;], expr(Epression), [')'], commands(Commands).
commands(if_else(boolean(Boolean), commands1(Commands1), commands2(Commands2))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands1),  [else],  commands(Commands2).
commands(if(boolean(Boolean), commands(Commands))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands).
commands(while(boolean(Boolean), commands(Commands))) --> [while], ['('], boolean(Boolean), [')'], commands(Commands).
commands(print_string(N)) --> [print], [<<], [N], [;], {string(N)}.
commands(print_number(N)) --> [print], [<<], [N], [;], {number(N)}.
commands(print_id(N)) --> [print], [<<], variable(N), [;].
commands(block(Block)) --> block(Block).

*/


eval_commands(block(Block), Environment, New_Environment) :- eval_block(Block, Environment, New_Environment).
eval_commands(while(boolean(Boolean), commands(Commands)), Environment, New_Environment) :- eval_bool(Boolean, Environment, MediatorEnvironment, true), eval_commands(Commands, MediatorEnvironment, MediatorEnvironment1), eval_commands(while(boolean(Boolean), commands(Commands)), MediatorEnvironment1, New_Environment).
eval_commands(while(boolean(Boolean), commands(_Commands)), Environment, New_Environment) :- eval_bool(Boolean, Environment, New_Environment, false).
eval_commands(if_else(boolean(Boolean), commands1(Commands1), commands2(_Commands2)), Environment, NewEnvironment) :- eval_bool(Boolean, Environment, MediatorEnvironment, true), eval_commands(Commands1, MediatorEnvironment,NewEnvironment).
eval_commands(if_else(boolean(Boolean), commands1(_Commands1), commands2(Commands2)), Environment, NewEnvironment) :- eval_bool(Boolean, Environment, MediatorEnvironment, false), eval_commands(Commands2, MediatorEnvironment,NewEnvironment).
eval_commands(if(boolean(Boolean), commands(Commands)), Environment, NewEnvironment) :- eval_bool(Boolean, Environment, MediatorEnvironment, true), eval_commands(Commands, MediatorEnvironment,NewEnvironment).
eval_commands(if(boolean(Boolean), commands(_Commands)), Environment, NewEnvironment) :- eval_bool(Boolean, Environment, NewEnvironment, false).
eval_commands(=(variable(Variable), Expressions), Environment, New_Environment) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), update(Variable, Result, MediatorEnvironment, New_Environment).
eval_commands(print_string(N), Environment, Environment) :- write(N).
eval_commands(print_number(N), Environment, Environment) :- write(N).
eval_commands(print_id(variable(N)), Environment, Environment) :- lookup(N, Environment, Result), write(Result).

% eval_bool/4 will take into account an Environment and will evaluate an expression, which might have a
% side affect on the environment, and hence will return a new environment as well. It will return true or false depending on
% whether the expressions resolved conform to the rules of a boolean expression

% eval_bool(not(var(x)=0), [(x, 2), (y,3)], NV, Boolean).
% eval_bool(not(var(x)=var(y)), [(x, 2), (y,3)], NV, Boolean).


/* 

boolean(true(T)) --> [true], {T = true}.
boolean(false(F)) --> [false], {F = false}.
boolean(>(Expressions, Expressions2)) --> expr(Expressions), [>], expr(Expressions2).
boolean(<(Expressions, Expressions2)) --> expr(Expressions), [<], expr(Expressions2).
boolean(==(Expressions, Expressions2)) --> expr(Expressions), [==], expr(Expressions2).
boolean(not(Boolean)) --> [!], boolean(Boolean).

*/


eval_bool(true(true), Environment, Environment, true).
eval_bool(false(false), Environment, Environment, false).
eval_bool(==(Expressions, Expressions2), Environment, New_Environment, true) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result = Result2.
eval_bool(==(Expressions, Expressions2), Environment, New_Environment, false) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result =\= Result2.
eval_bool(>(Expressions, Expressions2), Environment, New_Environment, true) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result > Result2.
eval_bool(>(Expressions, Expressions2), Environment, New_Environment, false) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result < Result2.
eval_bool(<(Expressions, Expressions2), Environment, New_Environment, true) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result < Result2.
eval_bool(<(Expressions, Expressions2), Environment, New_Environment, false) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result > Result2.
eval_bool(>=(Expressions, Expressions2), Environment, New_Environment, true) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result >= Result2.
eval_bool(>=(Expressions, Expressions2), Environment, New_Environment, false) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result < Result2.
eval_bool(<=(Expressions, Expressions2), Environment, New_Environment, true) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result =< Result2.
eval_bool(<=(Expressions, Expressions2), Environment, New_Environment, false) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), eval_expr(Expressions2, MediatorEnvironment, Result2, New_Environment), Result > Result2.
eval_bool(not(Boolean), Environment, New_Environment, true) :- eval_bool(Boolean, Environment, New_Environment, false).
eval_bool(not(Boolean), Environment, New_Environment, false) :- eval_bool(Boolean, Environment, New_Environment, true).

% eval_expr will take the parse tree, Tree, and then using the environment passed with the predicate,
% determine the result for the expressions embedded in the parse tree.

% expr(T, [x, +, y, +, z, *, z, *, x, *, y], []), eval_expr(T, [(x,1),(y,4),(z,2)], R).
% expr(T, ['(', x, +, '(',y, +, '(', z, *, z, ')', *, x, ')', *, y, ')'], []), eval_expr(T, [(x,1),(y,4),(z,2)], R).
% eval_bool(var(x)=var(y), [(x, 2), (y,3)], NV, Boolean).

eval_expr(+(Tree1, Tree2), Environment, Result, New_Environment) :- eval_expr(Tree1, Environment, ResultofTree1, Med_Env), eval_expr(Tree2, Med_Env, ResultofTree2, New_Environment), Result is ResultofTree1 + ResultofTree2.
eval_expr(-(Tree1, Tree2), Environment, Result, New_Environment) :- eval_expr(Tree1, Environment, ResultofTree1, Med_Env), eval_expr(Tree2, Med_Env, ResultofTree2, New_Environment), Result is ResultofTree1 - ResultofTree2.
eval_expr(*(Tree1, Tree2), Environment, Result, New_Environment) :- eval_expr(Tree1, Environment, ResultofTree1, Med_Env), eval_expr(Tree2, Med_Env, ResultofTree2, New_Environment), Result is ResultofTree1 * ResultofTree2.
eval_expr('/'(Tree1, Tree2), Environment, Result, New_Environment) :- eval_expr(Tree1, Environment, ResultofTree1, Med_Env), eval_expr(Tree2, Med_Env, ResultofTree2, New_Environment), Result is ResultofTree1 / ResultofTree2.
eval_expr(Number, Environment, Number, Environment) :- number(Number).
eval_expr('(expr)'(Expressions), Environment, Result, New_Environment) :- eval_expr(Expressions, Environment, Result, New_Environment).
eval_expr(expr_assgn(variable(Variable), Expressions), Environment, Result, New_Environment) :- eval_expr(Expressions, Environment, Result, MediatorEnvironment), update(Variable, Result, MediatorEnvironment, New_Environment).
eval_expr(variable(Variable), Environment, Number, Environment) :- lookup(Variable, Environment, Number).

% declaration_initial_lookup searches for the value of an identifier within an environment
% initially to give a value to the command line arguments which may be redeclared within the code
% Once found, it will return the value, else will throw an error.

% declaration_initial_lookup(x, [(x,2)], Value).
% declaration_initial_lookup(x, [(y,2)], Value).

declaration_initial_lookup(Identifier, [(Identifier, Value)|_], Value).
declaration_initial_lookup(Identifier, [(Head, _)|RestofEnvironment], Value) :- Identifier \= Head, declaration_initial_lookup(Identifier, RestofEnvironment, Value).
declaration_initial_lookup(_, [], _).

% lookup searches for the value of an identifier within an environment,
% Once found, it will return the value, else will throw an error.

% lookup(x, [(x,2)], Value).
% lookup(x, [(y,2)], Value).

lookup(Identifier, [(Identifier, Value)|_], Value).
lookup(Identifier, [(Head, _)|RestofEnvironment], Value) :- Identifier \= Head, lookup(Identifier, RestofEnvironment, Value).
lookup(Identifier, [], _):- write(Identifier), write(' does not exist'), nl, fail.

% update will update the value of an Identifier if it exists,
% Else will add the said Identifier within the environment.

% update(x, 2, [(x, 300), (y,3), (z,2)], New), update(y,6,New,Newer), update(z,7,Newer, Newest).

update(Identifier, Value, [(Identifier, _)|RestofEnvironment], [(Identifier, Value)|RestofEnvironment]).
update(Identifier, Value, [HeadofEnv|RestofEnvironment], [HeadofEnv|NewEnvironment]) :- HeadofEnv = (Head, _), Head \= Identifier, update(Identifier, Value, RestofEnvironment, NewEnvironment).
update(Identifier, Value, [], [(Identifier, Value)]).

