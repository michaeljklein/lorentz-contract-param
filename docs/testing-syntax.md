# Testing semantic and syntax

## Initial setting

First of all, let's assume that in our extended language we have functions, like
```
func1 {
  PUSH int 3;
  MUL;
  ADD;
}

func2 {
  PUSH int 5;
  ADD;
  ADD;
}

code {
  ...
  %%call func1;
  %%call func2;
}
```

It's not necessary, but if we have this one we can test them using a syntax proposed below.

## Motivation
First thing I thought of when started: whether constructions of our framework should be either extension of the language or stick to the Michelson.
I suggest to try make it as much as possible in Michelson standard syntax, there are some points:
1. We can leave `.tz` extension and it will be truth
2. We can remain some testing code inside of a file and send it to the blockchain. 
   Readers of the code will be able to run this tests or at least see which tests a code passes.
One way to follow it: start our constructions with `#`, which indicates start of the comment in Michelson.

So, first of lets call _code unit_ either code section or manually defined function.

## #testable statement

First construction I'd like to introduce is indicator whether a code unit should be tested or not.
```
#testable ?name_of_code_unit
func1 {
  PUSH int 3;
  MUL;
  ADD;
}
```

`#testable` accepts optional argument: name of the code unit which is going to be tested (`code` for code section or name of function, like `func1`). If nothing is passed, then `#testable` relates to a code unit which is below.

## #test statement
The next one: let's make distinguish between unit test with predefined input (and expected output) and with generated one. So:
```
#testable ?name_of_code_unit
#test ?name_of_code_unit ?name_of_unit_test
#   input-stack {first_inp_value; second_inp_value; ...}
#   output-stack {first_out_value; second_out_value; ...}
code {
  ...
}
```
There may be several #test sections for one code unit.
* `#test` accepts optional name of a code unit or relates to the below one.
* It accepts optional name of the unit test, it should be string, for example `"Test sending/tests1"`.

The next two lines describes state of the stack before test execution and after. It should be lists in Michelson.
Pay you attention to if there is no `#testable` like and there are some `#test` sections, this unit code isn't going to be tested.

Having these two statements, we already can define tests and, most important, we can leave unit test cases in a smart contract file to simplify understanding, what script does and what expecting as input and output, even without running the smart contract.

## #assert statement
The next thing is assertions inside code units. From my point of vision, there are two points for consideration:
1. How to manage with assertions for several test cases
2. What to pass to assertion as a property

Regarding 2nd, there may be at least two options: either code in Michelson (so user will be able to check whatever property he wants) or some custom DSL for it (like, %%top for stack head, some operators, like ==, >, etc).
For now, I suggest passing Michelson code, to make asserts more flexible, custom useful constructions we can implement as macros (though, the syntax won't be so convenient as with custom DSL, though, it depends on what we call macro, it could be some operators too). So, let's consider that assert statement is some snippet of the Morley code, which has an access to the whole stack and leaves a boolean value on the stack, indicating whether an assert holds or not. Obviously, changes of the stack made by an assertion should discarded after execution of the assertion statement, and previous state of the stack should be restored.
Also, probably it makes sense to provide some meta variables to assertion snippet, like length of the stack, performed number of operations before assertion is called, etc.

Let's consider the syntax of assertion and then get back to the 1st point.
```
...
PUSH int 3;
#assert ?reg_expr_mathing_name_of_test_case assertion_message code_in_Morley
MUL;
...
```
* The first argument is a string, which describe test-cases (using their name), where this assertion has to be checked.
So you may have an assertion which should be checked in some group of tests (like, `"Test sending/*"`),
or for any execution of the program (then you can just omit the first argument).
If you have two completely different checks for two different unit tests, you have to put two assertions with corresponding names.
* The second argument is message, which should be printed if a test is failed. I suggest making it string with where the stack content before assertion may be accessed, like `"Invalid first two element of the stack %[0] %[1]"`, and perhaps with access to metavariables, like `%%stack_length, %%performed_operations`.
* The third argument is checking property itself: either code within curl brackets or just macro call inside parentheses (like, `STACK_TOP_EQ (Pair 5 3)`) or construction like `%%call function_defined_above_or_in_separate_file`. All these options are more or less Morley code.

So, regarding the second point of consideration, you can see, that multiply test cases can cause a lot of assertion statements in a one place, like:
```
...code here..
#assert "Unit-test1" "Sum of %[0] and %[1] is greater than 10" {PUSH 10; CMP; GE;}
#assert "Unit-test2" "Sum of %[0] and %[1] is greater than 1000" {PUSH 1000; CMP; GE;}
#assert "Unit-test3" "%[0] is greater than %[1]" {CMP; LE;}
...code here...
```
It can be kinda awkward, I admit that there can be more convenient construction, but I didn't come up with it.

Last thing which is left to cover: generated (property based) tests.
I am not going to provide any ideas, until we come up with the way of generation test cases.