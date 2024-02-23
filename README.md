# racket-parser

A partial parser written in Racket for a simple programming language. See my bigger and better [version in C](https://github.com/gschick3/parser-c) as well (it actually creates a parse tree).

## Language Grammar

```
program -> linelist $$ 
linelist -> line linelist | epsilon 
line ->  label stmt linetail 
label -> id: | epsilon 
linetail -> ;stmt+ | epsilon 
stmt -> id = expr 
	| if (boolean) stmt 
	| while (boolean) linelist endwhile
	| read id
	| write expr
	| goto id
	| gosub id
	| return
	| break
	| end
boolean -> true | false | expr bool-op expr 
bool-op -> < | > | >= | <= | <> | =
expr -> id etail | num etail | (expr) 
etail -> + expr | - expr | * expr | / expr | epsilon
id -> [a-zA-Z][a-zA-Z0-9]*
num -> numsign digit digit*
numsign -> + | - | epsilon 
digit -> [0-9]
```

## Sources

_ChatGPT 3.5_
* "How do I write a function in racket that takes a variable number of arguments?"
  * "If I want to pass a list to this function, how would i separate it?"
* "What does this regex do? `((?<!^)\b(?!$)|(?=[\\(\\)])|(?<=[\\(\\)]))`" (This was regex that I made myself)
  * "Can you improve it?" (this yielded a shorter regex string)

_regex101.com_
* Used for testing regex statements

_docs.racket-lang.org_

_stackoverflow_
* Various specific questions about racket regex, etc.
