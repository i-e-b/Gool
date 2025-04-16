Parsers: Composite:
===================

These parsers do no text comparison themselves.
They either combine pairs of other parsers or
modify the behaviour of a single sub-parser.

Most of these classes are quite simple.
If you create your own, remember to check
for self-recursion and prevent it, otherwise
your stack will fill up and crash your program.
