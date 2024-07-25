Parsers: Transforms:
====================

These parsers wrap another parser and transform
it's output in the event of a successful match.

They don't match any part of the input by themselves,
and may produce output that is substantially different
from the input (for example ExpressionParser, which
transforms infix syntaxes to postfix syntaxes).