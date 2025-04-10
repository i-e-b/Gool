Gool is a lexer/parser for C#
==================================

A fast, robust, and thread-safe parser-combinator library for C#, with a fluent BNF-like interface for building parsers.

Use this to read and interpret a wide range of text-based input -- including file formats, data structures, and 
programming languages.

By being a run-time library inside the main program, grammars can be built and modified as required,
with even complex structures taking microseconds to build.

### Parser-Combinators?

Parser-Combinators are components that you can build into structures that encode languages: "Grammars".
The building can be done using a human-readable syntax, building parsers of increasing complexity on top of simpler parts.
Detailed grammars and languages can be processed in an efficient way.

By structuring the parser-combinator library in a particular way, building parsers is the same as writing a grammar itself.
Therefore instead of describing how to parse a language, a user must only specify the language itself.
The result is a working parser.

### Unique features

- Contextual parsing: can build new patterns at run-time *and* at parse time
- Patterns can be computed using any C# code
- Easily expanded to handle complex patterns
- Use all your existing navigation and refactoring tools

### When should I use this?

If you have a complex and/or fragile set of regular expressions, try using a parser instead.

See [Sample Parsers](https://github.com/i-e-b/Gool/tree/master/SamplesStd) for fully functional examples.

Basic example
-------------

Defining the parser: 

```csharp
BNF // Basic infix arithmetic expressions
    number     = FractionalDecimal(),    // Built-in helper for signed numbers

    factor     = number |  ('(' > _expression > ')'), // Number or parenthesised expression
    power      = factor > !('^' > factor),            // Factor, with optional '^' + exponent
    term       = power  %  ('*' | '/'),               // Powers, optionally joined with '*' or '/'
    expression = term   %  ('+' | '-');               // Terms, optionally joined will '+' or '-'
```

Reading an input:

```csharp
var result = expression.ParseEntireString( // Run the parser, refuse partial matches
    "(6.5 + 3) * (5.5 - -0.2e1)"
    );

var tree = TreeNode.FromParserMatch(result, true);        // Interpret the raw parse tree
var final = TreeNode.TransformTree(tree, ApplyOperation); // Apply functions to reduce tree to value

Console.WriteLine(final); // 71.25
```

(some details removed for clarity -- see bottom of this readme for full implementation)

BNF Syntax
----------

### Terminal parsers:

- `'…'` → *Character* parser that matches a single literal character in the input
- `"…"` → *String* parser that matches a literal string in the input
- `BNF.Regex("…")` → *Regex* parser that matches a string based on a regex pattern.
- `BNF.OneOf(…)` → Match a single character from the set provided
- `BNF.NoneOf(…)` → Match any single character that is **not** in the set provided
- `BNF.AnyChar` → Parser that matches any single character.
- `BNF.Empty` → Parser that matches an empty string (useful in unions)
- `BNF.EndOfInput` → Parser that matches the end of input (parsers will normally accept partial matches)
- `BNF.LineEnd` → Parser that matches a line end (either `\r`, or `\n`, or `\r\n`)
- `BNF.WhiteSpace` → Parser that matches a single character of white-space

### Combining parsers:

- a `|` b → Create a *union* parser that matches the **longest** result from either **a** or **b**. Parser will match if only one of **a** and **b** match, *or* if both **a** and **b** match.
    - Example: `"hello" | "world"` matches `hello` or `world` 
    - Example: `"on" | "one"` matches `on` and `one`. `+( "on" | "one" )` will match `oneone` as {`one`, `one`}
- a `>` b → Create a *sequence* parser that matches **a** then **b**
    - Example: `'x' > 'y'` matches `xy` but not `x` or `y`
- a `<` b → Create a *terminated list* parser that matches a list of **a**, each being terminated by **b**. The last item **a** must be terminated.
   - Example: `'x' < ';'` matches `x;x;x;` and `x;`, but not `x` or `x;x`
- a `%` b → Create a *delimited list* parser that matches a list of **a**, delimited by **b**. A trailing delimiter is not matched.
    - Example: `'x'%','` matches `x` and `x,x`, but not `x,x,`
- `-`a → Create an *optional repeat* parser that matches zero or more **a**
   - Example: `-"xy"` matches `xyxy`, `xy`, and *empty*
- `+`a → Create a *repeat* parser that matches one or more **a**
   - Example: `+"xy"` matches `xy` and `xyxy`, but not *empty*
- `!`a → Create an *option* parser that matches zero or one **a**
   - Example: `!"xy"` matches `xy` and *empty*, but not `xyxy`
   - Can also be expressed `BNF.Optional(`a`)`
- `~`a → Create a *non-consuming* parser that must match **a**, but does not consume the match
  - Example: `'x' > ~'y' > "yz"` matches `xyz` as `x` and `yz`
- a `&` b → Create an *intersection* parser that matches (**a** then **b**) or (**b** then **a**)
   - Example: `'x'&'y'` matches `xy` and `yx`, but not `xx` or `yy` 
- a `^` b → Create an *exclusion* parser that matches **a** or **b** but not both
    - Example: `'x'^'y'` matches `x` and `y`, but not `xy` or `yx`
- a `/` b → Create a *difference* parser that matches **a** but not **b**
    - Example: `"on" / "one"` matches `on` but not `one`. `+( 'x' / '.' )` will match `xx.` as {`x`, `x`}

Parsers generated by BNF can be used repeatedly.

Scanners
--------

Parsers operate over a 'scanner', which is an input string plus transforms and contextual data.
For common cases, you won't need to create one directly -- just use `BNF.ParseString` or `BnfPackage.ParseString`.

Scanners handle case-conversion and white-space skipping if you use those options.

Because scanners hold context for a parse, they cannot be reused or shared between parse attempts.

Tags, scopes, and trees
-----------------------

The basic output from a parser is a `ParserMatch`, which gives a complete tree of all matches, including those from combined
parsers. `ParserMatch` also gives access to the parser that made the match, and the scanner that was used for input.

The `ParserMatch` tree contains all the information from a result, but often this is too much.

Any parser can be tagged with a string value, and this can be used to extract salient information from the tree.

### TaggedTokensDepthFirst / TaggedTokensBreadthFirst

You can request a sequence of `ParserMatch`es from the result tree, only returning tagged results.
Tags are **not** inherited by parent matches.

### ScopeNode.FromMatch

The scoped node tree ignores the `ParserMatch` hierarchy, and uses `.OpenScope()` and `.CloseScope()`
to build a different structure. This is useful for structured data (like JSON and XML) and otherwise scoped
data that use open and close markers -- like (`{`,`}`) or (`begin`,`end`) in many programming languages.

### TreeNode.FromParserMatch

General tree nodes match the `ParserMatch` hierarchy, but only including nodes with a tag or scope set.

The `Pivot` scope has a specific effect on general trees, 'lifting' them to a level above non-pivot peers.
This is useful for chains of operators:

Given the parser `sum` from:

```csharp
BNF number = BNF.Regex("[0-9]+").Tag("num");
BNF addSub = BNF.OneOf('+', '-').Tag("op");
BNF sum = number % addSub;
```

and the input:

```csharp
var result = sum.ParseEntireString(
                             "1+2",
                             );
var tree = TreeNode.FromParserMatch(result, false);
```

outputs `tree` as:
```
┌───── 1   
│      +   
└──  2     
```

but changing `addSub` to `BNF.OneOf('+', '-').Tag("op").PivotScope();` results in

```
  ┌──1  
 +│     
  └──2  
```

Detailed examples
-----------------

See [Sample Parsers](https://github.com/i-e-b/Gool/tree/master/SamplesStd) for more fully functional examples.

### Basic infix arithmetic calculator

```csharp
using Gool;
using static Gool.BNF; // Include BNF methods without needing 'BNF.' everywhere

public double EvaluateExpression(string expression)
{
    var result = Arithmetic().ParseEntireString(expression);    // Step 1: parse input
    var tree = TreeNode.FromParserMatch(result, prune: true);   // Step 2: build expression tree
    var final = TreeNode.TransformTree(tree, ApplyOperation);   // Step 3: reduce the tree to a value
    
    return final;
}

public static Package Arithmetic()
{
    var _expression = Forward();

    BNF
        add_sub = OneOf('+', '-'),
        mul_div = OneOf('*', '/'),
        exp     = '^';

    BNF
        number     = FractionalDecimal(),
        factor     = number | ('(' > _expression > ')'),
        power      = factor > !(exp > factor),
        term       = power % mul_div,
        expression = term % add_sub;

    _expression.Is(expression);

    add_sub.TagWith(Operation).PivotScope();
    mul_div.TagWith(Operation).PivotScope();
    exp    .TagWith(Operation).PivotScope();
    number .TagWith(Value);

    return expression.WithOptions(Options.SkipWhitespace);
}

public const string Operation = "operation";
public const string Value = "value";

private static TreeNode ApplyOperation(TreeNode node)
{
    if (node.Source.Tag is null) return node.Children[0]; // pull child up through joining nodes

    if (node.Source.Tag != Operation) return node; // only look at operation nodes
    var operation = node.Source.Value;

    if (node.Children.Count < 2) throw new Exception("Invalid expression");
    var left = node.Children[0].Source;
    var right = node.Children[1].Source;

    if (!double.TryParse(left.Value, out var a)
     || !double.TryParse(right.Value, out var b)) return node; // one of our children is not a number

    // Both children are values: perform the operation
    var result = operation switch
    {
        "+" => a + b,
        "-" => a - b,
        "*" => a * b,
        "/" => a / b,
        "^" => Math.Pow(a, b),
        _ => throw new NotImplementedException($"Operation not implemented: '{operation}'")
    };

    // Return a new node with the calculated value
    return TreeNode.FromString(result.ToString(CultureInfo.InvariantCulture), Value);
}
```

### Simplified XML Parser

```csharp
BNF // Fragments
    text       = Regex("[^<>]+"),
    identifier = Regex("[_a-zA-Z][_a-zA-Z0-9]*"),
    whitespace = Regex(@"\s+");

BNF // Literals
    quoted_string = '"' > identifier > '"',
    attribute     = whitespace > identifier > '=' > quoted_string;

BNF // tags
    tag_id    = identifier.Tagged(TagId),
    open_tag  = '<' > tag_id > -attribute > '>',
    close_tag = "</" > tag_id > '>';

attribute.TagWith(Attribute);
text.TagWith(Text);
open_tag.TagWith(OpenTag).OpenScope();
close_tag.TagWith(CloseTag).CloseScope();

return Recursive(tree => -(open_tag > -(tree | text) > close_tag)).WithOptions(Options.None);
```


### Full spec JSON parser

From https://www.json.org/json-en.html

```csharp
var value = Forward();

BNF // Basic components
    ws = AnyWhiteSpace;

BNF // Strings
    unicodeEsc    = 'u' > AnyCharacterInRanges(('0', '9'), ('a', 'f'), ('A', 'F')).Repeat(4),
    escape        = OneOf('"', '\\', '/', 'b', 'f', 'n', 'r', 't') | unicodeEsc,
    character     = AnyCharacterNotInRanges('"', '\\') | ('\\' > escape),
    characters    = -character,
    quoted_string = '"' > characters > '"';

BNF // Elements of arrays
    element  = ws > value > ws,
    elements = element % ',';

BNF // Members of objects
    member_key = quoted_string.Copy(),
    member     = ws > member_key > ws > ':' > element,
    members    = member % ',';

BNF // Objects
    object_enter = '{',
    object_leave = '}',
    object_block = object_enter > (ws | members) > object_leave;

BNF // Arrays
    array_enter = '[',
    array_leave = ']',
    array_block = array_enter > elements > array_leave;

BNF number = FractionalDecimal(groupMark: "", decimalMark: "."); // this is slightly out of spec, as it allows "01234" or "+1234"

BNF primitive = quoted_string | number | "true" | "false" | "null";

value.Is(object_block | array_block | primitive);


array_enter.OpenScope().TagWith("array");
array_leave.CloseScope();

object_enter.OpenScope().TagWith("object");
object_leave.CloseScope();

member_key.TagWith("key");
primitive.TagWith("value");

return element.WithOptions(Options.None);
```
