
## Nice features

- [x] Parser show tags of failed results as the "expected" if parsing can't continue.
- [ ] Demo of segmenting parser: break input into (lines|expressions|statements), then parse each of those. Example of recovering parser if one line is bad.
  - This should really be a 'BadLine' / 'BadBlock' part of the main pattern? 
- [ ] Generate a BNF graphic (display of parser structure) -- maybe Mermaid syntax?
- [ ] Generate parser result graphic (like https://dubroy.com/blog/visualizing-packrat-parsing/ )
- [x] "Immediate stop" parser? If this is successful, we stop any other parsing and return the result up-to the stop parser.
- [x] "Context" parser? Function that takes parser match tree so far, gives a new BNF. This continues as parser match, then continues to next result after the context parser.
- [ ] More extensive read-me that covers all of BNF with examples
- [x] Ability to turn off auto-advance within a specific parser and its children.
- [x] Attempt a low-GC way of doing ParserMatch
    - Also tried turning of GC collection during parse. No measurable benefit.
- [x] Try to remove the `new BNF(new Parser())` wrapping layer.
    - tried: does not provide measurable benefit, at the cost of flexibility and complexity.

## Examples

- [x] SQL connection strings
- [x] Full XML: https://cs.lmu.edu/~ray/notes/xmlgrammar/
- [x] cron expressions: https://docs.rs/cronexpr/latest/cronexpr/
- [x] Complete basic programming language (including interpreter and sample programs)
- [ ] Syntax highlighting plug-in? 
    - https://github.com/jetbrains/resharper-rider-plugin
    - https://plugins.jetbrains.com/docs/intellij/rider.html#open-source-rider-plugins

Huge repo of grammars here: https://github.com/antlr/grammars-v4

Sources for more examples:

- https://github.com/TheAlgorithms
- https://rosettacode.org/wiki/BNF_Grammar
- https://docs.python.org/3/reference/grammar.html
- https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
- https://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html#tag_20_06_13_16
- COBOL might be a good stretch:
  - https://www.ibm.com/docs/en/cobol-zos/6.3?topic=reference-cobol-language-structure
  - https://www.cs.vu.nl/~x/coboldef/coboldef.html
  - https://www.cs.vu.nl/grammarware/browsable/cobol/#EBNF
  - https://github.com/shamrice/COBOL-Examples
- And SQL: https://ronsavage.github.io/SQL/sql-92.bnf.html

Diagrams:
- https://github.com/mermaid-js/mermaid
- https://mermaid.js.org/intro/syntax-reference.html