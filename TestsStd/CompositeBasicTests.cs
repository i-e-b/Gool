using System.Diagnostics;
using Gool;
using Gool.Parsers;
using Gool.Scanners;
using NUnit.Framework;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class CompositeBasicTests
{
    private static BNF DelimitedListParserSample()
    {
        BNF item      = BNF.Regex("[a-zA-Z]+");
        BNF delimiter = ",";

        BNF list = item % delimiter;

        item.TagWith("item");
        delimiter.TagWith("comma");

        return list;
    }

    [Test]
    public void delimited_list_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one, two ,three , four five
            """;

        Console.WriteLine("\r\n=================================================================================");
        var sw = new Stopwatch();
        sw.Start();
        var result = DelimitedListParserSample().ParseString(correct_sample, options: BNF.Options.SkipWhitespace);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", ",", "two", ",", "three", ",", "four" }).AsCollection);
    }

    [Test]
    public void delimited_list_parser_accepts_single_item_input()
    {
        const string correct_sample =
            """
            one
            """;

        Console.WriteLine("\r\n=================================================================================");

        var sw = new Stopwatch();
        sw.Start();
        var result = DelimitedListParserSample().ParseString(correct_sample, options: BNF.Options.SkipWhitespace);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one" }).AsCollection);
    }

    [Test]
    public void delimited_list_parser_excludes_trailing_separator()
    {
        const string correct_sample =
            """
            one, two, three,
            """;

        Console.WriteLine("\r\n=================================================================================");

        var sw = new Stopwatch();
        sw.Start();
        var result = DelimitedListParserSample().ParseString(correct_sample, options: BNF.Options.SkipWhitespace);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", ",", "two", ",", "three" }).AsCollection);
    }

    private static IParser DifferenceParserSample()
    {
        BNF item = BNF.Regex("[a-zA-Z]+");
        BNF end  = "end";

        BNF list = +(item / end);

        item.TagWith("item");

        return list;
    }

    [Test]
    public void difference_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two three end
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = DifferenceParserSample();
        var scanner = new ScanStrings(correct_sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "three" }).AsCollection);
    }


    private static IParser ExclusiveParserSample()
    {
        BNF prefixed  = BNF.Regex("px_[_a-zA-Z]+");
        BNF postfixed = BNF.Regex("[_a-zA-Z]+_pf");

        BNF list = +(prefixed ^ postfixed);

        prefixed.TagWith("item");
        postfixed.TagWith("item");

        return list;
    }

    [Test]
    public void exclusive_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            px_one two_pf px_end_pf
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = ExclusiveParserSample();
        var scanner = new ScanStrings(correct_sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "px_one", "two_pf" }).AsCollection);
    }


    private static IParser IntersectionParserSample()
    {
        BNF one = "one";
        BNF two = "two";

        BNF list = +(one & two);

        one.TagWith("item");
        two.TagWith("item");

        return list;
    }

    [Test]
    public void intersection_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two two one one one
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = IntersectionParserSample();
        var scanner = new ScanStrings(correct_sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "two", "one" }).AsCollection);
    }

    private static BNF.Package RepetitionParserSample()
    {
        BNF item = (BNF)"one" | "two" | "three" | "four";

        BNF list = !item > (-item);

        item.TagWith("item");

        return list.WithOptions(BNF.Options.SkipWhitespace);
    }

    [Test]
    public void repetition_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two three four
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = RepetitionParserSample();

        Console.WriteLine(parser.ToString());

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.ParsePartialString(correct_sample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.DepthFirstWalk())
        {
            Console.WriteLine(match.Value + ": " + match.Tag + " <-- " + match.SourceParser);
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "three", "four" }).AsCollection);
    }


    private static IParser SequenceParserSample()
    {
        BNF item = (BNF)"one" > "two" > "three" > "four";

        BNF list = +item;

        item.TagWith("item");

        return list;
    }

    [Test]
    public void sequence_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two three four
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = SequenceParserSample();
        var scanner = new ScanStrings(correct_sample) { AutoAdvance = BNF.WhiteSpaceString };

        Console.WriteLine(parser.ToString());

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.DepthFirstWalk())
        {
            Console.WriteLine(match.Value + ": " + match.Tag + " <-- " + match.SourceParser);
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one two three four" }).AsCollection);
    }


    private static IParser TerminatedListParserSample()
    {
        BNF item       = BNF.Regex("[a-z]+");
        BNF terminator = ';';

        BNF list = item < terminator;

        BNF groups = -('(' > list > ')') > BNF.EndOfInput;

        item.TagWith("item");

        return groups;
    }

    [Test]
    public void terminated_list_accepts_correct_input()
    {
        const string correct_sample =
            """

            ( a; )
            ( b; c; )
            ( d; e; f; ) 

            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = TerminatedListParserSample();
        var scanner = new ScanStrings(correct_sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "a", "b", "c", "d", "e", "f" }).AsCollection);
    }

    [Test]
    public void terminated_list_rejects_incorrect_input()
    {
        const string incorrect_sample =
            """

            ( a; )
            ( b; c )
            ( d; e; f; ) 

            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = TerminatedListParserSample();
        var scanner = new ScanStrings(incorrect_sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.False);
    }


    private static IParser NonConsumingMatchSample()
    {
        BNF
            name       = BNF.Regex("[a-zA-Z][a-zA-Z0-9]*"),
            not_a_func = name > ~BNF.NoneOf('{', '(', '['), // the '~' means we won't consume the list separator ','
            list       = not_a_func % ',';

        not_a_func.TagWith("item");
        return list;
    }

    [Test]
    public void non_consuming_match()
    {
        const string sample =
            """
            one, two, one, two(), three, four
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = NonConsumingMatchSample();
        var scanner = new ScanStrings(sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "one" }).AsCollection);
    }

    private static IParser UnionParserSample()
    {
        BNF item = (BNF)"one" | "two" | "three" | "not" | "in" | "the" | "sample";

        BNF list = +item;

        item.TagWith("item");

        return list;
    }

    [Test]
    public void union_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two one two three four
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = UnionParserSample();
        var scanner = new ScanStrings(correct_sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine(result.SourceParser.ToString());

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "one", "two", "three" }).AsCollection);
    }


    private static IParser ContextParserSample()
    {

        var _wrapped = BNF.Forward();
        BNF
            tag_id = BNF.Regex("[a-zA-Z][a-zA-Z0-9]*"),
            text   = -(BNF.AnyChar / "<"),
            wrapped =
                BNF.Context(
                    prefix: '<' > tag_id > '>',
                    select: result =>
                        result.GetTag("TagId"),
                    next: tag =>
                        -(text | _wrapped) > "</" > ((BNF)tag.Value).TagWith("TagId") > '>'
                );

        _wrapped.Is(wrapped);
        tag_id.TagWith("TagId");
        text.TagWith("Text");

        return wrapped;
    }

    [Test]
    public void context_parser_allows_forward_references()
    {
        const string sample =
            """
            <one>two</one>
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = ContextParserSample();
        var scanner = new ScanStrings(sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine(result.SourceParser.ToString());

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "one" }).AsCollection);
    }

    [Test]
    public void context_parser_allows_recursive_forward_references()
    {
        const string sample =
            """
            <one>two<three>four</three></one>
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = ContextParserSample();
        var scanner = new ScanStrings(sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine(result.SourceParser.ToString());

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "three", "four", "three", "one" }).AsCollection);
    }

    [Test]
    public void context_parser_can_reject_on_incorrect_forward_references()
    {
        const string sample =
            """
            <one>two</three>
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser  = ContextParserSample();
        var scanner = new ScanStrings(sample) { AutoAdvance = BNF.WhiteSpaceString };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.False, result + ": " + result.Value);
    }


    private static IParser CompositeSample()
    {
        return CssStrEsc("Hello World");

        BNF CssStrEsc(string s) => BNF.Composite(s.Select(CssCharEsc));

        BNF CssCharEsc(char c)
        {
            BNF
                u       = char.ToUpper(c), // upper case char
                l       = char.ToLower(c), // lower case char
                zs      = BNF.Repeat('0', 0, 4), // up to 4 '0'
                esc     = ((int)c).ToString("X2") | BNF.CharacterInRanges(('g', 'z'), ('G', 'Z')), // and two hex chars for the character
                pattern = u | l | ('\\' > zs > esc); // any of the above

            return pattern;
        }
    }

    [Test]
    [TestCase("hello world", true)]
    [TestCase("Hello World", true)]
    [TestCase("hELLo wORLd", true)]
    [TestCase("hELL\\o w\\ORLd", true)]
    [TestCase("hELL\\6F w\\6FRLd", true)]
    [TestCase("Hello", false)]
    [TestCase("Hello Earth", false)]
    [TestCase("hELL\\6E w\\6RLd", false)]
    public void composite_sequence_matches_all_items(string sample, bool matches)
    {
        var parser  = CompositeSample();
        var scanner = new ScanStrings(sample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.EqualTo(matches), result + ": " + result.Value);
    }

    private static IParser ParallelSetParserSample()
    {
        BNF words                = +BNF.AnyChar;
        BNF starts_with_alphanum = BNF.Regex("^[a-zA-Z].*");
        BNF ends_with_alphanum   = BNF.Regex(".*[a-zA-Z]$");
        BNF dns_length_limit     = BNF.RemainingLength(min: 2, max: 80);
        BNF domain_name          = words.WithValidators(starts_with_alphanum, ends_with_alphanum, dns_length_limit);

        return domain_name;
    }

    [Test]
    [TestCase("this.is.ok")]
    [TestCase("th1s . is . 0k")]
    public void parallel_set_accepts_valid_input(string correct_sample)
    {
        var parser  = ParallelSetParserSample();
        var scanner = new ScanStrings(correct_sample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.ToString(), Is.EqualTo(correct_sample));
    }

    [Test]
    [TestCase("not.ok-")]
    [TestCase("-nope")]
    [TestCase("1-nope-2")]
    [TestCase("a")]
    [TestCase("far.too.looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong")]
    public void parallel_set_rejects_invalid_input(string bad_sample)
    {
        var parser  = ParallelSetParserSample();
        var scanner = new ScanStrings(bad_sample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.False);
    }



    private static IParser PreviousCharCheckSample()
    {
        // deliberately ambiguous grammar
        BNF
            a_space = 'a' > -BNF.WhiteSpace,
            b_no_sp = 'b',
            space_c = BNF.RequiredWhiteSpace > 'c'; // 'RequiredWhiteSpace' allows us to resolve the ambiguity.

        return -((a_space | b_no_sp) > space_c > ',' > !BNF.WhiteSpace);
    }

    [Test]
    public void previous_match_tests()
    {
        const string sample  = "a c, b c,";
        var          parser  = PreviousCharCheckSample();
        var          scanner = new ScanStrings(sample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var match in result.TaggedTokensDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.ToString(), Is.EqualTo(sample));
    }




    private static BNF PreviousMatchSplitSample()
    {
        // deliberately weird grammar
        BNF
            name  = BNF.IdentifierString(),
            start = "START_" > name > BNF.LineEnd,
            block = BNF.AnyWhiteSpace > BNF.IdentifierString() > ";" > BNF.LineEnd;

        name.TagWith("name");
        block.TagWith("block");

        return BNF.Context(
            prefix: start,                                                              // string starts with "START_" then any ident
            select: result =>
                result.GetTag("name"),                                                  // pick the ident string
            next: match =>
                +block                                                                  // many sets of " blah;\r\n"
              > match.Value.CaseInsensitive().TagWith("end")                            // then must end with the same ident as start
               >
                (
                    (BNF.PreviousMatches(m => m.GetTag("end"), +BNF.Uppercase) > "!")   // if the end ident is all uppercase, the final char must be '!'
                  | "."                                                                 // otherwise the final char must be '.'
                )
        );
    }

    [Test]
    [TestCase("START_test\r\n one;\n   two;\rthree;\r\ntest.")]
    [TestCase("START_test\r\n one;\n   two;\rthree;\r\nTEST!")]
    [TestCase("START_test\r\n one;\n   two;\rthree;\r\nTest.")]
    public void split_decision_test(string input)
    {
        var parser  = PreviousMatchSplitSample();
        var scanner = new ScanStrings(input);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.ToString(), Is.EqualTo(input));
    }
}