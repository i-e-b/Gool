using System.Diagnostics;
using NUnit.Framework;
using Phantom;
using Phantom.Scanners;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class CompositeBasicTests
{
    private static BNF DelimitedListParserSample()
    {
        BNF item = "#[a-zA-Z]+";
        BNF delimiter = ",";

        BNF list = item % delimiter;

        item.Tag("item");
        delimiter.Tag("comma");

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
        var result = DelimitedListParserSample().ParseString(correct_sample, BNF.Options.SkipWhitespace);
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
        var result = DelimitedListParserSample().ParseString(correct_sample, BNF.Options.SkipWhitespace);
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
        var result = DelimitedListParserSample().ParseString(correct_sample, BNF.Options.SkipWhitespace);
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
        BNF item = "#[a-zA-Z]+";
        BNF end = "end";

        BNF list = +(item / end);

        item.Tag("item");

        return list.Parser();
    }

    [Test]
    public void difference_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two three end
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = DifferenceParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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
        BNF prefixed = "#px_[_a-zA-Z]+";
        BNF postfixed = "#[_a-zA-Z]+_pf";

        BNF list = +(prefixed ^ postfixed);

        prefixed.Tag("item");
        postfixed.Tag("item");

        return list.Parser();
    }

    [Test]
    public void exclusive_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            px_one two_pf px_end_pf
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = ExclusiveParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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

        one.Tag("item");
        two.Tag("item");

        return list.Parser();
    }

    [Test]
    public void intersection_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two two one one one
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = IntersectionParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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
    
    private static IParser RepetitionParserSample()
    {
        BNF item = (BNF)"one" | "two" | "three" | "four";

        BNF list = !item > (-item);

        item.Tag("item");

        return list.Parser();
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
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "three", "four" }).AsCollection);
    }

    
    private static IParser SequenceParserSample()
    {
        BNF item = (BNF)"one" > "two" > "three" > "four";

        BNF list = +item;

        item.Tag("item");

        return list.Parser();
    }
    
    [Test]
    public void sequence_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two three four
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = SequenceParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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
        BNF item = "#[a-z]+";
        BNF terminator = ';';

        BNF list = item < terminator;

        BNF groups = -('(' > list > ')') > BNF.EndOfInput;

        item.Tag("item");

        return groups.Parser();
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
        var parser = TerminatedListParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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
        const string correct_sample =
            """

            ( a; )
            ( b; c )
            ( d; e; f; ) 

            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = TerminatedListParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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

    
    private static IParser UnionParserSample()
    {
        BNF item = (BNF)"one" | "two" | "three" | "not" | "in" | "the" | "sample";

        BNF list = +item;

        item.Tag("item");

        return list.Parser();
    }
    
    [Test]
    public void union_parser_accepts_correct_input()
    {
        const string correct_sample =
            """
            one two one two three four
            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = UnionParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };

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

        Console.WriteLine(result.SourceParser?.ToString());

        Console.WriteLine("\r\n=================================================================================");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.TaggedTokensDepthFirst().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "one", "two", "three" }).AsCollection);
    }
}