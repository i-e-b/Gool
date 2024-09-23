using System.Diagnostics;
using Gool;
using Gool.Scanners;
using NUnit.Framework;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class CompositeBasicTests
{
    private static BNF DelimitedListParserSample()
    {
        BNF item = BNF.Regex("[a-zA-Z]+");
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
        BNF end = "end";

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
        BNF prefixed = BNF.Regex("px_[_a-zA-Z]+");
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
        BNF item = BNF.Regex("[a-z]+");
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
    
    
    
    private static IParser ParallelSetParserSample()
    {
        BNF words = +BNF.AnyChar;
        BNF starts_with_alphanum = BNF.Regex("^[a-zA-Z].*");
        BNF ends_with_alphanum = BNF.Regex(".*[a-zA-Z]$");
        BNF dns_length_limit = BNF.RemainingLength(min:2, max:80);
        BNF domain_name = words.WithValidators(starts_with_alphanum, ends_with_alphanum, dns_length_limit);

        return domain_name;
    }
    
    [Test]
    [TestCase("this.is.ok")]
    [TestCase("th1s . is . 0k")]
    public void parallel_set_accepts_valid_input(string correct_sample)
    {
        var parser = ParallelSetParserSample();
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
        var parser = ParallelSetParserSample();
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
}