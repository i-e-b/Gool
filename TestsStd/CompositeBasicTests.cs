using System.Diagnostics;
using NUnit.Framework;
using Phantom;
using Phantom.Scanners;
// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class CompositeBasicTests
{
    private static IParser DelimiterListParserSample()
    {
        BNF item = "#[a-z]+";
        BNF terminator = ';';

        BNF list = item < terminator;
        
        BNF groups = -( '(' > list > ')') > BNF.EndOfInput;

        item.Tag("item");

        return groups.Result();
    }
    
    [Test]
    public void delimited_list_accepts_correct_input()
    {
        const string correct_sample =
            """

            ( a; )
            ( b; c; )
            ( d; e; f; ) 

            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = DelimiterListParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };
        
        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        foreach (var match in result.TaggedTokens())
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
        Assert.That(result.TaggedTokens().Select(t => t.Value), Is.EqualTo(new[] { "a", "b", "c", "d", "e", "f"}).AsCollection);
    }
    
    [Test]
    public void delimited_list_rejects_incorrect_input()
    {
        const string correct_sample =
            """

            ( a; )
            ( b; c )
            ( d; e; f; ) 

            """;

        Console.WriteLine("\r\n=================================================================================");
        var parser = DelimiterListParserSample();
        var scanner = new ScanStrings(correct_sample) { SkipWhitespace = true };
        
        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        foreach (var match in result.TaggedTokens())
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
    
    
    private static IParser DifferenceParserSample()
    {
        BNF item = "#[a-zA-Z]+";
        BNF end = "end";

        BNF list = +(item / end);

        item.Tag("item");

        return list.Result();
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
        
        foreach (var match in result.TaggedTokens())
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
        Assert.That(result.TaggedTokens().Select(t => t.Value), Is.EqualTo(new[] { "one", "two", "three" }).AsCollection);
    }

    
    private static IParser ExclusiveParserSample()
    {
        BNF prefixed = "#px_[_a-zA-Z]+";
        BNF postfixed = "#[_a-zA-Z]+_pf";
        
        BNF list = +(prefixed ^ postfixed);

        prefixed.Tag("item");
        postfixed.Tag("item");

        return list.Result();
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
        
        foreach (var match in result.TaggedTokens())
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
        Assert.That(result.TaggedTokens().Select(t => t.Value), Is.EqualTo(new[] { "px_one", "two_pf" }).AsCollection);
    }
}