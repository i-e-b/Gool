using System.Diagnostics;
using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class PrefixCsvTests
{
    private const string SimplestList = " a, b, c, d ";
    private const string PrefixedList = " [4] a, b, c, d ";
    private const string NestedSample =
        """
        [5] a,
            [3] b1, b2, b3,
            [4] c1, c2, c3, c4,
            d, e,
        [3] f, g, h

        """;

    [Test]
    public void can_read_a_raw_list_without_prefix()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = PrefixCsvExample.Parser.ParseEntireString(SimplestList);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        if (!result.Success)
        {
            foreach (var fail in result.Scanner.ListFailures())
            {
                Console.WriteLine(fail);
            }
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            Console.Write($"({token.Tag}: {token.Value}); ");
        }
    }

    [Test]
    public void can_read_a_simple_prefixed_list()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = PrefixCsvExample.Parser.ParseEntireString(PrefixedList);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        if (!result.Success)
        {
            foreach (var fail in result.Scanner.ListFailures())
            {
                Console.WriteLine(fail);
            }
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            Console.Write($"({token.Tag}: {token.Value}); ");
        }
    }

    [Test]
    public void can_read_a_nested_prefixed_list()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = PrefixCsvExample.Parser.ParseEntireString(NestedSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        if (!result.Success)
        {
            foreach (var fail in result.Scanner.ListFailures())
            {
                Console.WriteLine(fail);
            }
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            if (token.Tag == PrefixCsvExample.List) Console.WriteLine();

            Console.Write($"({token.Tag}: {token.Value}); ");
        }
    }
}