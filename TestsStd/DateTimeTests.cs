using System.Diagnostics;
using NUnit.Framework;
using Phantom.Results;
using Samples;

namespace TestsStd;

[TestFixture]
public class DateTimeTests
{

    [Test]
    [TestCase("2024-08-14", true)]
    [TestCase("2007-04-05T14:30", true)]
    [TestCase("2007-04-05t14:30", true)]
    [TestCase("2007-04-05 14:30", true)]
    [TestCase("2007-04-05T14:30:15", true)]
    [TestCase("2007-04-05T14:30:15.654", true)]
    [TestCase("2007-04-05T14:30:15.999Z", true)]
    [TestCase("P1Y2M10DT2H30M", true)]
    [TestCase("2007-12-14T13:30/15:30", true)]
    [TestCase("2007-03-01T13:00:00Z/2008-05-11T15:30:00Z", true)]
    [TestCase("2007-03-01T13:00:00Z/P1Y2M10DT2H30M", true)]
    [TestCase("P1Y2M10DT2H30M/2008-05-11T15:30:00Z", true)]
    [TestCase("PT2H30M/2008-05-11T15:30:00Z", true)]
    [TestCase("2003-02-15T00:00:00Z/P2M", true)]
    [TestCase("200704051430", true)] // Truncated representation is now deprecated, but this parser includes them
    
    [TestCase("2007-04-05T14:30:61", false)] // parser does rough range checking
    [TestCase("2007-13-05T14:30:01", false)] // parser does rough range checking
    [TestCase("2007-11-32T14:30:01", false)] // parser does rough range checking
    public void ISO8601_Tests(string input, bool shouldPass)
    {
        var result = CheckTime("ISO 8601", ()=>DateTimeExamples.Iso8601().ParseEntireString(input));
        Console.WriteLine(input + " -> ");
        Console.WriteLine(result + "\r\n");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        foreach (var token in result.TaggedTokensDepthFirst())
        {
            Console.WriteLine($"{token.Tag} = {token.Value}");
        }
        
        Assert.That(result.Success, Is.EqualTo(shouldPass));
    }

    [Test]
    [TestCase("2024-08-14", false)] // RFC3339 must have time
    [TestCase("2007-04-05T14:30:15", false)] // RFC3339 must have time zone
    [TestCase("P1Y2M10DT2H30M", false)] // RFC3339 does not support durations
    [TestCase("2007-04-05T14:30Z", false)] // RFC3339 must have seconds
    
    [TestCase("2007-04-05T14:30:01Z", true)]
    [TestCase("2007-04-05t14:30:02-00:00", true)]
    [TestCase("2007-04-05 14:30:03+05:30", true)]
    [TestCase("2007-04-05 14:30:03.1234+05:30", true)]
    public void RFC3339_Tests(string input, bool shouldPass)
    {
        var result = CheckTime("RFC 3339", ()=>DateTimeExamples.Rfc3339().ParseEntireString(input));
        Console.WriteLine(input + " -> ");
        Console.WriteLine(result + "\r\n");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        foreach (var token in result.TaggedTokensDepthFirst())
        {
            Console.WriteLine($"{token.Tag} = {token.Value}");
        }
        
        Assert.That(result.Success, Is.EqualTo(shouldPass));
    }
    

    private static T CheckTime<T>(string name, Func<T> func)
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = func();
        sw.Stop();
        Console.WriteLine($"{name} took {sw.Elapsed.TotalMicroseconds} µs");
        return result;
    }
}