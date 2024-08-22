using System.Diagnostics;
using Gool.Results;
using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class DateTimeTests
{

    [Test]
    [TestCase("2024-08-14", true)]
    [TestCase("2007-04-05T14:30", true)]
    [TestCase("2007-04-05T14:30Z", true)]
    [TestCase("2007-04-05T14:30Z+01:30", true)]
    [TestCase("2007-04-05t14:30Z-05:00", true)]
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

        if (result.Success)
        {
            var dateRange = DateRange.From8601(result.TaggedTokensDepthFirst());
            Console.WriteLine($"Interpreted as {dateRange}");
        }
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

public class DateRange
{
    public DateTime Start { get; init; }
    public TimeSpan Duration { get; init; }

    public override string ToString()
    {
        return $"{Start:yyyy-MM-dd HH:mm:ss} + {Duration}";
    }

    #region Interpret parser output
    public static DateRange From8601(IEnumerable<ParserMatch> tokenSource)
    {
        var tokens = tokenSource.ToList();
        if (tokens.Count < 1) throw new Exception("Invalid date range (no tokens)");

        var span = TimeSpan.Zero;
        var start = DateTime.UtcNow;

        // Read first part (might be complete if not a range)
        var offset = 0;
        if (IsDuration(tokens[offset]))
        {
            span = ReadDuration(ref offset, tokens);
        }
        else if (IsDate(tokens[offset]))
        {
            start = ReadDate(ref offset, tokens);
        }
        else
        {
            throw new Exception($"Invalid date range (unknown start '{tokens[offset].Tag}')");
        }

        if (offset == tokens.Count - 1) return new DateRange { Start = start, Duration = span};
        
        // check '/' for period
        if (tokens[offset].Tag != DateTimeExamples.PeriodMarker) throw new Exception($"Invalid date range (unknown middle '{tokens[offset].Tag}')");
        offset++;
        
        
        if (IsDuration(tokens[offset]))
        {
            span = ReadDuration(ref offset, tokens);
        }
        else if (IsDate(tokens[offset]))
        {
            var end = ReadDate(ref offset, tokens);
            if (span != TimeSpan.Zero) start = end.Subtract(span);
            else span = end - start;
        }
        else
        {
            throw new Exception($"Invalid date range (unknown end '{tokens[offset].Tag}')");
        }
        
        return new DateRange { Start = start, Duration = span};
    }
    
    private static bool IsPeriodMarker(ParserMatch token) => token.Tag == DateTimeExamples.PeriodMarker;

    private static bool IsDate(ParserMatch token)
    {
        if (token.Tag is null) return false;
        if (IsDuration(token)) return false;
        if (IsPeriodMarker(token)) return false;

        return true;
    }

    private static bool IsDuration(ParserMatch token)
    {
        return token.Tag switch
        {
            DateTimeExamples.Seconds or DateTimeExamples.Minutes or DateTimeExamples.Hours 
                or DateTimeExamples.Days or DateTimeExamples.Weeks or DateTimeExamples.Months
                or DateTimeExamples.Years 
                => true,
            
            _ => false
        };
    }

    private static DateTime ReadDate(ref int offset, List<ParserMatch> tokens)
    {
        var now = DateTime.UtcNow;
        var year = now.Year;
        var month = now.Month;
        var day = now.Day;
        var hour = now.Hour;
        var minute = now.Minute;
        var second = now.Second;
        var millisecond = 0;
        var kind = DateTimeKind.Utc;

        for (int i = offset; i < tokens.Count; i++)
        {
            var token = tokens[i];
            offset = i;
            if (IsPeriodMarker(token)) break;

            switch (token.Tag)
            {
                case DateTimeExamples.Year:
                    year = int.Parse(token.Value);
                    break;
                case DateTimeExamples.Month:
                    month = int.Parse(token.Value);
                    break;
                case DateTimeExamples.DayOfMonth:
                    day = int.Parse(token.Value);
                    break;
                case DateTimeExamples.Hour:
                    hour = int.Parse(token.Value);
                    break;
                case DateTimeExamples.Minute:
                    minute = int.Parse(token.Value);
                    break;
                case DateTimeExamples.Second:
                    second = int.Parse(token.Value);
                    break;
                case DateTimeExamples.SecondFraction:
                    millisecond = (int)(double.Parse("0" + token.Value) * 1000.0);
                    break;
                case DateTimeExamples.TimeZone:
                    kind = DateTimeKind.Local; // mostly ignored
                    break;

                default:
                    Console.WriteLine($"{token.Tag} not interpreted");
                    break;
            }
        }

        return new DateTime(year, month, day, hour, minute, second, millisecond, kind);
    }

    private static TimeSpan ReadDuration(ref int offset, List<ParserMatch> tokens)
    {
        var span = TimeSpan.Zero;

        for (int i = offset; i < tokens.Count; i++)
        {
            var token = tokens[i];
            offset = i;
            if (IsPeriodMarker(token)) break;

            switch (token.Tag)
            {
                case DateTimeExamples.Years:
                    span = span.Add(TimeSpan.FromDays(365.2421 * int.Parse(token.Value)));
                    break;
                case DateTimeExamples.Months:
                    span = span.Add(TimeSpan.FromDays(30.4368 * int.Parse(token.Value)));
                    break;
                case DateTimeExamples.Days:
                    span = span.Add(TimeSpan.FromDays(int.Parse(token.Value)));
                    break;
                case DateTimeExamples.Hours:
                    span = span.Add(TimeSpan.FromHours(int.Parse(token.Value)));
                    break;
                case DateTimeExamples.Minutes:
                    span = span.Add(TimeSpan.FromMinutes(int.Parse(token.Value)));
                    break;
                case DateTimeExamples.Seconds:
                    span = span.Add(TimeSpan.FromSeconds(int.Parse(token.Value)));
                    break;
                
                default:
                    Console.WriteLine($"{token.Tag} not interpreted");
                    break;
            }
        }

        return span;
    }
    #endregion Interpret parser output
}