using System.Diagnostics;
using NUnit.Framework;
using Phantom.Scanners;
using Samples;
// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class PascalLanguageTests
{
    [Test]
    public void BasicPascalProgramParsesOK()
    {
        var parser = new PascalParser().TheParser;
        var scanner = new ScanStrings(sample_program)
        {
            SkipWhitespace = true,
            Transform = new TransformToLower()
        };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        PrintFailures(scanner);

        Assert.That(result.Success, Is.True, "Parsing failed");
        Assert.That(result.Value.ToLower(), Is.EqualTo(sample_program.ToLower()));
    }

    [Test]
    [TestCase(missing_quote)]
    [TestCase(missing_begin)]
    public void InvalidProgramFails(string program)
    {
        var parser = new PascalParser().TheParser;
        var scanner = new ScanStrings(missing_quote)
        {
            SkipWhitespace = true,
            Transform = new TransformToLower()
        };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Assert.That(result.Success, Is.False);

        PrintFailures(scanner);
    }

    private static void PrintFailures(ScanStrings scanner)
    {
        foreach (var mismatch in scanner.ListFailures())
        {
            Console.WriteLine("==================================================");
            Console.WriteLine(mismatch);
        }
    }

    private const string sample_program =
        """
        program WriteName;
        var
          i:Integer;
          j:String;
        begin
          Write('Please tell me your name: ');
          ReadLn(Name);
          for i := 1 to 100 do
          begin
            WriteLn('Hello ', Name)
          end
        end.
        """;

    private const string missing_begin =
        """
        program WriteName;
        var
          i:Integer;
          j:String;
        begin
          Write('Please tell me your name: ');
          ReadLn(Name);
          for i := 1 to 100 do
            WriteLn('Hello ', Name)
          end
        end.
        """;

    private const string missing_quote =
        """
        program WriteName;
        var
          i:Integer;
          j:String;
        begin
          Write('Please tell me your name: );
          ReadLn(Name);
          for i := 1 to 100 do
          begin
            WriteLn('Hello ', Name)
          end
        end.
        """;
}