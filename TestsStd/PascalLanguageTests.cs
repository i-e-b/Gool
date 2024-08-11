using System.Diagnostics;
using NUnit.Framework;
using Phantom.Results;
using Phantom.Scanners;
using Samples;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class PascalLanguageTests
{
    private const string sample_program =
        """
        program WriteName;
        var
          i, j : Integer;
          name : String;
        begin
          Write('Please tell me your name: ');
          ReadLn(name);
          for i := 1 to 100 do
          begin
            WriteLn('Hello ', name)
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


        var tree = ScopeNode.FromMatchesDepthFirst(result);
        tree.Specialise(PascalParser.Expression, PascalParser.PascalString, PascalParser.Identifier);

        bool line = false;
        PrintRecursive(tree, 0, ref line);

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


    private static void PrintRecursive(ScopeNode node, int indent, ref bool line)
    {
        switch (node.NodeType)
        {
            case ScopeNodeType.Root:
                Console.WriteLine("Document");
                line = true;
                if (node.OpeningMatch is not null || node.ClosingMatch is not null) Console.WriteLine("Unbalanced scopes!");
                break;
            case ScopeNodeType.Data:
                var tag = node.DataMatch?.Tag;
                
                if (tag == PascalParser.StatementEnd)
                {
                    if (line) Console.Write(I(indent));
                    Console.WriteLine($"{node.DataMatch?.Value}");
                    line = true;
                }
                else
                {
                    if (line) Console.Write(I(indent));
                    Console.Write(node.DataMatch?.Value + " ");
                    line = false;
                }

                break;
            case ScopeNodeType.ScopeChange:
                if (!line) Console.WriteLine();
                Console.WriteLine($"{I(indent)}'{node.OpeningMatch?.Value}' =>");
                line = true;
                break;

            default:
                Assert.Fail($"Node does not have a valid type: {node}");
                break;
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, indent + 1, ref line);
        }

        if (node.NodeType == ScopeNodeType.ScopeChange)
        {
            if (!line) Console.WriteLine();
            Console.WriteLine($"{I(indent)}'{node.ClosingMatch?.Value}' <=");
            line = true;
        }
    }

    private static string I(int indent)
    {
        return new string(' ', indent * 4);
    }
}