using System.Diagnostics;
using System.Text;
using Gool.Results;
using Gool.Scanners;
using NUnit.Framework;
using Samples;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class PascalLanguageTests
{
    private const string sample_program =
        """
        program WriteName; { This is a sample program }
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

    private const string dead_stop_program =
        """
        program WriteName;
        var
          i, j : Integer;
          name : String;
        begin
          #error This should stop the parser with an exception;
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
        var sw = new Stopwatch();
        sw.Start();
        var result = PascalExample.Parser.ParseEntireString(sample_program);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine("==================================================");
        var scopeTree = ScopeNode.FromMatch(result);
        scopeTree.Specialise(PascalExample.Expression, PascalExample.PascalString, PascalExample.Identifier);

        bool line = false;
        PrintRecursive(scopeTree, 0, ref line);

        PrintFailures(result.Scanner);

        var check = new StringBuilder();
        PrintRecursive(scopeTree, check);
        Console.WriteLine("==================================================");
        Console.WriteLine(check);
        Console.WriteLine("==================================================");
        Assert.That(check.ToString(), Is.EqualTo("program WriteName ; var i , j : Integer ; name : String ; begin Write ( 'Please tell me your name: ' ); ReadLn ( name ); for i := 1  to 100  do begin WriteLn ( 'Hello ' , name )endend. "));
        

        Assert.That(result.Success, Is.True, "Parsing failed");
        Assert.That(result.Value.ToLower(), Is.EqualTo(sample_program.ToLower()));
    }

    [Test]
    public void DirectActionAllowsImmediateStop()
    {
        var ex = Assert.Throws<Exception>(() => PascalExample.Parser.ParseEntireString(dead_stop_program));

        Assert.That(ex?.Message, Is.EqualTo("Hit the compiler directive: #error This should stop the parser with an exception"));
    }

    [Test]
    public void SyntaxColoringPascalProgram()
    {
        var result = PascalExample.Parser.ParsePartialString(sample_program);
        var output = new StringBuilder();


        output.Append(
            """
            <!DOCTYPE html>
            <html><head><title>Demo</title><style>
                .identifier { color: #007; }
                .string { color: #A00; }
                .expression { color: #000; }
                .statement { color: #000; }
                .constant { color: #0AA; }
                .keyword { color: #A0A; }
                .operator { color: #00F; }
                .inequality { color: #0F0; }
                .openParen { color: #F00; }
                .closeParen { color: #F0A; }
                .openBracket { color: #A0F; }
                .closeBracket { color: #AF0; }
            </style></head>
            <body><pre>
            """);

        foreach (var node in result.BottomLevelMatchesDepthFirst())
        {
            var safeText = node.Value.Replace("<", "&lt;").Replace(">", "&gt;");
            if (node.Tag is not null)
            {
                output.Append($"<span class=\"{node.Tag}\">{safeText}</span>");
            }
            else
            {
                output.Append(safeText);
            }

        }

        output.Append("</pre></body></html>");
        Console.WriteLine(output.ToString());
    }

    [Test]
    [TestCase(missing_quote)]
    [TestCase(missing_begin)]
    public void InvalidProgramFails(string program)
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = PascalExample.Parser.ParsePartialString(program);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Assert.That(result.Success, Is.False);

        PrintFailures(result.Scanner);
    }

    private static void PrintFailures(IScanner scanner)
    {
        foreach (var mismatch in scanner.ListFailures())
        {
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
                
                if (tag == PascalExample.StatementEnd)
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
    
    private static void PrintRecursive(ScopeNode node, StringBuilder sb)
    {
        switch (node.NodeType)
        {
            case ScopeNodeType.Root: break;
            case ScopeNodeType.Data:
                sb.Append(node.DataMatch?.Value + " ");
                break;
            case ScopeNodeType.ScopeChange:
                sb.Append(node.OpeningMatch?.Value + " ");
                break;
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, sb);
        }

        if (node.NodeType == ScopeNodeType.ScopeChange)
        {
            sb.Append(node.ClosingMatch?.Value);
        }
    }
    
    private static string I(int indent)
    {
        return new string(' ', indent * 4);
    }
}