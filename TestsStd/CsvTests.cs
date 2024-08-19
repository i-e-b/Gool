using System.Diagnostics;
using NUnit.Framework;
using Phantom.Results;
using Samples;

namespace TestsStd;

[TestFixture]
public class CsvTests
{

    private const string CsvFileExample
        = """
          one, two, three
          1,  2,3
          4,5, 6
          "seven is ""heaven""?", 8 , 9
          """;
    
    [Test]
    public void comma_separated_values()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = CsvExample.Csv(true, ",", "\n").ParseString(CsvFileExample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            if (token.Tag == CsvExample.NewRow) Console.WriteLine();
            else Console.Write($"{token.Value} [{token.Tag}]; ");
        }
    }
    
    [Test]
    public void comma_separated_values_without_header_row()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = CsvExample.Csv(false, ",", "\n").ParseString(CsvFileExample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            if (token.Tag == CsvExample.NewRow) Console.WriteLine();
            else Console.Write($"{token.Value} [{token.Tag}]; ");
        }
    }

    [Test]
    public void csv_with_enclosing_scopes()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = CsvExample.Csv(true, ",", "\n").ParseString(CsvFileExample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        var tree = ScopeNode.FromMatch(result);
        PrintRecursive(tree, 0);
    }

    private const string TsvFileExample
        = "one \t  two\t     three\n1  \t2\t3\n4 \t5\t    6\n\"seven is \"\"heaven\"\"?\"\t     8\t   9";
    
    [Test]
    public void tab_separated_values()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = CsvExample.Csv(true, "\t", "\n").ParseString(TsvFileExample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            if (token.Tag == CsvExample.NewRow) Console.WriteLine();
            else Console.Write($"{token.Value} [{token.Tag}]; ");
        }
    }

    private const string AsciiFileExample
        = "one\u001Ftwo\u001Fthree\u001E" +
          "1\u001F2\u001F3\n4\u001F5\u001F6\u001E" +
          "\"seven is \"\"heaven\"\"?\"\u001F8\u001F9";
    
    [Test] // 1F is unit separator, 1E is record separator.
    public void values_with_non_printing_separators()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = CsvExample.Csv(true, "\u001F", "\u001E").ParseString(AsciiFileExample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            if (token.Tag == CsvExample.NewRow) Console.WriteLine();
            else Console.Write($"{token.Value} [{token.Tag}]; ");
        }
    }
    
    
    

    private static void PrintRecursive(ScopeNode node, int indent)
    {
        switch (node.NodeType)
        {
            case ScopeNodeType.Root:
                Console.WriteLine("Document");
                break;
            case ScopeNodeType.Data:
                Console.WriteLine($"{I(indent)}{node.DataMatch?.Value.Trim()} [{node.DataMatch?.Tag}]");
                break;
            case ScopeNodeType.ScopeChange:
                Console.WriteLine($"{I(indent)}Scope [{node.OpeningMatch?.Tag}]");
                break;
            default:
                throw new ArgumentOutOfRangeException();
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, indent+1);
        }
    }

    private static string I(int indent)
    {
        return new string(' ', indent * 4);
    }
}