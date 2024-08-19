using System.Diagnostics;
using System.Text;
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
            switch (token.Tag)
            {
                case CsvExample.Row or CsvExample.Header:
                    continue;
                case CsvExample.NewRow:
                    Console.WriteLine();
                    break;
                default:
                    Console.Write($"{token.Value} [{token.Tag}]; ");
                    break;
            }
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
            switch (token.Tag)
            {
                case CsvExample.Row or CsvExample.Header:
                    continue;
                case CsvExample.NewRow:
                    Console.WriteLine();
                    break;
                default:
                    Console.Write($"{token.Value} [{token.Tag}]; ");
                    break;
            }
        }
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
            switch (token.Tag)
            {
                case CsvExample.Row or CsvExample.Header:
                    continue;
                case CsvExample.NewRow:
                    Console.WriteLine();
                    break;
                default:
                    Console.Write($"{token.Value} [{token.Tag}]; ");
                    break;
            }
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
    

    [Test, Description("Using the scope tree to decompose the CSV into a C# object")]
    public void csv_with_enclosing_scopes()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = CsvExample.Csv(true, ",", "\n").ParseString(CsvFileExample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        Assert.That (result.Success, Is.True);

        var tree = ScopeNode.FromMatch(result);
        
        var document = new CsvDocument();
        foreach (var row in tree.Children)
        {
            if (row.OpeningMatch?.Tag == CsvExample.Header)
            {
                document.SetHeaders(CsvRow.ReadRow(row.Children));
            }
            else if (row.OpeningMatch?.Tag == CsvExample.Row)
            {
                document.AddRow(CsvRow.ReadRow(row.Children));
            }
        }
        
        Console.WriteLine(document.PrettyPrint());
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


public class CsvDocument
{
    public List<string> Headers { get; set; } = new();
    public List<CsvRow> CsvRows { get; set; } = new();

    public void SetHeaders(List<string> headers)
    {
        Headers.Clear();
        Headers.AddRange(headers);
    }

    public void AddRow(List<string> row)
    {
        if (row.Count < 1) return;
        CsvRows.Add(new CsvRow(row));
    }

    public string PrettyPrint()
    {
        const string sep = " | ";
        var sb = new StringBuilder();
        var colWidths = new List<int>();

        // Calculate column widths
        for (var index = 0; index < Headers.Count; index++)
        {
            if (colWidths.Count <= index) colWidths.Add(0);
            colWidths[index] = Math.Max(colWidths[index], sep.Length + Headers[index].Length);
        }

        foreach (var row in CsvRows)
        {
            for (var index = 0; index < row.Entries.Count; index++)
            {
                if (colWidths.Count <= index) colWidths.Add(0);
                colWidths[index] = Math.Max(colWidths[index], sep.Length + row.Entries[index].Length);
            }
        }
        
        // Output headers
        if (Headers.Count > 0)
        {
            for (var index = 0; index < Headers.Count; index++)
            {
                sb.Append(Headers[index]);
                sb.Append(' ', colWidths[index] - Headers[index].Length - sep.Length);
                sb.Append(sep);
            }
            sb.AppendLine();
            sb.Append('=', colWidths.Sum());
            sb.AppendLine();
        }

        // Output data
        foreach (var row in CsvRows)
        {
            for (var index = 0; index < row.Entries.Count; index++)
            {
                sb.Append(row.Entries[index]);
                sb.Append(' ', colWidths[index] - row.Entries[index].Length - sep.Length);
                sb.Append(sep);
            }
            sb.AppendLine();
        }
        

        return sb.ToString();
    }
}
    

public class CsvRow
{
    public CsvRow(List<string> row)
    {
        Entries.AddRange(row);
    }

    public List<string> Entries { get; set; } = new();

    public static List<string> ReadRow(List<ScopeNode> list)
    {
        return list.Select(item=>item.DataMatch?.Value ?? "").ToList();
    }
}