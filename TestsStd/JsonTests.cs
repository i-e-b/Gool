using System.Diagnostics;
using Gool.Results;
using NUnit.Framework;
using SkinnyJson;
using JsonParser = Samples.JsonParser;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class JsonTests
{
    private const string valid_sample =
        """
        {"menu": {
          "id" : "file",
          "value" :"File",
          "popup": {
            "menuitem": [
              {"value": "New", "onclick": "CreateNewDoc()"},
              {"value": "Open", "onclick": "OpenDoc()"},
              {"value": "Close", "onclick": "CloseDoc()"}
            ]
          },
          "meta": {
            "index": [1, 2.5 , 3.14e-10],
            "affirmative": true,
            "declined": false,
            "tricky-string": "Hello \\\"World\"\r\n"
          }
        }}
        """;

    [Test]
    public void recursive_parsers_can_be_applied_multiple_times()
    {
        var parser = JsonParser.Json;
        var phantomTime = new Stopwatch();
        var result = parser.ParsePartialString(valid_sample);
        phantomTime.Start();
        for (int i = 0; i < 99; i++)
        {
            result = parser.ParsePartialString(valid_sample);
        }
        phantomTime.Stop();
        Console.WriteLine($"Parsing took {phantomTime.Elapsed.TotalMicroseconds / 100} µs on average");
        Assert.That(result.Success, Is.True);
    }


    [Test]
    public void recursive_parsers_can_be_run_concurrently()
    {
        var parser = JsonParser.Json;

        var t1 = new Thread(() =>
        {
            for (int i = 0; i < 99; i++)
            {
                var result = parser.ParsePartialString(valid_sample);
                Assert.That(result.Success, Is.True);
                Console.Write('a');
            }
        });
        
        var t2 = new Thread(() =>
        {
            for (int i = 0; i < 99; i++)
            {
                var result = parser.ParsePartialString(valid_sample);
                Assert.That(result.Success, Is.True);
                Console.Write('b');
            }
        });

        t1.Start();
        t2.Start();
        t1.Join();
        t2.Join();
    }

    [Test]
    public void json_parsing()
    {
        Console.WriteLine("\r\n=================================================================================");
        var phantomTime = new Stopwatch();
        phantomTime.Start();
        var parser = JsonParser.Json;
        phantomTime.Stop();
        Console.WriteLine($"Creating parser took {phantomTime.Elapsed.TotalMicroseconds} µs");
        
        phantomTime.Restart();
        var result = parser.ParsePartialString(valid_sample);
        phantomTime.Stop();
        Console.WriteLine($"Parsing took {phantomTime.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine($"Total matches = {result.DepthFirstWalk().Count()}");
        
        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Console.WriteLine("\r\n=================================================================================");

        phantomTime.Start();
        var scopes = ScopeNode.FromMatch(result);
        phantomTime.Stop();

        PrintRecursive(scopes, 0);
        
        Console.WriteLine("\r\n=================================================================================");

        // "Deserialise" the scope tree, test it against a proper JSON library.
        var dict = new Dictionary<string, object>();
        phantomTime.Start();
        FillObject(scopes, dict);
        phantomTime.Stop();
        Console.WriteLine(Json.Beautify(Json.Freeze(dict)));

        Console.WriteLine("\r\n=================================================================================");

  
        var sjTime = new Stopwatch();
        sjTime.Start();
        Json.Parse(valid_sample); // this has a more special purpose parser
        sjTime.Stop();
        Console.WriteLine($"Gool deserialising took {phantomTime.Elapsed.TotalMicroseconds} µs");
        Console.WriteLine($"Real serialiser took {sjTime.Elapsed.TotalMicroseconds} µs");
        

        Assert.That(result.Success, Is.True);
    }

    /// <summary>
    /// Returns true if next item should be skipped
    /// </summary>
    private static bool FillObject(ScopeNode node, object target)
    {
        switch (node.NodeType)
        {
            case ScopeNodeType.Root:
            {
                if (node.OpeningMatch is not null || node.ClosingMatch is not null) throw new Exception("Unbalanced tree!");
                if (node.Children.Count != 1) throw new Exception("Tree is empty");
                var firstChild = node.Children[0];

                if (firstChild.NodeType != ScopeNodeType.ScopeChange
                    || firstChild.OpeningMatch?.Tag != "object") throw new Exception("This only accepts an object at the root level");

                var skip = false;
                foreach (var childNode in firstChild.Children)
                {
                    if (skip) { skip = false; continue; }

                    skip = FillObject(childNode, target);
                }

                break;
            }

            case ScopeNodeType.Data:
                switch (target)
                {
                    case Dictionary<string, object> dict:
                    {
                        if (node.DataMatch?.Tag != "key") throw new Exception($"Expected a dictionary key. Got '{node.DataMatch?.Tag ?? "<null>"}'");

                        var key = GetPrimitiveValue(node).ToString()!;
                        if (node.NextNode is null) throw new Exception($"Key '{key}' has no value");
                            
                        var value = GetNodeValue(node.NextNode);
                        if (!dict.TryAdd(key, value)) throw new Exception($"Key '{key}' is duplicated");
                        return true;
                    }

                    case List<object> list:
                    {
                        if (node.DataMatch?.Tag != "value") throw new Exception($"Expected 'value' for array, got '{node.DataMatch?.Tag ?? "<null>"}'");
                        var value = GetNodeValue(node);
                        list.Add(value);
                        break;
                    }

                    default: throw new Exception($"Unexpected container: {target.GetType().Name}");
                }

                break;
            case ScopeNodeType.ScopeChange:
            {
                switch (target)
                {
                    case List<object> list:
                        list.Add(GetNodeValue(node));
                        break;
                    
                    default: throw new Exception($"Expected to write to array, got {target.GetType().Name}");
                }


                break;
            }

            default:
                Assert.Fail($"Node does not have a valid type: {node}");
                break;
        }

        return false;
    }

    private static object GetNodeValue(ScopeNode node)
    {
        object value;
        switch (node.NodeType)
        {
            case ScopeNodeType.Data: // should be a value
            {
                if (node.DataMatch?.Tag != "value") throw new Exception($"Expected a value to go with key, got {node.DataMatch?.Tag ?? "<null>"}");
                
                value = GetPrimitiveValue(node);
                break;
            }

            case ScopeNodeType.ScopeChange: // should be object or array
            {
                switch (node.OpeningMatch?.Tag)
                {
                    case "object":
                        value = new Dictionary<string, object>();
                        break;

                    case "array":
                        value = new List<object>();
                        break;

                    default: throw new Exception($"Expected 'object' or 'array', got '{node.OpeningMatch?.Tag ?? "<null>"}'");
                }

                // recurse down to get the value
                var skip = false;
                foreach (var childNode in node.Children)
                {
                    if (skip) { skip = false; continue; }

                    skip = FillObject(childNode, value);
                }

                break;
            }

            default: throw new Exception($"Expected data or scope, got {node.NodeType.ToString()}");
        }

        return value;
    }

    private static object GetPrimitiveValue(ScopeNode node)
    {
        if (node.DataMatch is null) return "";

        var clean = node.DataMatch.Value.Trim();

        if (clean.StartsWith('"')) return clean.Trim('"'); // string

        if (double.TryParse(clean, out var d)) return d;

        if (bool.TryParse(clean, out var b)) return b;

        if (clean == "null") return "<null>"; // cheating for demo
        
        return "unhandled value: "+node.DataMatch.Value.Trim().Trim('"');
    }

    private static void PrintRecursive(ScopeNode node, int indent)
    {
        switch (node.NodeType)
        {
            case ScopeNodeType.Root:
                Console.WriteLine("Document");
                if (node.OpeningMatch is not null || node.ClosingMatch is not null) Console.WriteLine("Unbalanced scopes!");
                break;
            case ScopeNodeType.Data:
                Console.WriteLine($"{I(indent)}{node.DataMatch?.Value} [{node.DataMatch?.Tag}]");
                break;
            case ScopeNodeType.ScopeChange:
                Console.WriteLine($"{I(indent + 1)}{node.OpeningMatch?.Tag} =>");
                break;

            default:
                Assert.Fail($"Node does not have a valid type: {node}");
                break;
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, indent + 2);
        }
    }

    private static string I(int indent) => new(' ', indent * 2);
}