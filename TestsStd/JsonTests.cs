using System.Diagnostics;
using NUnit.Framework;
using Phantom.Scanners;
using Samples;
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
        "index": [1,2,3]
      }
    }}
    """;

  [Test]
  public void json_parsing()
  {
    Console.WriteLine("\r\n=================================================================================");
    var parser = new JsonParser().TheParser;
    var scanner = new ScanStrings(valid_sample) { SkipWhitespace = false};

    var sw = new Stopwatch();
    sw.Start();
    var result = parser.Parse(scanner);
    sw.Stop();
    Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

    Console.WriteLine($"Total matches = {result.DepthFirstWalk().Count()}");
    
    foreach (var match in result.TaggedTokens())
    {
      Console.WriteLine($"{match.Value} [{match.Tag}]");
    }

    Console.WriteLine("\r\n=================================================================================");

    
    foreach (var match in result.DepthFirstWalk())
    {
      Console.WriteLine($"{match.Value} [{match.Tag}]");
    }

    Console.WriteLine("\r\n=================================================================================");
    
    foreach (var fail in scanner.ListFailures())
    {
      Console.WriteLine(fail);
    }

    Assert.That(result.Success, Is.True);
  }

}