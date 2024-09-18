using System.Diagnostics;
using Gool.Results;
using NUnit.Framework;
using Samples;
using SkinnyJson;

namespace TestsStd;

[TestFixture]
public class SqlConnectionStringTests
{
    [Test] // Examples from https://www.connectionstrings.com/
    [TestCase("","{}")]
    [TestCase(";","{}")]
    [TestCase(
        "Server=myServerAddress;Database=myDataBase;",
        "{\"Server\":\"myServerAddress\",\"Database\":\"myDataBase\"}")]
    [TestCase(
        "Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;",
        "{\"Server\":\"myServerAddress\",\"Database\":\"myDataBase\",\"User Id\":\"myUsername\",\"Password\":\"myPassword\"}")]
    [TestCase(
        "Server=myServerAddress;Database=myDataBase;Trusted_Connection=True;",
        "{\"Server\":\"myServerAddress\",\"Database\":\"myDataBase\",\"Trusted_Connection\":\"True\"}")]
    [TestCase(
        "Server=myServerName\\myInstanceName;Database=myDataBase;User Id=myUsername;Password=myPassword;",
        "{\"Server\":\"myServerName\\\\myInstanceName\",\"Database\":\"myDataBase\",\"User Id\":\"myUsername\",\"Password\":\"myPassword\"}")]
    [TestCase(
        "User ID=root;Password=myPassword;Host=localhost;Port=5432;Database=myDataBase;Pooling=true;Min Pool Size=0;Max Pool Size=100;Connection Lifetime=0;",
        "{\"User ID\":\"root\",\"Password\":\"myPassword\",\"Host\":\"localhost\",\"Port\":\"5432\",\"Database\":\"myDataBase\",\"Pooling\":\"true\",\"Min Pool Size\":\"0\",\"Max Pool Size\":\"100\",\"Connection Lifetime\":\"0\"}")]
    [TestCase(
        "Server=tcp:myserver.database.windows.net,1433;Database=myDataBase;User ID=mylogin@myserver;Password=myPassword;Trusted_Connection=False;Encrypt=True;",
        "{\"Server\":\"tcp:myserver.database.windows.net,1433\",\"Database\":\"myDataBase\",\"User ID\":\"mylogin@myserver\",\"Password\":\"myPassword\",\"Trusted_Connection\":\"False\",\"Encrypt\":\"True\"}")]
    [TestCase(
        "User ID=root;Password=myPassword;Host=localhost;Port=3306;Database=myDataBase;Direct=true;Protocol=TCP;Compress=false;Pooling=true;Min Pool Size=0;Max Pool Size=100;Connection Lifetime=0;",
        "{\"User ID\":\"root\",\"Password\":\"myPassword\",\"Host\":\"localhost\",\"Port\":\"3306\",\"Database\":\"myDataBase\",\"Direct\":\"true\",\"Protocol\":\"TCP\",\"Compress\":\"false\",\"Pooling\":\"true\",\"Min Pool Size\":\"0\",\"Max Pool Size\":\"100\",\"Connection Lifetime\":\"0\"}")]
    public void basic_connection_strings(string input, string jsonExpected)
    {
        var parser = SqlConnectionStringExample.Connection();
        var sw     = new Stopwatch();
        sw.Start();
        var result = parser.ParseEntireString(input);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        // Convert to a dictionary
        sw.Restart();
        var settings = new Dictionary<string, string>();
        var scopes   = ScopeNode.FromMatch(result);
        scopes.DepthFirstVisitTags("Key",
            node => settings[node.Value] = string.Join(", ", node.Children.Select(c => c.Value))
        );
        sw.Stop();
        Console.WriteLine($"Conversion took {sw.Elapsed.TotalMicroseconds} µs");

        var actual = Json.Freeze(settings);
        Console.WriteLine(actual);

        Assert.That(result.Success, Is.True);
        Assert.That(actual, Is.EqualTo(jsonExpected));
    }

    [Test]
    [TestCase(
        "Driver={PostgreSQL ANSI};Server=IP address;Port=5432;Database=myDataBase;Uid=myUsername;Pwd=myPassword;sslmode=require;",
        "{\"Driver\":\"PostgreSQL ANSI\",\"Server\":\"IP address\",\"Port\":\"5432\",\"Database\":\"myDataBase\",\"Uid\":\"myUsername\",\"Pwd\":\"myPassword\",\"sslmode\":\"require\"}")]
    [TestCase(
        "Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=\"password'with;single'quotes;\";",
        "{\"Server\":\"myServerAddress\",\"Database\":\"myDataBase\",\"User Id\":\"myUsername\",\"Password\":\"password'with;single'quotes;\"}")]
    [TestCase(
        "Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=';password\"with;double\"quotes';",
        "{\"Server\":\"myServerAddress\",\"Database\":\"myDataBase\",\"User Id\":\"myUsername\",\"Password\":\";password\\\"with;double\\\"quotes\"}")]
    public void quoted_connection_strings(string input, string jsonExpected)
    {
        var parser = SqlConnectionStringExample.Connection();
        var sw     = new Stopwatch();
        sw.Start();
        var result = parser.ParseEntireString(input);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        // Convert to a dictionary
        sw.Restart();
        var settings = new Dictionary<string, string>();
        var scopes   = ScopeNode.FromMatch(result);
        scopes.DepthFirstVisitTags("Key",
            node => settings[node.Value] = string.Join(", ", node.Children.Select(c => c.Value))
        );
        sw.Stop();
        Console.WriteLine($"Conversion took {sw.Elapsed.TotalMicroseconds} µs");

        var actual = Json.Freeze(settings);
        Console.WriteLine(actual);

        Assert.That(result.Success, Is.True);
        Assert.That(actual, Is.EqualTo(jsonExpected));
    }
}