using System.Diagnostics;
using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class EmailTests
{
    [Test]
    [TestCase("example@email.com")]
    [TestCase("example.first.middle.lastname@email.com")]
    [TestCase("example@subdomain.email.com")]
    [TestCase("example+firstname+lastname@email.com")]
    [TestCase("example@234.234.234.234")]
    [TestCase("example@[234.234.234.234]")]
    [TestCase("\"example\"@email.com")]
    [TestCase("0987654321@example.com")]
    [TestCase("example@email-one.com")]
    [TestCase("_______@email.com")]
    [TestCase("example@email.name")]
    [TestCase("example@email.museum")]
    [TestCase("example@email.co.jp")]
    [TestCase("example.firstname-lastname@email.com")]
    [TestCase("extremely.\"odd\\\\unusual\"@example.com")]
    [TestCase("extremely.unusual.\"@\".unusual.com@example.com")]
    [TestCase("very.\"(),:;<>[]\".VERY.\"very@\\\\ \\\"very\".unusual@strange.email.example.com")]
    [TestCase("John Doe <example@email.com>")] // we accept this because we can extract the core address reliably
    public void valid_emails_are_accepted(string email)
    {
        var parser = EmailAddressExample.Parser;
        var sw = new Stopwatch();
        sw.Start();
        var result = parser.ParseEntireString(email);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        Console.WriteLine(result);

        Console.WriteLine(result.FindTag("email")?.Value ?? "<not found>");
        Assert.That(result.Success, Is.True);
    }
    
    [Test]
    [TestCase("")]
    [TestCase("x")]
    [TestCase("plaintextaddress")]
    [TestCase("@#@@##@%^%#$@#$@#.com")]
    [TestCase("@email.com")]
    [TestCase("example.email.com")]
    [TestCase(".example@email.com")]
    [TestCase("example.@email.com")]
    [TestCase("example…example@email.com")]
    [TestCase("おえあいう@example.com")]
    [TestCase("example@email.com (John Doe)")]
    [TestCase("example@email")]
    [TestCase("example@-email.com")]
    [TestCase("example@email…com")]
    [TestCase("CAT…123@email.com")]
    [TestCase("\"(),:;<>[\\]@email.com")]
    [TestCase("obviously\"not\"correct@email.com")]
    [TestCase("example\\ is\"especially\"not\\allowed@email.com")]
    [TestCase("example@300.234.277.234")]
    [TestCase("example@ridiculous.domain.cooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooom")]
    public void invalid_emails_are_rejected(string email)
    {
        var parser = EmailAddressExample.Parser;
        var sw = new Stopwatch();
        sw.Start();
        var result = parser.ParseEntireString(email);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        Console.WriteLine(result);
        if (result.Success) Console.WriteLine(string.Join("\r\n", result.TaggedTokensDepthFirst().Select(t=>$"{t.Value} [{t.Tag}]")));
        
        Assert.That(result.Success, Is.False);
    }
}