using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class ConfirmationMessageTests
{
    private const string Sample1 = "UARE44Z6K4 Confirmed. Ksh300.00 sent to eWATERservices Limited for account 679B9B3E on 27/1/26 at 5:12 PM New M-PESA balance is Ksh0.00. Transaction cost, Ksh5.00.Amount you can transact within the day is 489,700.00. Save frequent paybills for quick payment on M-PESA app https://bit.ly/mpesalnk";
    private const string Sample2 = "UAR3C55WSO Confirmed. Ksh30.00 sent to eWATERservices Limited for account 13eadba4  on 27/1/26 at 4:50 PM New M-PESA balance is Ksh513.58. Transaction cost, Ksh0.00.Amount you can transact within the day is 497,622.00. Save frequent paybills for quick payment on M-PESA app https://bit.ly/mpesalnk";
    private const string Sample3 = "UAR3C55Y15 Confirmed. Ksh200.00 sent to eWATERservices Limited for account 13eadba4  on 27/1/26 at 4:45 PM New M-PESA balance is Ksh543.58. Transaction cost, Ksh5.00.Amount you can transact within the day is 497,652.00. Save frequent paybills for quick payment on M-PESA app https://bit.ly/mpesalnk";

    [Test]
    public void basic_message_test_1()
    {
        var result = ConfirmationMessageExample.MPesa().ParseEntireString(Sample1);

        if (!result.Success)
        {
            foreach (var fail in result.Scanner.ListFailures())
            {
                Console.WriteLine(fail);
            }
        }

        Assert.That(result.Success, Is.True);

        Assert.That(result.GetTag(ConfirmationMessageExample.TransactionId)?.Value, Is.EqualTo("UARE44Z6K4"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Value)?.Value, Is.EqualTo("300.00"));
        Assert.That(result.GetTag(ConfirmationMessageExample.TagId)?.Value, Is.EqualTo("679B9B3E"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Date)?.Value, Is.EqualTo("27/1/26"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Time)?.Value, Is.EqualTo("5:12 PM"));

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            Console.Write($"({token.Tag}: {token.Value}); ");
        }
    }
    [Test]
    public void basic_message_test_2()
    {
        var result = ConfirmationMessageExample.MPesa().ParseEntireString(Sample2);

        if (!result.Success)
        {
            foreach (var fail in result.Scanner.ListFailures())
            {
                Console.WriteLine(fail);
            }
        }

        Assert.That(result.Success, Is.True);

        Assert.That(result.GetTag(ConfirmationMessageExample.TransactionId)?.Value, Is.EqualTo("UAR3C55WSO"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Value)?.Value, Is.EqualTo("30.00"));
        Assert.That(result.GetTag(ConfirmationMessageExample.TagId)?.Value, Is.EqualTo("13eadba4"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Date)?.Value, Is.EqualTo("27/1/26"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Time)?.Value, Is.EqualTo("4:50 PM"));

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            Console.Write($"({token.Tag}: {token.Value}); ");
        }
    }
    [Test]
    public void basic_message_test_3()
    {
        var result = ConfirmationMessageExample.MPesa().ParseEntireString(Sample3);

        if (!result.Success)
        {
            foreach (var fail in result.Scanner.ListFailures())
            {
                Console.WriteLine(fail);
            }
        }

        Assert.That(result.Success, Is.True);

        Assert.That(result.GetTag(ConfirmationMessageExample.TransactionId)?.Value, Is.EqualTo("UAR3C55Y15"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Value)?.Value, Is.EqualTo("200.00"));
        Assert.That(result.GetTag(ConfirmationMessageExample.TagId)?.Value, Is.EqualTo("13eadba4"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Date)?.Value, Is.EqualTo("27/1/26"));
        Assert.That(result.GetTag(ConfirmationMessageExample.Time)?.Value, Is.EqualTo("4:45 PM"));

        var tokens = result.TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            Console.Write($"({token.Tag}: {token.Value}); ");
        }
    }
}