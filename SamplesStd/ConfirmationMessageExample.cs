using Gool;
using static Gool.BNF;

namespace Samples;

/// <summary>
/// Safaricom M-Pesa confirmation SMS, based on examples
/// </summary>
public static class ConfirmationMessageExample
{

    /// <summary>
    /// Safaricom M-Pesa confirmation SMS, based on examples
    /// </summary>
    public static ParserPackage MPesa()
    {
        BNF
            date = IntegerRange(1, 31) > '/' > IntegerRange(1, 12) > '/' > IntegerRange(0, 99),
            time = IntegerRange(0, 24) > ':' > IntegerRange(0, 60) > ((BNF)"AM" | "PM"),

            pattern = StringTerminatedBy(" ").TagWith(TransactionId)
                    > "Confirmed. Ksh"
                    > FractionalDecimal().TagWith(Value)
                    > "sent to eWATERservices Limited for account"
                    > FixedSizeInteger(0, 0xFFFFFFFF, 8, useHex: true).TagWith(TagId)
                    > "on"
                    > date.TagWith(Date)
                    > "at"
                    > time.TagWith(Time)
                    > RestOfInput;

        return pattern.BuildWithOptions(Options.SkipWhitespace);
    }

    public const string TransactionId = "TransId";
    public const string Value         = "Value";
    public const string TagId         = "TagId";
    public const string Date          = "Date";
    public const string Time          = "Time";
}