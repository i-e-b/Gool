using Gool;
using static Gool.BNF;
// ReSharper disable RedundantRangeBound

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
            date = Integer(1..31) > '/' > (1..12) > '/' > (0..99),
            time = Integer(0..24) > ':' > (0..60) > ((BNF)"AM" | "PM"),

            pattern = StringTerminatedBy(" ")[TransactionId]
                    > "Confirmed. Ksh"
                    > FractionalDecimal()[Value]
                    > "sent to eWATERservices Limited for account"
                    > FixedSizeInteger(0, 0xFFFFFFFF, 8, useHex: true).TagWith(TagId)
                    > "on"
                    > date[Date]
                    > "at"
                    > time[Time]
                    > RestOfInput;

        return pattern.BuildWithOptions(Options.SkipWhitespace);
    }

    public const string TransactionId = "TransId";
    public const string Value         = "Value";
    public const string TagId         = "TagId";
    public const string Date          = "Date";
    public const string Time          = "Time";
}