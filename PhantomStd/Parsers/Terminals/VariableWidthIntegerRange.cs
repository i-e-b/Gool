using System;
using System.Globalization;
using System.Text;
using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Terminals;

/// <summary>
/// Parse a variable width unsigned integer, within a given value range.
/// </summary>
public class VariableWidthIntegerRange : Parser, IMatchingParser
{
    private readonly long _lower;
    private readonly long _upper;
    private readonly bool _useHex;
    private readonly NumberStyles _style;

    /// <summary>
    /// Create a parser for a variable width unsigned integer, within a given value range.
    /// </summary>
    /// <param name="lower">Inclusive minimum value for result. Must be zero or greater</param>
    /// <param name="upper">Inclusive maximum value for result. Must be greater than lower</param>
    /// <param name="allowLeadingWhitespace">
    /// If <c>true</c> the input may have leading whitespace to fill the fixed width.
    /// If <c>false</c> the input must have digits in all places.
    /// </param>
    /// <param name="useHex">
    /// If <c>true</c> the input may have 0-9 and A-F/a-f; Number will be checked against range as a hexadecimal value.
    /// If <c>false</c> the input may have 0-9 only; Number will be checked against range as a decimal value.
    /// </param>
    public VariableWidthIntegerRange(long lower, long upper, bool allowLeadingWhitespace, bool useHex)
    {
        if (lower < 0) throw new ArgumentOutOfRangeException(nameof(lower));
        if (lower >= upper) throw new ArgumentOutOfRangeException(nameof(upper));
        
        _lower = lower;
        _upper = upper;
        _useHex = useHex;

        _style = NumberStyles.None;
        if (useHex) _style |= NumberStyles.AllowHexSpecifier;
        if (allowLeadingWhitespace) _style |= NumberStyles.AllowLeadingWhite;
    }

    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var start = previousMatch?.Right ?? 0;
        var offset = previousMatch?.Right ?? 0;

        // Read as many digits as we can
        var src = new StringBuilder();
        while (IsValidDigit(scan.Peek(offset)))
        {
            src.Append(scan.Peek(offset));
            if (!scan.Read(ref offset)) break;
        }

        // try to parse the result
        var compare = src.ToString();
        if (long.TryParse(compare, _style, null, out var result))
        {
            if (result <= _upper && result >= _lower) return scan.CreateMatch(this, start, compare.Length);
        }

        return scan.NoMatch(this, previousMatch);
    }

    private bool IsValidDigit(char c)
    {
        if (!_useHex) return c is >= '0' and <= '9';
        
        if (c is >= '0' and <= '9') return true;
        if (c is >= 'a' and <= 'f') return true;
        if (c is >= 'A' and <= 'F') return true;
        return false;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = $"[v{(_useHex?"H":"D")}:{_lower}..{_upper}]";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}