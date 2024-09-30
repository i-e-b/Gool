using System;
using System.Collections.Generic;
using System.Globalization;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parse a fixed width unsigned integer, within a given value range.
/// </summary>
public class FixedWidthIntegerRange : Parser
{
    private readonly long _lower;
    private readonly long _upper;
    private readonly int _fixedWidth;
    private readonly bool _useHex;
    private readonly NumberStyles _style;

    /// <summary>
    /// Create a parser for a fixed width unsigned integer, within a given value range.
    /// </summary>
    /// <param name="lower">Inclusive minimum value for result. Must be zero or greater</param>
    /// <param name="upper">Inclusive maximum value for result. Must be greater than lower</param>
    /// <param name="fixedWidth">Number of characters to read from input</param>
    /// <param name="allowLeadingWhitespace">
    /// If <c>true</c> the input may have leading whitespace to fill the fixed width.
    /// If <c>false</c> the input must have digits in all places.
    /// </param>
    /// <param name="useHex">
    /// If <c>true</c> the input may have 0-9 and A-F/a-f; Number will be checked against range as a hexadecimal value.
    /// If <c>false</c> the input may have 0-9 only; Number will be checked against range as a decimal value.
    /// </param>
    public FixedWidthIntegerRange(long lower, long upper, int fixedWidth, bool allowLeadingWhitespace, bool useHex)
    {
        if (lower < 0) throw new ArgumentOutOfRangeException(nameof(lower));
        if (lower >= upper) throw new ArgumentOutOfRangeException(nameof(upper));
        if (fixedWidth < 1) throw new ArgumentOutOfRangeException(nameof(fixedWidth));
        
        _lower = lower;
        _upper = upper;
        _fixedWidth = fixedWidth;
        _useHex = useHex;

        _style = NumberStyles.None;
        if (useHex) _style |= NumberStyles.AllowHexSpecifier;
        if (allowLeadingWhitespace) _style |= NumberStyles.AllowLeadingWhite;
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override bool IsOptional() => false;

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Right ?? 0;

        var compare = scan.Substring(offset, _fixedWidth);
        if (compare.Length != _fixedWidth) return scan.NoMatch(this, previousMatch);

        if (long.TryParse(compare, _style, null, out var result))
        {
            if (result <= _upper && result >= _lower) return scan.CreateMatch(this, offset, compare.Length);
        }

        return scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = $"[f{(_useHex?"H":"D")}:{_lower.ToString("D"+_fixedWidth)}..{_upper.ToString("D"+_fixedWidth)}]";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
    
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}