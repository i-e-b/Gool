using System;
using Gool.Results;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parse a variable width signed fractional decimal number.
/// This does <b>not</b> try to fit the result in a float representation.
/// </summary>
public class VariableWidthFractionalDecimal : Parser
{
    private readonly bool   _allowLeadingWhitespace;
    private readonly string _groupMark;
    private readonly string _decimalMark;

    /// <summary>
    /// Create a number parser
    /// </summary>
    public VariableWidthFractionalDecimal(bool allowLeadingWhitespace, string groupMark, string decimalMark)
    {
        _allowLeadingWhitespace = allowLeadingWhitespace;
        _groupMark = groupMark;
        _decimalMark = decimalMark;
    }

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        // This could be replaced with a simpler set-up using composite parsers...
        /*
		BNF // Number literals
			neg      = '-',
			digit    = AnyCharacterInRanges(('0', '9')),
			exp      = OneOf('e', 'E'),
			sign     = OneOf('+', '-'),
			digits   = +digit,
			exponent = !(exp > sign > digits),
			fraction = !('.' > digits),
			integer  = (!neg) > (+digit), // this is slightly out of spec, as it allows "01234"
			number   = integer > fraction > exponent;

		return number;*/
        // ... but this custom parser is marginally faster.

        var start  = previousMatch?.Right ?? 0;
        var offset = previousMatch?.Right ?? 0;
        var result = scan.EmptyMatch(this, start);

        // Skip leading whitespace
        if (_allowLeadingWhitespace)
        {
            while (!scan.EndOfInput(offset))
            {
                var isWhiteSpace = char.IsWhiteSpace(scan.Peek(offset));
                if (!isWhiteSpace) break; // no more whitespace

                offset++;
                result.ExtendTo(offset);
            }
        }

        // Optional '+' or '-'
        if (scan.Peek(offset) is '+' or '-')
        {
            offset++;
            result.ExtendTo(offset);
        }

        // Must have at least one digit
        if (scan.Peek(offset) is >= '0' and <= '9')
        {
            offset++;
            result.ExtendTo(offset);
        }
        else
        {
            return scan.NoMatch(this, previousMatch);
        }

        // Read as many digits as we can, skipping group marks.
        // Allow only one decimal, switch if we find 'e' or 'E'.
        var gStart          = _groupMark.Length > 0 ? _groupMark[0] : (char)0;
        var dStart          = _decimalMark[0];
        var lastWasNumber   = true;
        var seenDecimalMark = false;
        var needExponent = false;
        while (!scan.EndOfInput(offset))
        {
            var c = scan.Peek(offset);
            if (c is >= '0' and <= '9')
            {
                lastWasNumber = true;
                offset++;
                result.ExtendTo(offset);
            }
            else if (c == gStart)
            {
                // Check for the group string
                if (_groupMark.Length < 1) break; // saw a 'NUL' character?
                var compare = scan.Substring(offset, _groupMark.Length);
                if (compare.Equals(_groupMark, StringComparison.Ordinal))
                {
                    offset += _groupMark.Length;
                    lastWasNumber = false;
                    // don't extend the result until we see more numbers
                }
                else break;
            }
            else if (c == dStart)
            {
                // Check for decimal string
                var compare = scan.Substring(offset, _decimalMark.Length);
                if (compare.Equals(_decimalMark, StringComparison.Ordinal))
                {
                    if (seenDecimalMark) return scan.NoMatch(this, previousMatch); // more than one decimal marker
                    seenDecimalMark = true;
                    offset += _decimalMark.Length;
                    lastWasNumber = false;
                    // don't extend the result until we see more numbers
                }
                else break;
            }
            else if (c is 'e' or 'E')
            {
                offset++;
                needExponent = true;
                lastWasNumber = false;
                break;
            }
            else break; // end of the number?
        }

        if (!needExponent) return lastWasNumber ? result : scan.NoMatch(this, previousMatch);

        // Check for exponent
        // Optional '+' or '-'
        if (scan.Peek(offset) is '+' or '-')
        {
            offset++;
            result.ExtendTo(offset);
        }

        // Must have at least one digit
        if (scan.Peek(offset) is >= '0' and <= '9')
        {
            offset++;
            result.ExtendTo(offset);
            lastWasNumber = true;
        }
        else
        {
            return scan.NoMatch(this, previousMatch);
        }

        while (!scan.EndOfInput(offset))
        {
            var c = scan.Peek(offset);
            if (c is >= '0' and <= '9')
            {
                lastWasNumber = true;
                offset++;
                result.ExtendTo(offset);
            }
            else if (c == gStart)
            {
                // Check for the group string
                var compare = scan.Substring(offset, _groupMark.Length);
                if (compare.Equals(_groupMark, StringComparison.Ordinal))
                {
                    offset += _groupMark.Length;
                    // don't extend the result until we see more numbers
                }
                else break;
            }
            else if (c == dStart)
            {
                // Check for decimal string
                var compare = scan.Substring(offset, _decimalMark.Length);
                if (compare.Equals(_decimalMark, StringComparison.Ordinal))
                {
                    return scan.NoMatch(this, previousMatch); // decimal exponents not allowed
                }

                break;
            }
            else break;
        }

        // Must not end in a decimal or group mark.
        return lastWasNumber ? result : scan.NoMatch(this, previousMatch);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = "{decimal}";

        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }
}