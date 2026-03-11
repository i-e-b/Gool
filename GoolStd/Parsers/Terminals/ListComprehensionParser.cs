using System;
using System.Collections.Generic;
using System.Linq;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Terminals;

/// <summary>
/// Parser that matches the longest option from a set of characters and strings
/// </summary>
internal class ListComprehensionParser : Parser
{
    private List<string>? _strings;
    private List<char>?   _chars;

    // Range of the first character, as an optimisation.
    private char          _lowest = char.MaxValue;
    private char          _highest = char.MinValue;

    /// <inheritdoc />
    internal override ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
    {
        var offset = previousMatch?.Right ?? 0;

        var c = scan.Peek(offset);
        if (c == 0 || c < _lowest || c > _highest) return scan.NoMatch(this, previousMatch); // can't be in any of the ranges

        if (_strings is not null)
        {
            int longestMatch = 0;
            foreach (var pattern in _strings)
            {
                if (pattern.Length < longestMatch) continue;
                var compare = scan.Substring(offset, pattern.Length);
                if (compare.Equals(pattern, StringComparison.Ordinal)) longestMatch = pattern.Length;
            }

            if (longestMatch > 0) return scan.CreateMatch(this, offset, longestMatch, previousMatch);
        }

        if (_chars?.Contains(c) != true) return scan.NoMatch(this, previousMatch);

        // if we arrive at this point, we have a match
        return scan.CreateMatch(this, offset, 1, previousMatch);
    }

    /// <inheritdoc />
    public override IEnumerable<IParser> ChildParsers() { yield break; }

    /// <inheritdoc />
    public override string ToString()
    {
        if (_chars is not null)
        {
            var desc = "[" + string.Join("", _chars.Select(c => c.ToString())) + "]";

            if (Tag is null) return desc;
            return desc + " Tag=‘" + Tag + "’";
        }

        if (_strings is not null)
        {
            var desc = "['" + string.Join("', '", _strings.Select(c => c.ToString())) + "']";

            if (Tag is null) return desc;
            return desc + " Tag=‘" + Tag + "’";
        }

        return "<invalid>";
    }

    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        return ToString();
    }

    /// <summary>
    /// Add a character to the search set.
    /// </summary>
    public void Add(char c)
    {
        _chars ??= new List<char>();
        if (_chars.Contains(c)) return;

        if (c < _lowest) _lowest = c;
        if (c > _highest) _highest = c;
        _chars.Add(c);
    }

    public void Add(string s)
    {
        _strings ??= new List<string>();
        if (_strings.Contains(s)) return;
        if (string.IsNullOrEmpty(s)) throw new Exception("Invalid pattern string (empty)");

        var c = s[0];
        if (c < _lowest) _lowest = c;
        if (c > _highest) _highest = c;

        _strings.Add(s);
    }
}