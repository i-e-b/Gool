﻿using System.Collections.Generic;
using Gool.Parsers.Interfaces;
using Gool.Results;
using Gool.Scanners;

namespace Gool.Parsers.Composite;

/// <summary>
/// If the <c>capture</c> parser matches, test the <b>match text</b> against a further
/// set of patterns. The final result is only successful if <b>all</b> the given
/// patterns match the original result.
/// </summary>
public class ParallelSet : Parser, IMatchingParser
{
    private readonly IParser _capture;
    private readonly List<IParser> _parsers = new();
	
    /// <summary>
    /// If the <c>capture</c> parser matches, test the <b>match text</b> against a further
    /// set of patterns. The final result is only successful if <b>all</b> the given
    /// patterns match the original result.
    /// </summary>
    public ParallelSet(IParser capture, IEnumerable<IParser> set)
    {
        _capture = capture;
        _parsers.AddRange(set);
    }

    /// <inheritdoc />
    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        // The capture parser must match
        var baseMatch = _capture.Parse(scan, previousMatch);
        if (!baseMatch.Success) return baseMatch;
        
        // Every validation parser must match the result of the capture parser
        var capture = new ScanStrings(baseMatch.Value);
        foreach (var parser in _parsers)
        {
            var result = parser.Parse(capture);
            if (!result.Success) return scan.NoMatch(this, previousMatch);
        }

        return baseMatch;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        var desc = $"[\u220f {_capture} / {_parsers.Count}]";
			
        if (Tag is null) return desc;
        return desc + " Tag='" + Tag + "'";
    }
	
    /// <inheritdoc />
    public override string ShortDescription(int depth)
    {
        if (depth < 1) return GetType().Name;
        return $"[\u220f {_capture.ShortDescription(depth-1)} / {_parsers.Count}]";
    }
}